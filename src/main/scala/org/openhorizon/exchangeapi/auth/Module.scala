package org.openhorizon.exchangeapi.auth

import org.openhorizon.exchangeapi.utility.ExchMsg
import org.openhorizon.exchangeapi.ExchangeApi

import javax.security.auth._
import javax.security.auth.callback._
import javax.security.auth.login.FailedLoginException
import javax.security.auth.spi.LoginModule
import scala.util._
import scala.util.matching.Regex

/**
 * JAAS module to authenticate local user/pw, nodeid/token, and agbotid/token in the exchange.
 * Called from AuthenticationSupport:authenticate() because JAAS.config references this module.
 */
class Module extends LoginModule with AuthorizationSupport {
  private var subject: Subject = _
  private var handler: CallbackHandler = _
  private var identity: Identity = _
  private var succeeded = false
  def logger = ExchangeApi.defaultLogger

  override def initialize(
    subject: Subject,
    handler: CallbackHandler,
    sharedState: java.util.Map[String, _],
    options: java.util.Map[String, _]): Unit = {
    this.subject = subject
    this.handler = handler
  }

  /*
   * This is where the actual login logic is performed, and is called by the
   * LoginContext when its login method is called. This uses the callback to
   * get acces to the web request, and then uses the logic from the credsAndLog
   * to get an Identity from the request. This is later attached to the subject
   * in the commit method, which is called by the context after login succeeds,
   * and that is where we can get access to it in the route handling code.
   */
  override def login(): Boolean = {
    //logger.debug("in Module.login() to try to authenticate a local exchange user")
    val reqCallback = new RequestCallback
    
    val loginResult =
      Try {
        handler.handle(Array(reqCallback))
        
        if (reqCallback.request.isEmpty) {
          logger.debug("Unable to get HTTP request while authenticating")
          throw new AuthInternalErrorException(ExchMsg.translate("unable.to.get.http.request.when.authenticating"))
        }
        
        val reqInfo: RequestInfo = reqCallback.request.get   // reqInfo is of type RequestInfo
        /*** // ***/ logger.debug(s"auth/Module.login(): reqInfo: $reqInfo") // TODO: comment
        //val clientIp = req.header("X-Forwarded-For").orElse(Option(req.getRemoteAddr)).get // haproxy inserts the real client ip into the header for us
        
        // Split an id in the form org/id and return both parts. If there is no / we assume it is an id without the org.
        def compositeIdSplit(compositeId: String): (String, String) = {
          val reg: Regex = """^(\S*?)/(\S*)$""".r
          compositeId match {
            case reg(org, id) => (org, id)
            // These 2 lines never get run, and aren't needed. If we really want to handle a special, put something like this as the 1st case above: case reg(org, "") => return (org, "")
            //case reg(org, _) => return (org, "")
            //case reg(_, id) => return ("", id)
            case _ => ("", compositeId)
          }
        }
        
        // Get the creds from the request
        val (org, id) = compositeIdSplit(reqInfo.creds.id)
        
        if (org == "")
          throw new OrgNotSpecifiedException
        
        if (id == "iamapikey" || id == "iamtoken")
          throw new NotLocalCredsException
        
        //logger.info("User or id " + userOrId + " from " + clientIp + " running " + req.getMethod + " " + req.getPathInfo)
        if (reqInfo.isDbMigration && !Role.isSuperUser(reqInfo.creds.id))
          throw new IsDbMigrationException()
          
        identity = IIdentity(reqInfo.creds, Identity2(None, "", None, "", "")).authenticate(reqInfo.hint) // authenticate() is in AuthorizationSupport and both authenticates this identity and returns the correct IIdentity subclass (IUser, Inode, or IAgbot)
        //}
        true
      }
    
    //logger.debug("Module.login(): loginResult=" + loginResult)
    succeeded = loginResult.isSuccess
    
    if (!succeeded) {
      // Throw an exception so we can report the correct error
      loginResult.failed.get match {
        case _: NotLocalCredsException => return false
        case e: AuthException => throw e
        case _ => throw new FailedLoginException
      }
    }
    
    succeeded
  }

  override def logout(): Boolean = {
    subject.getPrivateCredentials().add(identity)
    true
  }

  override def abort() = false

  override def commit(): Boolean = {
    if (succeeded) {
      subject.getPrivateCredentials().add(identity)
      //subject.getPrincipals().add(ExchangeRole(identity.role)) // don't think we need this
    }
    succeeded
  }
}

/*
 * Login modules get info about the subject (like username and password)
 * through callbacks. JAAS doesn't really seem to have been meant for web
 * apps (a lot of the callbacks seem to be meant to interact with the user,
 * e.g., prompt them to type in a password), and after some research I
 * found some people were implementing it in web apps by creating callbacks
 * that return the http request object, so I went with that. It's possible
 * that we will want to support more callbacks like Name and Password, which
 * we can pull from the request in the callback handler, but grabbing the
 * request like this made it easy to re-use the existing logic.
 */
class ExchCallbackHandler(request: RequestInfo) extends CallbackHandler {
  override def handle(callbacks: Array[Callback]): Unit = {
    for (callback <- callbacks) {
      callback match {
        case cb: RequestCallback => cb.request = request
        case _ =>
      }
    }
  }
}

class RequestCallback extends Callback {
  private var req: Option[RequestInfo] = None

  def request_=(request: RequestInfo): Unit = {
    req = Some(request)
  }

  def request: Option[RequestInfo] = req
}

/* Everything below here is for JAAS authorization, but not using the java authorization framework anymore, because it doesn't add any value for us and adds complexity
// Both ExchangeRole and AccessPermission are listed in resources/auth.policy
final case class ExchangeRole(role: String) extends Principal {
  override def getName = role
}

final case class AccessPermission(name: String) extends BasicPermission(name)

final case class PermissionCheck(permission: String) extends PrivilegedAction[Unit] {
  import Access._

  // It is easier to list the actions an admin user is *not* allowed to do
  private val adminNotAllowed = Set(
    CREATE_ORGS.toString,
    READ_OTHER_ORGS.toString,
    WRITE_OTHER_ORGS.toString,
    CREATE_IN_OTHER_ORGS.toString,
    SET_IBM_ORG_TYPE.toString,
    ADMIN.toString)

  private def isAdminAllowed(permission: String) = {
    if (adminNotAllowed.contains(permission)) {
      Failure(new Exception(ExchMsg.translate("admins.not.given.permission", permission)))
    } else {
      Success(())
    }
  }

  override def run() = {
    val literalCheck = Try(AccessController.checkPermission(AccessPermission(permission)))
    lazy val adminCheck = for {
      _ <- isAdminAllowed(permission) //note: changed allowed to _ to make the editor happy
      ok <- Try(AccessController.checkPermission(AccessPermission("ALL_IN_ORG")))
    } yield ok
    lazy val superCheck = Try(AccessController.checkPermission(AccessPermission("ALL")))

    for {
      literalFailure <- literalCheck.failed
      _ <- adminCheck.failed
      _ <- superCheck.failed
    } {
      throw literalFailure
    }
  }
}
*/

