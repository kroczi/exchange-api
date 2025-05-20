package org.openhorizon.exchangeapi.route.agreementbot.message

import com.github.pjfanning.pekkohttpjackson.JacksonSupport
import io.swagger.v3.oas.annotations.enums.ParameterIn
import io.swagger.v3.oas.annotations.media.{Content, ExampleObject, Schema}
import io.swagger.v3.oas.annotations.parameters.RequestBody
import io.swagger.v3.oas.annotations.{Operation, Parameter, responses}
import jakarta.ws.rs.{GET, POST, Path}
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.event.LoggingAdapter
import org.apache.pekko.http.scaladsl.model.{StatusCode, StatusCodes}
import org.apache.pekko.http.scaladsl.server.Directives._
import org.apache.pekko.http.scaladsl.server.Route
import org.openhorizon.exchangeapi.auth.{Access, AuthenticationSupport, DBProcessingError, Identity2, OrgAndId, TAgbot}
import org.openhorizon.exchangeapi.route.agreementbot.{GetAgbotMsgsResponse, PostAgbotsMsgsRequest}
import org.openhorizon.exchangeapi.table.agreementbot.message.{AgbotMsg, AgbotMsgRow, AgbotMsgsTQ}
import org.openhorizon.exchangeapi.table.node.NodesTQ
import org.openhorizon.exchangeapi.table.resourcechange
import org.openhorizon.exchangeapi.table.resourcechange.{ResChangeCategory, ResChangeOperation, ResChangeResource}
import org.openhorizon.exchangeapi.utility.{ApiRespType, ApiResponse, ApiTime, Configuration, ExchMsg, ExchangePosgtresErrorHandling, HttpCode}
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}


@Path("/v1/orgs/{organization}/agbots/{agreementbot}/msgs")
trait Messages extends JacksonSupport with AuthenticationSupport {
  // Will pick up these values when it is mixed in with ExchangeApiApp
  def db: Database
  def system: ActorSystem
  def logger: LoggingAdapter
  implicit def executionContext: ExecutionContext
  
  
  // ========== GET /orgs/{organization}/agbots/{agreementbot}/msgs =========================================
  @GET
  @Operation(summary = "Returns all Messages sent to this Agreement Bot (AgBot).",
             description = "They will be returned in the order they were sent. All Messages that have been sent to this AgBot will be returned, unless the AgBot has deleted some, or some are past their TTL. Can be run by a User or the AgBot.",
             parameters =
               Array(new Parameter(name = "organization", in = ParameterIn.PATH, description = "Organization identifier"),
                     new Parameter(name = "agreementbot", in = ParameterIn.PATH, description = "Agreement Bot identifier"),
                     new Parameter(name = "maxmsgs", in = ParameterIn.QUERY, required = false, description = "Maximum number of Messages returned. If this is less than the number of Messages available, the oldest Messages are returned. Defaults to unlimited.")),
             responses =
               Array(new responses.ApiResponse(responseCode = "200", description = "response body",
                                               content =
                                                 Array(new Content(mediaType = "application/json",
                                                                   schema =
                                                                     new Schema(implementation = classOf[GetAgbotMsgsResponse])))),
                     new responses.ApiResponse(responseCode = "400", description = "bad input"),
                     new responses.ApiResponse(responseCode = "401", description = "invalid credentials"),
                     new responses.ApiResponse(responseCode = "403", description = "access denied"),
                     new responses.ApiResponse(responseCode = "404", description = "not found")))
  @io.swagger.v3.oas.annotations.tags.Tag(name = "agreement bot/message")
  def getMessages(@Parameter(hidden = true) agreementBot: String,
                  @Parameter(hidden = true) identity: Identity2,
                  @Parameter(hidden = true) organization: String,
                  @Parameter(hidden = true) resource: String): Route = {
    parameter("maxmsgs".as[Int].?) {
      maxMsgs =>
        logger.debug(s"GET /orgs/${organization}/agbots/${agreementBot}/msgs?maxmsgs=${maxMsgs.getOrElse("None")} - By ${identity.resource}:${identity.role}")
        complete({
          // Set the query, including maxmsgs
          var query = AgbotMsgsTQ.getMsgs(resource).sortBy(_.msgId)
          if (0 < maxMsgs.getOrElse(0))
            query = query.take(maxMsgs.get) // Get the msgs for this agbot
          db.run(query.result)
            .map({
              list =>
                logger.debug("GET /orgs/" + organization + "/agbots/" + agreementBot + "/msgs result size: " + list.size)
                //logger.debug("GET /orgs/"+orgid+"/agbots/"+id+"/msgs result: "+list.toString)
                val msgs: List[AgbotMsg] = list.map(_.toAgbotMsg).toList
                val code: StatusCode =
                  if (msgs.nonEmpty)
                    StatusCodes.OK
                  else
                    StatusCodes.NotFound
                (code, GetAgbotMsgsResponse(msgs, 0))
            })
        })
    }
  }
  
  // ========== POST /orgs/{organization}/agbots/{agreementbot}/msgs ========================================
  @POST
  @Operation(summary = "Sends a Message from a Node to an Agreement Bot (AgBot)",
             description = "The Node must first sign the Message (with its private key) and then encrypt the msg (with the AgBot's public key). Can be run by any Node.",
             parameters =
               Array(new Parameter(name = "organization",
                                   in = ParameterIn.PATH,
                                   description = "Organization identifier"),
                     new Parameter(name = "agreementbot",
                                   in = ParameterIn.PATH,
                                   description = "Agreement Bot identifier")),
             requestBody =
               new RequestBody(content =
                 Array(new Content(examples =
                   Array(new ExampleObject(value = """{
  "message": "VW1RxzeEwTF0U7S96dIzSBQ/hRjyidqNvBzmMoZUW3hpd3hZDvs",
  "ttl": 86400
}
""")),
                                   mediaType = "application/json",
                                   schema =
                                     new Schema(implementation = classOf[PostAgbotsMsgsRequest]))),
                               required = true),
             responses =
               Array(new responses.ApiResponse(responseCode = "201",
                                               description = "response body",
                                               content =
                                                 Array(new Content(mediaType = "application/json",
                                                                   schema = new Schema(implementation = classOf[ApiResponse])))),
                     new responses.ApiResponse(responseCode = "401",
                                               description = "invalid credentials"),
                     new responses.ApiResponse(responseCode = "403",
                                               description = "access denied"),
                     new responses.ApiResponse(responseCode = "404",
                                               description = "not found")))
  @io.swagger.v3.oas.annotations.tags.Tag(name = "agreement bot/message")
  def postMessages(@Parameter(hidden = true) agreementBot: String,
                   @Parameter(hidden = true) identity: Identity2,
                   @Parameter(hidden = true) organization: String,
                   @Parameter(hidden = true) resource: String): Route =
    entity (as[PostAgbotsMsgsRequest]) {
      reqBody =>
        complete({
          val nodeId: String = identity.resource //somday: handle the case where the acls allow users to send msgs
          var msgNum = ""
          val maxMessagesInMailbox: Int = Configuration.getConfig.getInt("api.limits.maxMessagesInMailbox")
          val getNumOwnedDbio =
            if (maxMessagesInMailbox == 0)
              DBIO.successful(0)
            else
              AgbotMsgsTQ.getNumOwned(resource).result // avoid DB read for this if there is no max
          // Remove msgs whose TTL is past, then check the mailbox is not full, then get the node publicKey, then write the agbotmsgs row, all in the same db.run thread
          db.run(getNumOwnedDbio.flatMap({
                                  xs =>
                                    if (maxMessagesInMailbox != 0)
                                      logger.debug("POST /orgs/" + organization + "/agbots/" + agreementBot + "/msgs mailbox size: " + xs)
                                    val mailboxSize: Int = xs
                                    if (maxMessagesInMailbox == 0 ||
                                        mailboxSize < maxMessagesInMailbox)
                                      NodesTQ.getPublicKey(nodeId).result.asTry
                                    else
                                      DBIO.failed(new DBProcessingError(HttpCode.BAD_GW, ApiRespType.BAD_GW, ExchMsg.translate("agbot.mailbox.full", resource, maxMessagesInMailbox))).asTry
                                })
                                .flatMap({
                                  case Success(v) =>
                                    logger.debug("POST /orgs/" + organization + "/agbots/" + agreementBot + "/msgs node publickey result: " + v)
                                    val nodePubKey: String = v.head
                                    if (nodePubKey != "")
                                      AgbotMsgRow(0, resource, nodeId, nodePubKey, reqBody.message, ApiTime.nowUTC, ApiTime.futureUTC(reqBody.ttl)).insert.asTry
                                    else
                                      DBIO.failed(new DBProcessingError(HttpCode.BAD_INPUT, ApiRespType.BAD_INPUT, ExchMsg.translate("agbot.message.invalid.input"))).asTry
                                  case Failure(t) =>
                                    DBIO.failed(t).asTry // rethrow the error to the next step
                                })
                                .flatMap({
                                  case Success(v) => // Add the resource to the resourcechanges table
                                    logger.debug("POST /orgs/{organization}/agbots/" + agreementBot + "/msgs write row result: " + v)
                                    msgNum = v.toString
                                    resourcechange.ResourceChange(0L, organization, agreementBot, ResChangeCategory.AGBOT, public = false, ResChangeResource.AGBOTMSGS, ResChangeOperation.CREATED).insert.asTry
                                  case Failure(t) =>
                                    DBIO.failed(t).asTry}))
            .map({
              case Success(v) =>
                logger.debug("POST /orgs/{organization}/agbots/" + agreementBot + "/msgs updated in changes table: " + v)
                (HttpCode.POST_OK, ApiResponse(ApiRespType.OK, "agbot msg " + msgNum + " inserted"))
              case Failure(t: DBProcessingError) =>
                t.toComplete
              case Failure(t: org.postgresql.util.PSQLException) =>
                if (ExchangePosgtresErrorHandling.isKeyNotFoundError(t))
                  (HttpCode.NOT_FOUND, ApiResponse(ApiRespType.NOT_FOUND, ExchMsg.translate("agbot.message.agbotid.not.found", resource, t.getMessage)))
                else
                  ExchangePosgtresErrorHandling.ioProblemError(t, ExchMsg.translate("agbot.message.not.inserted", resource, t.toString))
              case Failure(t) =>
                (HttpCode.INTERNAL_ERROR, ApiResponse(ApiRespType.INTERNAL_ERROR, ExchMsg.translate("agbot.message.not.inserted", resource, t.toString)))
            })
        })
    }
  
  
  def messagesAgreementBot(identity: Identity2): Route =
    path("orgs" / Segment / "agbots" / Segment / "msgs") {
      (organization,
       agreementBot) =>
        val resource: String = OrgAndId(organization, agreementBot).toString
        
        get {
          exchAuth(TAgbot(resource), Access.READ, validIdentity = identity) {
            _ =>
              getMessages(agreementBot, identity, organization, resource)
          }
        } ~
        post {
          exchAuth(TAgbot(resource), Access.SEND_MSG_TO_AGBOT, validIdentity = identity) {
            _ =>
              postMessages(agreementBot, identity, organization, resource)
          }
        }
    }
  
}
