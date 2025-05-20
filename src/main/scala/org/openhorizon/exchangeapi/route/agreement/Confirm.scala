package org.openhorizon.exchangeapi.route.agreement

import com.github.pjfanning.pekkohttpjackson.JacksonSupport
import io.swagger.v3.oas.annotations.enums.ParameterIn
import io.swagger.v3.oas.annotations.media.{Content, ExampleObject, Schema}
import io.swagger.v3.oas.annotations.parameters.RequestBody
import io.swagger.v3.oas.annotations.{Operation, Parameter, responses}
import jakarta.ws.rs.{POST, Path}
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.event.LoggingAdapter
import org.apache.pekko.http.scaladsl.server.Directives.{complete, path, post, _}
import org.apache.pekko.http.scaladsl.server.Route
import org.openhorizon.exchangeapi.auth.{Access, AuthRoles, AuthenticationSupport, IAgbot, IUser, Identity, Identity2, OrgAndId, TAgbot}
import org.openhorizon.exchangeapi.route.agreementbot.PostAgreementsConfirmRequest
import org.openhorizon.exchangeapi.table.agreementbot.AgbotsTQ
import org.openhorizon.exchangeapi.table.agreementbot.agreement.AgbotAgreementsTQ
import org.openhorizon.exchangeapi.utility.{ApiRespType, ApiResponse, ExchMsg, HttpCode}
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.ExecutionContext

@Path("/v1/orgs/{organization}/agreements/confirm")
@io.swagger.v3.oas.annotations.tags.Tag(name = "organization")
trait Confirm extends JacksonSupport with AuthenticationSupport {
  def db: Database
  def system: ActorSystem
  def logger: LoggingAdapter
  implicit def executionContext: ExecutionContext
  
  // =========== POST /orgs/{organization}/agreements/confirm ===============================
  @POST
  @Operation(
    summary = "Confirms if this agbot agreement is active",
    description = "Confirms whether or not this agreement id is valid, is owned by an agbot owned by this same username, and is a currently active agreement. Can only be run by an agbot or user.",
    parameters = Array(
      new Parameter(
        name = "organization",
        in = ParameterIn.PATH,
        description = "Organization id."
      )
    ),
    requestBody = new RequestBody(
      content = Array(
        new Content(
          examples = Array(
            new ExampleObject(
              value = """{
  "agreementId": "ABCDEF"
}
"""
            )
          ),
          mediaType = "application/json",
          schema = new Schema(implementation = classOf[PostAgreementsConfirmRequest])
        )
      ),
      required = true
    ),
    responses = Array(
      new responses.ApiResponse(
        responseCode = "201",
        description = "response body",
        content = Array(new Content(mediaType = "application/json", schema = new Schema(implementation = classOf[ApiResponse])))
      ),
      new responses.ApiResponse(
        responseCode = "401",
        description = "invalid credentials"
      ),
      new responses.ApiResponse(
        responseCode = "403",
        description = "access denied"
      ),
      new responses.ApiResponse(
        responseCode = "404",
        description = "not found"
      )
    )
  )
  def postConfirm(@Parameter(hidden = true) identity: Identity2,
                  @Parameter(hidden = true) orgid: String,
                  reqBody: PostAgreementsConfirmRequest): Route = {
    logger.debug(s"POST /orgs/$orgid/agreements/confirm - By ${identity.resource}:${identity.role}")
    complete({
      val creds = identity
      identity.role match {
        case (AuthRoles.AdminUser |
              AuthRoles.HubAdmin |
              AuthRoles.SuperUser |
              AuthRoles.User) =>
          // the user invoked this rest method, so look for an agbot owned by this user with this agr id
          val agbotAgreementJoin = for {
            (agbot, agr) <- AgbotsTQ joinLeft AgbotAgreementsTQ on (_.id === _.agbotId)
            if agbot.owner === creds.owner.get && agr.map(_.agrId) == reqBody.agreementId
          } yield (agbot, agr)
          db.run(agbotAgreementJoin.result).map({ list =>
            logger.debug("POST /agreements/confirm of "+reqBody.agreementId+" result: "+list.toString)
            // this list is tuples of (AgbotRow, Option(AgbotAgreementRow)) in which agbot.owner === owner && agr.agrId === req.agreementId
            if (list.nonEmpty && list.head._2.isDefined && list.head._2.get.state != "") {
              (HttpCode.POST_OK, ApiResponse(ApiRespType.OK, ExchMsg.translate("agreement.active")))
            } else {
              (HttpCode.NOT_FOUND, ApiResponse(ApiRespType.NOT_FOUND, ExchMsg.translate("agreement.not.found.not.active")))
            }
          })
        case AuthRoles.Agbot =>
          // an agbot invoked this rest method, so look for the agbot with this id and for the agbot with this agr id, and see if they are owned by the same user
          val agbotAgreementJoin = for {
            (agbot, agr) <- AgbotsTQ joinLeft AgbotAgreementsTQ on (_.id === _.agbotId)
            if agbot.id === creds.resource || agr.map(_.agrId) === reqBody.agreementId
          } yield (agbot, agr)
          db.run(agbotAgreementJoin.result).map({ list =>
            logger.debug("POST /agreements/confirm of "+reqBody.agreementId+" result: "+list.toString)
            if (list.nonEmpty) {
              // this list is tuples of (AgbotRow, Option(AgbotAgreementRow)) in which agbot.id === creds.id || agr.agrId === req.agreementId
              val agbot1 = list.find(r => r._1.id == creds.resource).orNull
              val agbot2 = list.find(r => r._2.isDefined && r._2.get.agrId == reqBody.agreementId).orNull
              if (agbot1 != null && agbot2 != null && agbot1._1.owner == agbot2._1.owner && agbot2._2.get.state != "") {
                (HttpCode.POST_OK, ApiResponse(ApiRespType.OK, ExchMsg.translate("agreement.active")))
              } else {
                (HttpCode.NOT_FOUND, ApiResponse(ApiRespType.NOT_FOUND, ExchMsg.translate("agreement.not.found.not.active")))
              }
            } else {
              (HttpCode.NOT_FOUND, ApiResponse(ApiRespType.NOT_FOUND, ExchMsg.translate("agreement.not.found.not.active")))
            }
          })
        case _ => //node should not be calling this route
          (HttpCode.ACCESS_DENIED, ApiResponse(ApiRespType.ACCESS_DENIED, ExchMsg.translate("access.denied")))
      }
    })
  }
  
  def confirmAgreement(identity: Identity2): Route =
    path("orgs" / Segment / "agreements" / "confirm") {
      organization =>
        post {
          exchAuth(TAgbot(OrgAndId(organization,"#").toString), Access.READ, validIdentity = identity) {
            _ =>
              entity(as[PostAgreementsConfirmRequest]) {
                reqBody =>
                  postConfirm(identity, organization, reqBody)
              }
          }
        }
    }
}
