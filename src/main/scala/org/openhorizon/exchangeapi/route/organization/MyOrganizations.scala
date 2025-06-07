package org.openhorizon.exchangeapi.route.organization

import com.github.pjfanning.pekkohttpjackson.JacksonSupport
import io.swagger.v3.oas.annotations.Operation
import io.swagger.v3.oas.annotations.media.{Content, ExampleObject, Schema}
import io.swagger.v3.oas.annotations.responses
import io.swagger.v3.oas.annotations.parameters.RequestBody
import jakarta.ws.rs.{GET, POST, Path}
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.event.LoggingAdapter
import org.apache.pekko.http.scaladsl.model.{StatusCode, StatusCodes}
import org.apache.pekko.http.scaladsl.server.Directives._
import org.apache.pekko.http.scaladsl.server.Route
import org.openhorizon.exchangeapi.auth.cloud.IamAccountInfo
import org.openhorizon.exchangeapi.auth.{Access, AuthenticationSupport, TOrg}
import org.openhorizon.exchangeapi.table.organization.{Org, OrgsTQ}
import org.openhorizon.exchangeapi.table.ExchangePostgresProfile.api._

import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext
import org.openhorizon.exchangeapi.auth.Identity
import io.swagger.v3.oas.annotations.Parameter

final case class OrgSummary(
  label: String,
  description: String
)

final case class GetOrgsSummaryResponse(
  orgs: Map[String, OrgSummary],
  lastIndex: Int
)

@Path("/v1/myorgs")
@io.swagger.v3.oas.annotations.tags.Tag(name = "organization")
trait MyOrganizations extends JacksonSupport with AuthenticationSupport {
  def db: Database
  def system: ActorSystem
  def logger: LoggingAdapter
  implicit def executionContext: ExecutionContext
  
  
  // ====== POST /myorgs ================================
  @POST
  @Path("/v1/myorgs")
  @Operation(summary = "Returns the orgs a user can view", description = "Returns all the org definitions in the exchange that match the accounts the caller has access too. Can be run by any user. Request body is the response from /idmgmt/identity/api/v1/users/<user_ID>/accounts API.",
    requestBody = new RequestBody(
      content = Array(
        new Content(
          examples = Array(
            new ExampleObject(
              value = """[
  {
    "id": "orgid",
    "name": "MyOrg",
    "description": "String Description for Account",
    "createdOn": "2020-09-15T00:20:43.853Z"
  },
  {
    "id": "orgid2",
    "name": "otherOrg",
    "description": "String Description for Account",
    "createdOn": "2020-09-15T00:20:43.853Z"
  }
]"""
            )
          ),
          mediaType = "application/json",
          schema = new Schema(implementation = classOf[List[IamAccountInfo]])
        )
      ),
      required = true
    ),
    responses = Array(
      new responses.ApiResponse(responseCode = "200", description = "response body",
        content = Array(new Content(
          examples = Array(
            new ExampleObject(
              value ="""{
  "orgs": {
    "string" : {
      "orgType": "",
      "label": "",
      "description": "",
      "lastUpdated": "",
      "tags": null,
      "limits": {
        "maxNodes": 0
      },
      "heartbeatIntervals": {
        "minInterval": 0,
        "maxInterval": 0,
        "intervalAdjustment": 0
      }
    }
  },
  "lastIndex": 0
}
"""
            )
          ),
          mediaType = "application/json",
          schema = new Schema(implementation = classOf[GetOrgsResponse])
        )
        )),
      new responses.ApiResponse(responseCode = "400", description = "bad input"),
      new responses.ApiResponse(responseCode = "401", description = "invalid credentials"),
      new responses.ApiResponse(responseCode = "403", description = "access denied"),
      new responses.ApiResponse(responseCode = "404", description = "not found")))
  def postMyOrganizations: Route =
    {
      entity(as[List[IamAccountInfo]]) {
        reqBody =>
          logger.debug("Doing POST /myorgs")
          
          complete({
            val excludedOrgIds = Set("IBM", "root")
            val q = OrgsTQ.filter(org => !(org.orgid inSet excludedOrgIds))
            db.run(q.result).map { list =>
              logger.debug("POST /myorgs result size: {}", list.size)
              val orgs: Map[String, Org] = list.map(a => a.orgId -> a.toOrg).toMap
              val code: StatusCode = if (orgs.nonEmpty) StatusCodes.OK else StatusCodes.NotFound
              (code, GetOrgsResponse(orgs, 0))
            }
          })
      }
    }

  // ====== GET /myorgs ================================
  @GET
  @Path("/v1/myorgs")
  @Operation(summary = "Returns all the user orgs", description = "Returns all the user orgs.",
    responses = Array(
      new responses.ApiResponse(responseCode = "200", description = "response body",
        content = Array(new Content(
          examples = Array(
            new ExampleObject(
              value ="""{
  "orgs": {
    "string" : {
      "orgType": "",
      "label": "",
      "description": ""
    }
  },
  "lastIndex": 0
}
"""
            )
          ),
          mediaType = "application/json",
          schema = new Schema(implementation = classOf[GetOrgsSummaryResponse])
        )
        )),
      new responses.ApiResponse(responseCode = "401", description = "invalid credentials"),
      new responses.ApiResponse(responseCode = "403", description = "access denied"),
      new responses.ApiResponse(responseCode = "404", description = "not found")))
  def getMyOrganizations(@Parameter(hidden = true)identity: Identity): Route =
    {
      logger.debug("Doing GET /myorgs")
      
      complete({
        val excludedOrgTypes = Set("IBM")
        val excludedOrgIds = Set("root")
        val orgId = identity.getOrg

        val orgQuery =
          // if (orgId == "") {
            OrgsTQ
              .filter(org => !(org.orgType inSet excludedOrgTypes) && !(org.orgid inSet excludedOrgIds))
              .map(org => (org.orgid, org.label, org.description))
          // } else {
          //   OrgsTQ
          //     .filter(_.orgid === orgId)
          //     .map(org => (org.orgid, org.label, org.description))
          // }

        db.run(orgQuery.result).map { list =>
          logger.debug("GET /myorgs result size: {}", list.size)
          val orgs: Map[String, OrgSummary] = list.map {
            case (id, label, desc) => id -> OrgSummary(label, desc)
          }.toMap
          val code: StatusCode = if (orgs.nonEmpty) StatusCodes.OK else StatusCodes.NotFound
          (code, GetOrgsSummaryResponse(orgs, 0))
        }
      })
    }
  
  val myOrganizations: Route =
    path("myorgs") {
      post {
        // set hint here to some key that states that no org is ok
        // UI should omit org at the beginning of credentials still have them put the slash in there
        try {
          exchAuth(TOrg("#"), Access.READ_MY_ORG, hint = "exchangeNoOrgForMultLogin") {
            _ =>
              postMyOrganizations
          }
        } catch {
          case ex: Exception => 
            logger.error("[MKMK] POST/myorgs error: ", ex)
            throw ex
        }
      } ~
      get {
        // set hint here to some key that states that no org is ok
        // UI should omit org at the beginning of credentials still have them put the slash in there
        try {
          exchAuth(TOrg("#"), Access.READ_MY_ORG, hint = "exchangeNoOrgForMultLogin") {
            identity => getMyOrganizations(identity)
          }
        } catch {
          case ex: Exception => 
            logger.error("[MKMK] GET/myorgs error: ", ex)
            throw ex
        }
      }
    }
}