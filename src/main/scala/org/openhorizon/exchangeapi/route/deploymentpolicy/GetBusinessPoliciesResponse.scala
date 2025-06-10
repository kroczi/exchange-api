package org.openhorizon.exchangeapi.route.deploymentpolicy

import org.openhorizon.exchangeapi.table.deploymentpolicy.BusinessPolicy

final case class GetBusinessPoliciesResponse(businessPolicy: Map[String, BusinessPolicy] = Map.empty[String, BusinessPolicy],
                                             lastIndex: Int = 0)
