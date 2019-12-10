/*
 * Copyright 2018 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks

import play.api.http.Status.{INTERNAL_SERVER_ERROR, NOT_FOUND, OK}
import play.api.libs.json.Json
import uk.gov.hmrc.vatsignupfrontend.helpers.IntegrationTestConstants.testCompanyName
import uk.gov.hmrc.vatsignupfrontend.httpparsers.GetCompanyNameHttpParser._
import uk.gov.hmrc.vatsignupfrontend.models.companieshouse._
import uk.gov.hmrc.vatsignupfrontend.utils.JsonUtils._

object GetCompanyNameStub extends WireMockMethods {

  def stubGetCompanyName(companyNumber: String, companyType: CompanyType, companyStatus: Option[String] = None): Unit = {
    when(method = GET, uri = s"/incorporation-information/$companyNumber/incorporated-company-profile")
      .thenReturn(
        status = OK,
        body = Json.obj(
          "company_name" -> testCompanyName,
          "type" -> {
            companyType match {
              case LimitedPartnership => LimitedPartnershipKey
              case LimitedLiabilityPartnership => LimitedLiabilityPartnershipKey
              case ScottishLimitedPartnership => ScottishLimitedPartnershipKey
              case NonPartnershipEntity => "ltd"
            }
          }
        ) + ("company_status" -> companyStatus)
      )
  }

  def stubGetCompanyNameCompanyNotFound(companyNumber: String): Unit = {
    when(method = GET, uri = s"/incorporation-information/$companyNumber/incorporated-company-profile")
      .thenReturn(status = NOT_FOUND, body = Json.obj())
  }

  def stubGetCompanyNameCompanyFailure(companyNumber: String): Unit = {
    when(method = GET, uri = s"/incorporation-information/$companyNumber/incorporated-company-profile")
      .thenReturn(status = INTERNAL_SERVER_ERROR)
  }
}
