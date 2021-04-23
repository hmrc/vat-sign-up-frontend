/*
 * Copyright 2020 HM Revenue & Customs
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

import play.api.libs.json.Json
import uk.gov.hmrc.vatsignupfrontend.models.MigratableDates


object VatNumberEligibilityStub extends WireMockMethods {

  def stubVatNumberEligibility(vatNumber: String)
                              (status: Int, optEligibilityResponse: Option[EligibilityResponse]): Unit = {

    val jsonBody = optEligibilityResponse match {
      case Some(Eligible) => Json.obj(
        "mtdStatus" -> "Eligible",
        "eligibilityDetails" -> Json.obj("isMigrated" -> false, "isOverseas" -> false, "isNew" -> false)
      )
      case Some(RecentlyRegistered) => Json.obj(
        "mtdStatus" -> "Eligible",
        "eligibilityDetails" -> Json.obj("isMigrated" -> false, "isOverseas" -> false, "isNew" -> true)
      )
      case Some(Migrated) => Json.obj(
        "mtdStatus" -> "Eligible",
        "eligibilityDetails" -> Json.obj("isMigrated" -> true, "isOverseas" -> false, "isNew" -> false)
      )
      case Some(Ineligible) => Json.obj("mtdStatus" -> "Ineligible")
      case Some(Deregistered) => Json.obj("mtdStatus" -> "Deregistered")
      case Some(Overseas(isMigrated)) => Json.obj(
        "mtdStatus" -> "Eligible",
        "eligibilityDetails" -> Json.obj("isMigrated" -> isMigrated, "isOverseas" -> true, "isNew" -> false)
      )
      case Some(Inhibited(dates)) => Json.obj(
        "mtdStatus" -> "Inhibited",
        "migratableDates" -> Json.obj("migratableDate" -> dates.migratableDate, "migratableCutoffDate" -> dates.migratableCutoffDate)
      )
      case Some(MigrationInProgress) => Json.obj("mtdStatus" -> "MigrationInProgress")
      case Some(AlreadySubscribed(isOverseas)) => Json.obj("mtdStatus" -> "AlreadySubscribed", "isOverseas" -> isOverseas)
      case None => Json.obj()
    }

    when(method = GET, uri = s"/vat-sign-up/subscription-request/vat-number/$vatNumber/new-mtdfb-eligibility")
      .thenReturn(status, jsonBody)

  }

  sealed trait EligibilityResponse

  case object Eligible extends EligibilityResponse
  case object RecentlyRegistered extends EligibilityResponse
  case object Migrated extends EligibilityResponse
  case object Ineligible extends EligibilityResponse
  case object Deregistered extends EligibilityResponse
  case class Overseas(isMigrated: Boolean) extends EligibilityResponse
  case class Inhibited(dates: MigratableDates) extends EligibilityResponse
  case object MigrationInProgress extends EligibilityResponse
  case class AlreadySubscribed(isOverseas: Boolean) extends EligibilityResponse

}
