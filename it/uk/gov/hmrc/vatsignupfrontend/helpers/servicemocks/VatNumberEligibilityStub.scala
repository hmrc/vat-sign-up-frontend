
package uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks

import play.api.libs.json.Json
import uk.gov.hmrc.vatsignupfrontend.models.MigratableDates


object VatNumberEligibilityStub extends WireMockMethods {

  def stubVatNumberEligibility(vatNumber: String)
                              (status: Int, optEligibilityResponse: Option[EligibilityResponse]): Unit = {

    val jsonBody = optEligibilityResponse match {
      case Some(Eligible) => Json.obj("mtdStatus" -> "Eligible", "eligibilityDetails" -> Json.obj("isMigrated" -> false, "isOverseas" -> false))
      case Some(Migrated) => Json.obj("mtdStatus" -> "Eligible", "eligibilityDetails" -> Json.obj("isMigrated" -> true, "isOverseas" -> false))
      case Some(Ineligible) => Json.obj("mtdStatus" -> "Ineligible")
      case Some(Overseas) => Json.obj("mtdStatus" -> "Eligible", "eligibilityDetails" -> Json.obj("isMigrated" -> false, "isOverseas" -> true))
      case Some(Inhibited(dates)) => Json.obj("mtdStatus" -> "Inhibited", "migratableDates" -> Json.obj("migratableDate" -> dates.migratableDate, "migratableCutoffDate" -> dates.migratableCutoffDate))
      case Some(MigrationInProgress) => Json.obj("mtdStatus" -> "MigrationInProgress")
      case Some(AlreadySubscribed) => Json.obj("mtdStatus" -> "AlreadySubscribed")
      case None => Json.obj()
    }

    when(method = GET, uri = s"/vat-sign-up/subscription-request/vat-number/$vatNumber/new-mtdfb-eligibility")
      .thenReturn(status, jsonBody)

  }

  sealed trait EligibilityResponse

  case object Eligible extends EligibilityResponse
  case object Migrated extends EligibilityResponse
  case object Ineligible extends EligibilityResponse
  case object Overseas extends EligibilityResponse
  case class Inhibited(dates: MigratableDates) extends EligibilityResponse
  case object MigrationInProgress extends EligibilityResponse
  case object AlreadySubscribed extends EligibilityResponse
}
