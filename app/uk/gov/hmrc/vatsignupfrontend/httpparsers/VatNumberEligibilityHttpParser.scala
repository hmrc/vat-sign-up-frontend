/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.vatsignupfrontend.httpparsers

import play.api.http.Status.{NOT_FOUND, OK}
import play.api.libs.functional.syntax._
import play.api.libs.json.{Format, JsPath, JsSuccess, Json, OFormat, Reads}
import uk.gov.hmrc.http.{HttpReads, HttpResponse, InternalServerException}
import uk.gov.hmrc.vatsignupfrontend.models.MigratableDates


object VatNumberEligibilityHttpParser {

  type VatNumberEligibilityResponse = Either[VatNumberEligibilityFailure, VatNumberEligibilitySuccess]

  val MigrationInProgressValue = "MigrationInProgress"
  val AlreadySubscribedValue = "AlreadySubscribed"
  val IneligibleValue = "Ineligible"
  val InhibitedValue = "Inhibited"
  val EligibleValue = "Eligible"
  val DeregisteredValue = "Deregistered"
  val MtdStatusKey = "mtdStatus"
  val MigratableDatesKey = "migratableDates"
  val EligibilityDetailsKey = "eligibilityDetails"
  val overseasKey = "isOverseas"

  // scalastyle:off
  implicit object VatNumberEligibilityHttpReads extends HttpReads[VatNumberEligibilityResponse] {
    override def read(method: String, url: String, response: HttpResponse): VatNumberEligibilityResponse = {
      response.status match {
        case OK =>
          (response.json \ MtdStatusKey).asOpt[String] match {
            case Some(MigrationInProgressValue) =>
              Right(MigrationInProgress)
            case Some(AlreadySubscribedValue) =>
              val isOverseas = (response.json \ overseasKey).asOpt[Boolean].getOrElse(
                throw new InternalServerException(s"Backend returned a malformed AlreadySubscribed response")
              )
              Right(AlreadySubscribed(isOverseas))
            case Some(IneligibleValue) =>
              Right(Ineligible)
            case Some(DeregisteredValue) =>
              Right(Deregistered)
            case Some(InhibitedValue) =>
              val inhibitedDates = (response.json \ MigratableDatesKey).asOpt[MigratableDates].getOrElse(
                throw new InternalServerException("Backend returned Inhibited state without dates")
              )
              Right(Inhibited(inhibitedDates))
            case Some(EligibleValue) =>
              val eligible: Eligible = (response.json \ EligibilityDetailsKey).asOpt[Eligible].getOrElse(
                throw new InternalServerException("Backend returned Eligible state without details")
              )
              Right(eligible)
            case _ =>
              throw new InternalServerException("Backend returned 200 without a valid MTDStatus")
          }
        case NOT_FOUND =>
          Left(VatNumberNotFound)
        case status =>
          Left(VatNumberEligibilityFailure)
      }
    }
  }

  sealed trait VatNumberEligibilitySuccess

  case class AlreadySubscribed(isOverseas: Boolean) extends VatNumberEligibilitySuccess

  case class Eligible(isOverseas: Boolean, isMigrated: Boolean, isNew: Boolean) extends VatNumberEligibilitySuccess

  object Eligible {
    implicit val reads: Reads[Eligible] = (
      (JsPath \ "isOverseas").read[Boolean] and
        (JsPath \ "isMigrated").read[Boolean] and
        (JsPath \ "isNew").read[Boolean]
      ) (Eligible.apply _)
  }

  implicit val format: OFormat[Eligible] = Json.format[Eligible]

  case object Ineligible extends VatNumberEligibilitySuccess

  case class Inhibited(migratableDates: MigratableDates) extends VatNumberEligibilitySuccess

  case object Deregistered extends VatNumberEligibilitySuccess

  case object MigrationInProgress extends VatNumberEligibilitySuccess

  sealed trait VatNumberEligibilityFailure

  case object VatNumberEligibilityFailure extends VatNumberEligibilityFailure

  case object VatNumberNotFound extends VatNumberEligibilityFailure

}
