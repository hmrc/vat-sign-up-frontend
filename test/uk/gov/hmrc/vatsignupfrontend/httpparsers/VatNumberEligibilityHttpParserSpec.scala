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

import java.time.LocalDate

import play.api.http.Status._
import play.api.libs.json.{JsObject, Json}
import uk.gov.hmrc.http.{HttpResponse, InternalServerException}
import uk.gov.hmrc.vatsignupfrontend.utils.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.httpparsers.VatNumberEligibilityHttpParser._
import uk.gov.hmrc.vatsignupfrontend.models.MigratableDates


class VatNumberEligibilityHttpParserSpec extends UnitSpec {
  val testHttpVerb = "PUT"
  val testUri = "/"

  val currentDate: LocalDate = LocalDate.now()
  val testMigratableDates = MigratableDates(Some(currentDate), Some(currentDate))
  val testEligibilityDetails = Eligible(isOverseas = false, isMigrated = false, isNew = false)

  def response(mtdStatus: String,
               migratableDates: Option[MigratableDates] = None,
               eligibilityDetails: Option[Eligible] = None,
               isOverseas: Option[Boolean] = None): Some[JsObject] = {
    Some(Json.obj(
      MtdStatusKey -> mtdStatus,
      MigratableDatesKey -> Json.toJson(migratableDates),
      EligibilityDetailsKey -> Json.toJson(eligibilityDetails),
      overseasKey -> Json.toJson(isOverseas)
    ))
  }

  "MTDStatusHttpReads" when {
    "read" should {
      s"parse an OK response when the $MtdStatusKey is $AlreadySubscribedValue as AlreadySubscribed" in {
        val httpResponse = HttpResponse(OK, response(mtdStatus = AlreadySubscribedValue, isOverseas = Some(true)))

        val res = VatNumberEligibilityHttpReads.read(testHttpVerb, testUri, httpResponse)

        res shouldBe Right(AlreadySubscribed(isOverseas = true))
      }
      s"parse an OK response when the $MtdStatusKey is $IneligibleValue as Ineligible" in {
        val httpResponse = HttpResponse(OK, response(IneligibleValue))

        val res = VatNumberEligibilityHttpReads.read(testHttpVerb, testUri, httpResponse)

        res shouldBe Right(Ineligible)
      }
      s"parse an OK response when the $MtdStatusKey is $DeregisteredValue as Deregistered" in {
        val httpResponse = HttpResponse(OK, response(DeregisteredValue))

        val res = VatNumberEligibilityHttpReads.read(testHttpVerb, testUri, httpResponse)

        res shouldBe Right(Deregistered)
      }
      s"parse an OK response when the $MtdStatusKey is $MigrationInProgressValue as MigrationInProgress" in {
        val httpResponse = HttpResponse(OK, response(MigrationInProgressValue))

        val res = VatNumberEligibilityHttpReads.read(testHttpVerb, testUri, httpResponse)

        res shouldBe Right(MigrationInProgress)
      }
      s"parse an OK response when the $MtdStatusKey is $InhibitedValue and has correct migratable dates as Inhibited" in {
        val httpResponse = HttpResponse(OK, response(InhibitedValue, migratableDates = Some(testMigratableDates)))

        val res = VatNumberEligibilityHttpReads.read(testHttpVerb, testUri, httpResponse)

        res shouldBe Right(Inhibited(testMigratableDates))
      }
      s"parse an OK response when the $MtdStatusKey is $EligibleValue and has correct eligibility details as Eligible" in {
        val httpResponse = HttpResponse(OK, response(EligibleValue, eligibilityDetails = Some(testEligibilityDetails)))

        val res = VatNumberEligibilityHttpReads.read(testHttpVerb, testUri, httpResponse)

        res shouldBe Right(Eligible(isOverseas = false, isMigrated = false, isNew = false))
      }
      "parse a NOT_FOUND response as VatNumberNotFound" in {
        val httpResponse = HttpResponse(NOT_FOUND)

        val res = VatNumberEligibilityHttpReads.read(testHttpVerb, testUri, httpResponse)

        res shouldBe Left(VatNumberNotFound)
      }
      "parse a BAD_REQUEST response as VatNumberEligibilityFailure" in {
        val httpResponse = HttpResponse(BAD_REQUEST)

        val res = VatNumberEligibilityHttpReads.read(testHttpVerb, testUri, httpResponse)

        res shouldBe Left(VatNumberEligibilityFailure)
      }
      s"throw an InternalServerException for an OK response when the $MtdStatusKey is invalid" in {
        val httpResponse = HttpResponse(OK, response(""))

        intercept[InternalServerException] {
          VatNumberEligibilityHttpReads.read(testHttpVerb, testUri, httpResponse)
        }
      }
    }
  }
}


