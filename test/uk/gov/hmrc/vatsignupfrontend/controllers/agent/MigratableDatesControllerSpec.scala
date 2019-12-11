/*
 * Copyright 2019 HM Revenue & Customs
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

package uk.gov.hmrc.vatsignupfrontend.controllers.agent

import java.time.LocalDate

import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.i18n.Messages.Implicits._
import play.api.libs.json.Json
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.play.test.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.SessionKeys._
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockControllerComponents
import uk.gov.hmrc.vatsignupfrontend.models.MigratableDates._
import uk.gov.hmrc.vatsignupfrontend.models.{DateModel, MigratableDates}
import uk.gov.hmrc.vatsignupfrontend.views.html.agent.{sign_up_after_this_date, sign_up_between_these_dates}

class MigratableDatesControllerSpec extends UnitSpec with GuiceOneAppPerSuite with MockControllerComponents {

  object TestMigratableDatesController extends MigratableDatesController(mockControllerComponents)

  def testGetRequest(date: Option[LocalDate] = None, cutoffDate: Option[LocalDate] = None): FakeRequest[AnyContentAsEmpty.type] =
    FakeRequest("GET", "/sign-up-later").withSession(migratableDatesKey -> Json.toJson(MigratableDates(date, cutoffDate)).toString())

  private def dateFormat(date: LocalDate) = DateModel.dateConvert(date).toCheckYourAnswersDateFormat

  "Calling the show action of the migratable dates controller" should {
    "redirect to the capture vat number page" when {
      "no migratable dates are available" in {
        mockAuthRetrieveAgentEnrolment()
        val request = testGetRequest()

        val result = TestMigratableDatesController.show(request)
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.CaptureVatNumberController.show().url)
      }
    }

    "show the sign up after this date page" when {
      "one migratable date is available" in {
        mockAuthRetrieveAgentEnrolment()
        val testDate = LocalDate.now()
        implicit val request = testGetRequest(date = Some(testDate))

        val result = TestMigratableDatesController.show(request)
        status(result) shouldBe Status.OK
        contentType(result) shouldBe Some("text/html")
        charset(result) shouldBe Some("utf-8")

        contentAsString(result) shouldBe sign_up_after_this_date(testDate).body
      }
    }

    "show the sign up between these dates page" when {
      "two migratable dates are available" in {
        mockAuthRetrieveAgentEnrolment()
        val testDate = LocalDate.now()
        val testDate2 = testDate.plusDays(1)

        implicit val request = testGetRequest(Some(testDate), Some(testDate2))

        val result = TestMigratableDatesController.show(request)
        status(result) shouldBe Status.OK
        contentType(result) shouldBe Some("text/html")
        charset(result) shouldBe Some("utf-8")

        contentAsString(result) shouldBe sign_up_between_these_dates(testDate, testDate2).body
      }
    }
  }

}
