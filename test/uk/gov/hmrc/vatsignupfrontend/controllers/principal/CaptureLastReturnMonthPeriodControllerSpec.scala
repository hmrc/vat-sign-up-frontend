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

package uk.gov.hmrc.vatsignupfrontend.controllers.principal

import play.api.http.Status
import play.api.mvc.{AnyContentAsEmpty, AnyContentAsFormUrlEncoded}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.http.NotFoundException
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockVatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.forms.MonthForm
import uk.gov.hmrc.vatsignupfrontend.models.January
import uk.gov.hmrc.vatsignupfrontend.utils.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.capture_last_return_month_period


class CaptureLastReturnMonthPeriodControllerSpec extends UnitSpec
  with MockVatControllerComponents {

  val view = app.injector.instanceOf[capture_last_return_month_period]
  object TestCaptureLastReturnMonthPeriodController extends CaptureLastReturnMonthPeriodController(view)

  lazy val testGetRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/last-vat-return-date")

  lazy val testPostRequest: FakeRequest[AnyContentAsFormUrlEncoded] =
    FakeRequest("POST", "/last-vat-return-date").withFormUrlEncodedBody(MonthForm.month -> January.toString)

  "Calling the show action of the Capture Last Return Month Period controller" should {
    "go to the Capture Last Return Month Period page" in {
      mockAuthAdminRole()

      val result = TestCaptureLastReturnMonthPeriodController.show(testGetRequest)
      status(result) shouldBe Status.OK
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
    }
  }

  "Calling the submit action of the Capture Last Return Month Period controller" when {
    "form successfully submitted" should {
      "go to Check Your Answers Page" in {
        mockAuthAdminRole()

        val result = TestCaptureLastReturnMonthPeriodController.submit(testPostRequest)
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.CheckYourAnswersController.show().url)
        session(result) get SessionKeys.lastReturnMonthPeriodKey should contain(January.toString)
      }
    }
    "form unsuccessfully submitted" should {
      "reload the page with errors" in {
        mockAuthAdminRole()

        val result = TestCaptureLastReturnMonthPeriodController.submit(FakeRequest("POST", "/last-vat-return-date")
          .withFormUrlEncodedBody(MonthForm.month -> "invalid"))
        status(result) shouldBe Status.BAD_REQUEST
        contentType(result) shouldBe Some("text/html")
        charset(result) shouldBe Some("utf-8")
      }
    }
  }
}
