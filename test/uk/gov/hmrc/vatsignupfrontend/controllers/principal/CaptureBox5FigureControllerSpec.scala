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

import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.http.NotFoundException
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockVatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.forms.Box5FigureForm._
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.utils.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.capture_box_5_figure

class CaptureBox5FigureControllerSpec extends UnitSpec with GuiceOneAppPerSuite with MockVatControllerComponents {

  val view = app.injector.instanceOf[capture_box_5_figure]
  object TestCaptureBox5FigureController extends CaptureBox5FigureController(view)

  lazy val testGetRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/box-5-figure")

  private def testPostRequest(box5Value: String = testBox5Figure) = FakeRequest("POST", "/box-5-figure").withFormUrlEncodedBody(box5Figure -> box5Value)

  "calling the show method on CaptureBox5FigureController" should {
    "go to the capture box five value page" in {
      mockAuthAdminRole()

      val result = TestCaptureBox5FigureController.show(testGetRequest)
      status(result) shouldBe OK
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
    }
  }

  "calling the submit method on CaptureBox5FigureController" should {
    "redirect to CaptureLastMonthReturnPeriod page" in {
      mockAuthAdminRole()

      val result = TestCaptureBox5FigureController.submit(testPostRequest())

      status(result) shouldBe SEE_OTHER
      redirectLocation(result) shouldBe Some(routes.CaptureLastReturnMonthPeriodController.show().url)
      session(result).get(SessionKeys.box5FigureKey) should contain(testBox5Figure)
    }

    "redirect to CaptureLastMonthReturnPeriod page with negative values" in {
      mockAuthAdminRole()

      val result = TestCaptureBox5FigureController.submit(testPostRequest(testBox5FigureNegative))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result) shouldBe Some(routes.CaptureLastReturnMonthPeriodController.show().url)
      session(result).get(SessionKeys.box5FigureKey) should contain(testBox5FigureNegative)
    }

    "form unsuccessfully submitted" should {
      "reload the page with errors" in {
        val testPostRequest = FakeRequest("POST", "/box-5-figure")

        mockAuthAdminRole()

        val result = TestCaptureBox5FigureController.submit(testPostRequest)
        status(result) shouldBe BAD_REQUEST
        contentType(result) shouldBe Some("text/html")
        charset(result) shouldBe Some("utf-8")
      }
    }
  }
}