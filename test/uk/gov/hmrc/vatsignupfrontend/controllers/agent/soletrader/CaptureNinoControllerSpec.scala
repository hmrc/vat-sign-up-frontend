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

package uk.gov.hmrc.vatsignupfrontend.controllers.agent.soletrader

import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.mvc.AnyContentAsFormUrlEncoded
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.vatsignupfrontend.utils.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockVatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.controllers.agent.soletrader.routes.ConfirmNinoController
import uk.gov.hmrc.vatsignupfrontend.forms.NinoForm.nino
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants.testNino
import uk.gov.hmrc.vatsignupfrontend.views.html.agent.soletrader.agent_capture_nino

class CaptureNinoControllerSpec extends UnitSpec with GuiceOneAppPerSuite with MockVatControllerComponents {

  val view = app.injector.instanceOf[agent_capture_nino]

  object TestCaptureNinoController extends CaptureNinoController(view)

  val testGetRequest = FakeRequest("GET", "/client/national-insurance-number")

  private def testPostRequest(postNino: String = testNino): FakeRequest[AnyContentAsFormUrlEncoded] =
    FakeRequest("POST", "/client/national-insurance-number").withFormUrlEncodedBody(nino -> postNino)

  "Calling the Show method of the Capture NINO controller for agent" when {
    "show the Capture NINO page" in {
      mockAuthRetrieveAgentEnrolment()
      val res = TestCaptureNinoController.show(testGetRequest)
      status(res) shouldBe OK
      contentType(res) shouldBe Some("text/html")
      charset(res) shouldBe Some("utf-8")
    }
  }
  "Calling the Submit method of the Capture NINO controller for agent" when {
    "A valid Nino has been submitted" should {
      "Redirect to the confirm NINO page " in {
        mockAuthRetrieveAgentEnrolment()
        val res = TestCaptureNinoController.submit(testPostRequest())

        status(res) shouldBe SEE_OTHER
        redirectLocation(res) should contain(ConfirmNinoController.show().url)
      }
    }
    "An invalid NINO has not been submitted" should {
      "Display the Capture NINO page with error message" in {
        mockAuthRetrieveAgentEnrolment()
        val res = TestCaptureNinoController.submit(testPostRequest("QQ123456C"))
        status(res) shouldBe BAD_REQUEST
        contentType(res) shouldBe Some("text/html")
        charset(res) shouldBe Some("utf-8")
      }
    }
  }
}
