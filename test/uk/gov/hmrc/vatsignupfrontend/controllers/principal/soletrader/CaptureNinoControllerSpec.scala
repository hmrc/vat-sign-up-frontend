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

package uk.gov.hmrc.vatsignupfrontend.controllers.principal.soletrader

import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.mvc.AnyContentAsFormUrlEncoded
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.play.test.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch.FeatureSwitching
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockControllerComponents
import uk.gov.hmrc.vatsignupfrontend.forms.NinoForm._
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants.testNino

class CaptureNinoControllerSpec extends UnitSpec with GuiceOneAppPerSuite with MockControllerComponents with FeatureSwitching {

  object TestCaptureNinoController extends CaptureNinoController(mockControllerComponents)

  val testGetRequest = FakeRequest("GET", "/national-insurance-number")

  private def testPostRequest(postNino: String = testNino): FakeRequest[AnyContentAsFormUrlEncoded] =
    FakeRequest("POST", "/national-insurance-number").withFormUrlEncodedBody(nino -> postNino)

  "calling the Show method of Capture NINO controller" when {
    "show the Capture NINO page" in {
      mockAuthAdminRole()
      val res = TestCaptureNinoController.show(testGetRequest)
      status(res) shouldBe OK
      contentType(res) shouldBe Some("text/html")
      charset(res) shouldBe Some("utf-8")
    }
  }

  "The submit method of the Capture NINO controller" when {
    "a valid NINO has been submitted" should {
      "redirect to the Confirm Your Details page" in {
        mockAuthAdminRole()
        val res = await(TestCaptureNinoController.submit(testPostRequest()))
        status(res) shouldBe SEE_OTHER
        redirectLocation(res) should contain(routes.ConfirmNinoController.show().url)
        res.session(testPostRequest()).get(SessionKeys.ninoKey) should contain(testNino)
      }
    }
    "an invalid NINO has been submitted" should {
      "display the capture NINO page, with error messages" in {
        mockAuthAdminRole()
        val res = await(TestCaptureNinoController.submit(testPostRequest("QQ123456C")))
        status(res) shouldBe BAD_REQUEST
        contentType(res) shouldBe Some("text/html")
        charset(res) shouldBe Some("utf-8")
      }
    }
  }

}
