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

import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.mvc.AnyContentAsFormUrlEncoded
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.play.test.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockControllerComponents
import uk.gov.hmrc.vatsignupfrontend.forms.EmailForm._
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._

class CaptureAgentEmailControllerSpec extends UnitSpec with GuiceOneAppPerSuite with MockControllerComponents {

  object TestCaptureAgentEmailController extends CaptureAgentEmailController(mockControllerComponents)

  val testGetRequest = FakeRequest("GET", "/your-email-address")

  def testPostRequest(emailAddress: String): FakeRequest[AnyContentAsFormUrlEncoded] =
    FakeRequest("POST", "/your-email-address").withFormUrlEncodedBody(email -> emailAddress)

  "Calling the show action of the Capture Agent Email controller without a submitted Email" should {
    "go to the Capture Agent Email page" in {
      mockAuthRetrieveAgentEnrolment()

      val result = TestCaptureAgentEmailController.show(testGetRequest)

      status(result) shouldBe Status.OK
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
    }
  }

  "Calling the show action of the Capture Agent Email controller with a submitted Email" should {
    "go to the Capture Agent Email page" in {
      mockAuthRetrieveAgentEnrolment()

      val result = TestCaptureAgentEmailController.show(testGetRequest.withSession(SessionKeys.transactionEmailKey -> "test@test.com"))

      status(result) shouldBe Status.SEE_OTHER
      redirectLocation(result) shouldBe Some(routes.ConfirmAgentEmailController.show().url)
    }
  }

  "Calling the submit action of the Capture Agent Email controller" when {
    "form successfully submitted" should {
      "go to the Confirm Email page" in {
        mockAuthRetrieveAgentEnrolment()

        val request = testPostRequest(testEmail)

        val result = TestCaptureAgentEmailController.submit(request)

        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.ConfirmAgentEmailController.show().url)

        await(result).session(request).get(SessionKeys.transactionEmailKey) shouldBe Some(testEmail)
      }
    }

    "form unsuccessfully submitted" should {
      "reload the page with errors" in {
        mockAuthRetrieveAgentEnrolment()

        val result = TestCaptureAgentEmailController.submit(testPostRequest("invalid"))
        status(result) shouldBe Status.BAD_REQUEST
        contentType(result) shouldBe Some("text/html")
        charset(result) shouldBe Some("utf-8")
      }
    }
  }
}
