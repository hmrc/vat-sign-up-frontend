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

package uk.gov.hmrc.vatsignupfrontend.controllers.agent

import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.mvc.{AnyContentAsEmpty, AnyContentAsFormUrlEncoded}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockVatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.forms.EmailForm._
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.utils.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.views.html.agent.capture_agent_email

class CaptureAgentEmailControllerSpec extends UnitSpec with GuiceOneAppPerSuite with MockVatControllerComponents {

  val view = app.injector.instanceOf[capture_agent_email]

  object TestCaptureAgentEmailController extends CaptureAgentEmailController(view)

  val uri = "/client/email-address"
  val testGetRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", uri)

  def testPostRequest(emailAddress: String): FakeRequest[AnyContentAsFormUrlEncoded] =
    FakeRequest("POST", uri).withFormUrlEncodedBody(email -> emailAddress)

  "Calling the show action of the Capture Agent Email controller" when {
    "the isMigrated session flag is true" should {
      "redirect to SendYourApplication" in {
        mockAuthRetrieveAgentEnrolment()

        val reqWithSession = FakeRequest("POST", uri).withSession(
          SessionKeys.vatNumberKey -> testVatNumber,
          SessionKeys.isMigratedKey -> true.toString
        )
        val result = TestCaptureAgentEmailController.show(reqWithSession)

        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.AgentSendYourApplicationController.show().url)
      }
    }
    "the isMigrated session flag is false" should {
      "return OK" in {
        mockAuthRetrieveAgentEnrolment()

        val reqWithSession = FakeRequest("POST", uri).withSession(
          SessionKeys.vatNumberKey -> testVatNumber,
          SessionKeys.isMigratedKey -> false.toString
        )
        val result = TestCaptureAgentEmailController.show(reqWithSession)

        status(result) shouldBe Status.OK
      }
    }
    "the isMigrated session flag isn't set" should {
      "redirect to CheckYourAnswersFinal" in {
        mockAuthRetrieveAgentEnrolment()

        val reqWithSession = FakeRequest("POST", uri).withSession(SessionKeys.vatNumberKey -> testVatNumber)
        val result = TestCaptureAgentEmailController.show(reqWithSession)

        status(result) shouldBe Status.OK
      }
    }
  }

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

  "Calling the change action of the Capture Agent Email controller without a submitted Email" should {
    "go to the Capture Agent Email page" in {
      mockAuthRetrieveAgentEnrolment()

      val result = TestCaptureAgentEmailController.change(testGetRequest)

      status(result) shouldBe Status.SEE_OTHER
      session(result).get(SessionKeys.transactionEmailKey) shouldBe None

    }
  }

  "Calling the change action of the Capture Agent Email controller with a submitted Email" should {
    "go to the Capture Agent Email page" in {
      mockAuthRetrieveAgentEnrolment()
      val request = testGetRequest.withSession(SessionKeys.transactionEmailKey -> "test@test.com")

      val result = TestCaptureAgentEmailController.change(request)

      status(result) shouldBe Status.SEE_OTHER
      session(result).get(SessionKeys.transactionEmailKey) shouldBe None

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

        session(result).get(SessionKeys.transactionEmailKey) shouldBe Some(testEmail)
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
