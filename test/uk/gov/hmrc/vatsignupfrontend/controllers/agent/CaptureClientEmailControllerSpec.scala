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
import uk.gov.hmrc.vatsignupfrontend.utils.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockVatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.controllers.agent.error.{routes => errorRoutes}
import uk.gov.hmrc.vatsignupfrontend.forms.EmailForm._
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstantsGenerator
import uk.gov.hmrc.vatsignupfrontend.views.html.agent.capture_client_email

class CaptureClientEmailControllerSpec extends UnitSpec with GuiceOneAppPerSuite with MockVatControllerComponents {

  val view = app.injector.instanceOf[capture_client_email]

  object TestCaptureClientEmailController extends CaptureClientEmailController(view)

  val testGetRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/email-address")

  def testPostRequest(emailAddress: String): FakeRequest[AnyContentAsFormUrlEncoded] =
    FakeRequest("POST", "/email-address").withFormUrlEncodedBody(email -> emailAddress)

  "Calling the show action of the Capture Email controller" should {
    "go to the Capture Email page" in {
      mockAuthRetrieveAgentEnrolment()

      val result = TestCaptureClientEmailController.show(testGetRequest)

      status(result) shouldBe Status.OK
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
    }
  }


  "Calling the submit action of the Capture Email controller" when {
    "form successfully submitted" when {
      "there is no provided agent e-mail address" should {
        "go to the Confirm Email page" in {
          mockAuthRetrieveAgentEnrolment()

          val request = testPostRequest(testEmail)

          val result = TestCaptureClientEmailController.submit(request)
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.ConfirmClientEmailController.show().url)

          session(result).get(SessionKeys.emailKey) shouldBe Some(testEmail)
        }
      }
      "the provided e-mail address does not match the provided agent e-mail address" should {
        "go to the Confirm Email page" in {
          mockAuthRetrieveAgentEnrolment()

          val nonMatchingEmail = TestConstantsGenerator.randomEmail

          val request = testPostRequest(testEmail).withSession(SessionKeys.transactionEmailKey -> nonMatchingEmail)

          val result = TestCaptureClientEmailController.submit(request)
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.ConfirmClientEmailController.show().url)

          session(result).get(SessionKeys.emailKey) shouldBe Some(testEmail)
        }
      }

      "the provided e-mail address matches the provided agent e-mail address" should {
        "go to the use different email address page" in {
          mockAuthRetrieveAgentEnrolment()

          val request = testPostRequest(testEmail).withSession(SessionKeys.transactionEmailKey -> testEmail)

          val result = TestCaptureClientEmailController.submit(request)
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(errorRoutes.UseDifferentEmailAddressController.show().url)
        }
      }
    }

    "form unsuccessfully submitted" should {
      "reload the page with errors" in {
        mockAuthRetrieveAgentEnrolment()

        val result = TestCaptureClientEmailController.submit(testPostRequest("invalid"))
        status(result) shouldBe Status.BAD_REQUEST
        contentType(result) shouldBe Some("text/html")
        charset(result) shouldBe Some("utf-8")
      }
    }
  }
}