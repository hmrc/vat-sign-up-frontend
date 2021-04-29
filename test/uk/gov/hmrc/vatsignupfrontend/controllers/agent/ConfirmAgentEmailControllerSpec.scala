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
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.http.InternalServerException
import uk.gov.hmrc.vatsignupfrontend.utils.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockVatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.services.mocks.MockStoreEmailAddressService
import uk.gov.hmrc.vatsignupfrontend.views.html.agent.confirm_agent_email

class ConfirmAgentEmailControllerSpec extends UnitSpec with GuiceOneAppPerSuite with MockVatControllerComponents
  with MockStoreEmailAddressService {

  val view = app.injector.instanceOf[confirm_agent_email]

  object TestConfirmAgentEmailController extends ConfirmAgentEmailController(mockStoreEmailAddressService, view)

  lazy val testGetRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/confirm-email")

  lazy val testPostRequest: FakeRequest[AnyContentAsEmpty.type] =
    FakeRequest("POST", "/confirm-email")

  "Calling the show action of the Confirm Agent Email controller" when {
    "there is a email in the session" should {
      "show the Confirm Your Email page" in {
        mockAuthRetrieveAgentEnrolment()
        val request = testGetRequest.withSession(
          SessionKeys.vatNumberKey -> testVatNumber,
          SessionKeys.transactionEmailKey -> testEmail
        )

        val result = TestConfirmAgentEmailController.show(request)

        status(result) shouldBe Status.OK
        contentType(result) shouldBe Some("text/html")
        charset(result) shouldBe Some("utf-8")
      }
    }

    "there isn't a email in the session" should {
      "redirect to Capture Agent Email page" in {
        mockAuthRetrieveAgentEnrolment()

        val result = TestConfirmAgentEmailController.show(testGetRequest.withSession(
          SessionKeys.vatNumberKey -> testVatNumber
        ))
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.CaptureAgentEmailController.show().url)
      }
    }
  }

  "Calling the submit action of the Confirm Agent Email controller" when {
    "transaction email and vat number is in session and store call is successful" when {
      "transaction email is not verified" should {
        "redirect to Verify Agent Email page" in {
          mockAuthRetrieveAgentEnrolment()
          mockStoreTransactionEmailAddressSuccess(
            vatNumber = testVatNumber,
            transactionEmail = testEmail
          )(emailVerified = false)

          val result = TestConfirmAgentEmailController.submit(testPostRequest.withSession(
            SessionKeys.transactionEmailKey -> testEmail,
            SessionKeys.vatNumberKey -> testVatNumber
          ))
          redirectLocation(result) shouldBe Some(routes.VerifyAgentEmailController.show().url)
        }
      }
      "email is verified" should {
        "redirect to Contact Preferences page" in {
          mockAuthRetrieveAgentEnrolment()
          mockStoreTransactionEmailAddressSuccess(
            vatNumber = testVatNumber,
            transactionEmail = testEmail
          )(emailVerified = true)

          val result = TestConfirmAgentEmailController.submit(testPostRequest.withSession(
            SessionKeys.transactionEmailKey -> testEmail,
            SessionKeys.vatNumberKey -> testVatNumber
          ))

          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.ContactPreferenceController.show().url)
        }
      }
    }
  }
  "email and vat number is in session but store call is unsuccessful" should {
    "throw Internal Server Error" in {
      mockAuthRetrieveAgentEnrolment()
      mockStoreTransactionEmailAddressFailure(vatNumber = testVatNumber, transactionEmail = testEmail)

      intercept[InternalServerException] {
        TestConfirmAgentEmailController.submit(testPostRequest.withSession(
          SessionKeys.vatNumberKey -> testVatNumber,
          SessionKeys.transactionEmailKey -> testEmail
        ))
      }
    }
  }
  "vat number is not in session" should {
    "redirect to Capture Vat number page" in {
      mockAuthRetrieveAgentEnrolment()

      val result = TestConfirmAgentEmailController.submit(testPostRequest.withSession(
        SessionKeys.transactionEmailKey -> testEmail
      ))
      status(result) shouldBe Status.SEE_OTHER
      redirectLocation(result) shouldBe Some(routes.CaptureVatNumberController.show().url)
    }
  }
  "email is not in session" should {
    "redirect to Capture Agent Email page" in {
      mockAuthRetrieveAgentEnrolment()

      val result = TestConfirmAgentEmailController.submit(testPostRequest.withSession(
        SessionKeys.vatNumberKey -> testVatNumber
      ))
      status(result) shouldBe Status.SEE_OTHER
      redirectLocation(result) shouldBe Some(routes.CaptureAgentEmailController.show().url)
    }
  }

}
