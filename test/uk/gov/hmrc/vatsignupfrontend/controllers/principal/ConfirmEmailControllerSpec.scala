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
import play.api.http.Status
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.http.InternalServerException
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch.EmailVerification
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockVatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.models.{AlreadyVerifiedEmailAddress, RequestEmailPasscodeSuccessful, StoreEmailVerifiedSuccess}
import uk.gov.hmrc.vatsignupfrontend.services.mocks.{MockEmailVerificationService, MockStoreEmailAddressService}
import uk.gov.hmrc.vatsignupfrontend.utils.UnitSpec

import scala.concurrent.Future
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.confirm_email

class ConfirmEmailControllerSpec extends UnitSpec with GuiceOneAppPerSuite with MockVatControllerComponents
  with MockStoreEmailAddressService
  with MockEmailVerificationService {

  val view = app.injector.instanceOf[confirm_email]

  object TestConfirmEmailController extends ConfirmEmailController(mockStoreEmailAddressService, mockEmailVerificationService, view)

  lazy val testGetRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/confirm-email")

  lazy val testPostRequest: FakeRequest[AnyContentAsEmpty.type] =
    FakeRequest("POST", "/confirm-email")

  "Calling the show action of the Confirm Email controller" when {
    "there is a email in the session" should {
      "show the Confirm Email page" in {
        mockAuthAdminRole()

        val request = testGetRequest.withSession(SessionKeys.vatNumberKey -> testVatNumber, SessionKeys.emailKey -> testEmail)

        val result = TestConfirmEmailController.show(request)

        status(result) shouldBe Status.OK
        contentType(result) shouldBe Some("text/html")
        charset(result) shouldBe Some("utf-8")
      }
    }

    "there isn't a email in the session" should {
      "redirect to Capture Email page" in {
        mockAuthAdminRole()

        val result = TestConfirmEmailController.show(testGetRequest.withSession(SessionKeys.vatNumberKey -> testVatNumber))
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.CaptureEmailController.show().url)
      }
    }
  }

  "Calling the submit action of the Confirm Email controller" when {
    "email and vat number are in session" when {
      "the EmailVerified FS is disabled" when {
        "store call is successful" when {
          "email is not verified" should {
            "redirect to Verify Email page" in {
              mockAuthAdminRole()
              mockStoreTransactionEmailAddressSuccess(vatNumber = testVatNumber, transactionEmail = testEmail)(emailVerified = false)

              val result = TestConfirmEmailController.submit(testPostRequest.withSession(SessionKeys.emailKey -> testEmail,
                SessionKeys.vatNumberKey -> testVatNumber))

              status(result) shouldBe Status.SEE_OTHER
              redirectLocation(result) shouldBe Some(routes.VerifyEmailController.show().url)

            }
          }
          "email is verified" should {
            "redirect to receive email notifications controller page" in {
              mockAuthAdminRole()
              mockStoreTransactionEmailAddressSuccess(vatNumber = testVatNumber, transactionEmail = testEmail)(emailVerified = true)

              val result = TestConfirmEmailController.submit(testPostRequest.withSession(SessionKeys.emailKey -> testEmail,
                SessionKeys.vatNumberKey -> testVatNumber))

              status(result) shouldBe Status.SEE_OTHER
              redirectLocation(result) shouldBe Some(routes.ReceiveEmailNotificationsController.show().url)

            }
          }
        }
        "store call is unsuccessful" should {
          "throw Internal Server Error" in {
            mockAuthAdminRole()
            mockStoreTransactionEmailAddressFailure(vatNumber = testVatNumber, transactionEmail = testEmail)

            intercept[InternalServerException] {
              TestConfirmEmailController.submit(testPostRequest.withSession(
                SessionKeys.vatNumberKey -> testVatNumber,
                SessionKeys.emailKey -> testEmail
              ))
            }
          }
        }
      }
      "the EmailVerified FS is enabled" when {
        "the request passcode API returns RequestEmailPasscodeSuccessful" should {
          "redirect to the CaptureEmailPasscode page" in {
            enable(EmailVerification)
            mockAuthAdminRole()
            mockRequestPasscode(testEmail)(Future.successful(RequestEmailPasscodeSuccessful))

            val res = TestConfirmEmailController.submit(testPostRequest.withSession(SessionKeys.emailKey -> testEmail,
              SessionKeys.vatNumberKey -> testVatNumber))

            status(res) shouldBe Status.SEE_OTHER
            redirectLocation(res) shouldBe Some(routes.CaptureEmailPasscodeController.show().url)
          }
        }
        "the request passcode API returns AlreadyVerifiedEmail" should {
          "store the email and redirect to the EmailVerified page" in {
            enable(EmailVerification)
            mockAuthAdminRole()
            mockRequestPasscode(testEmail)(Future.successful(AlreadyVerifiedEmailAddress))
            mockStoreEmailVerified(testVatNumber, testEmail)(Future.successful(StoreEmailVerifiedSuccess))

            val res = TestConfirmEmailController.submit(testPostRequest.withSession(SessionKeys.emailKey -> testEmail,
              SessionKeys.vatNumberKey -> testVatNumber))

            status(res) shouldBe Status.SEE_OTHER
            redirectLocation(res) shouldBe Some(routes.EmailVerifiedController.show().url)
          }
          "throw an exception if the store email call fails" in {
            enable(EmailVerification)
            mockAuthAdminRole()
            mockRequestPasscode(testEmail)(Future.successful(AlreadyVerifiedEmailAddress))
            mockStoreEmailVerified(testVatNumber, testEmail)(Future.failed(new InternalServerException("")))

            intercept[InternalServerException] {
              await(TestConfirmEmailController.submit(testPostRequest.withSession(SessionKeys.emailKey -> testEmail,
                SessionKeys.vatNumberKey -> testVatNumber)))
            }
          }
        }
        "the request passcode API call fails" should {
          "throw an exception" in {
            enable(EmailVerification)
            mockAuthAdminRole()
            mockRequestPasscode(testEmail)(Future.failed(new InternalServerException("")))

            intercept[InternalServerException] {
              await(TestConfirmEmailController.submit(testPostRequest.withSession(SessionKeys.emailKey -> testEmail,
                SessionKeys.vatNumberKey -> testVatNumber)))
            }
          }
        }
      }
    }
    "vat number is not in session" should {
      "redirect to Capture Vat number page" in {
        mockAuthAdminRole()

        val result = TestConfirmEmailController.submit(testPostRequest.withSession(SessionKeys.emailKey -> testEmail))
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.ResolveVatNumberController.resolve().url)

      }
    }
    "email is not in session" should {
      "redirect to Capture Email page" in {
        mockAuthAdminRole()
        mockAuthRetrieveAgentEnrolment()

        val result = TestConfirmEmailController.submit(testPostRequest.withSession(SessionKeys.vatNumberKey -> testVatNumber))
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.CaptureEmailController.show().url)
      }
    }
  }

}
