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
import play.api.mvc.{AnyContentAsEmpty, AnyContentAsFormUrlEncoded}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.http.InternalServerException
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockVatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.forms.ContactPreferencesForm._
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.models._
import uk.gov.hmrc.vatsignupfrontend.services.mocks.{MockStoreContactPreferenceService, MockStoreEmailAddressService}
import uk.gov.hmrc.vatsignupfrontend.utils.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.receive_email_notifications

class ReceiveEmailNotificationsControllerSpec extends UnitSpec with GuiceOneAppPerSuite with MockVatControllerComponents
  with MockStoreContactPreferenceService with MockStoreEmailAddressService {

  override def beforeEach(): Unit = {
    super.beforeEach()
  }

  val view = app.injector.instanceOf[receive_email_notifications]

  object TestReceiveEmailController extends ReceiveEmailNotificationsController(
    mockStoreContactPreferenceService,
    mockStoreEmailAddressService,
    view
  )

  val testGetRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/receive-email-notifications")

  def testPostRequest(answer: String): FakeRequest[AnyContentAsFormUrlEncoded] =
    FakeRequest("POST", "/receive-email-notifications").withFormUrlEncodedBody(contactPreference -> answer)

  "Calling the show action of the Receive Email Notifications Controller" when {
    "there is an email in session" should {
      "go to the Receive Email Notifications page" in {
        mockAuthAdminRole()
        val request = testGetRequest.withSession(
          SessionKeys.emailKey -> testEmail
        )

        val result = TestReceiveEmailController.show(request)
        status(result) shouldBe Status.OK
        contentType(result) shouldBe Some("text/html")
        charset(result) shouldBe Some("utf-8")
      }
    }
    "there isn't a email in session" should {
      "go to the capture email page" in {
        mockAuthAdminRole()

        val result = TestReceiveEmailController.show(testGetRequest)
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.CaptureEmailController.show().url)
      }
    }

    "Calling the submit action of the Receive Email Notifications controller" when {
      "vat number is in session" when {
        "User answered Digital" should {
          "go to the 'check your answers final' page" in {
            mockAuthAdminRole()
            mockStoreContactPreferenceSuccess(
              vatNumber = testVatNumber,
              contactPreference = Digital
            )
            mockStoreEmailAddressSuccess(
              vatNumber = testVatNumber,
              email = testEmail
            )(emailVerified = true)

            val result = TestReceiveEmailController.submit(testPostRequest("digital").withSession(
              SessionKeys.vatNumberKey -> testVatNumber,
              SessionKeys.emailKey -> testEmail
            ))
            status(result) shouldBe Status.SEE_OTHER
            redirectLocation(result) shouldBe Some(routes.CheckYourAnswersFinalController.show().url)
          }
        }
        "User answered Paper" should {
          "go to check your answers final page" in {
            mockAuthAdminRole()
            mockStoreContactPreferenceSuccess(
              vatNumber = testVatNumber,
              contactPreference = Paper
            )

            val result = TestReceiveEmailController.submit(testPostRequest("paper").withSession(
              SessionKeys.vatNumberKey -> testVatNumber,
              SessionKeys.emailKey -> testEmail
            ))
            status(result) shouldBe Status.SEE_OTHER
            redirectLocation(result) shouldBe Some(routes.CheckYourAnswersFinalController.show().url)
          }
        }
        "User answered Paper with Direct Debit in session" should {
          "go to check your answers final page" in {
            mockAuthAdminRole()
            mockStoreContactPreferenceSuccess(
              vatNumber = testVatNumber,
              contactPreference = Paper
            )
            mockStoreEmailAddressSuccess(
              vatNumber = testVatNumber,
              email = testEmail
            )(emailVerified = true)

            val result = TestReceiveEmailController.submit(testPostRequest("paper").withSession(
              SessionKeys.vatNumberKey -> testVatNumber,
              SessionKeys.emailKey -> testEmail,
              SessionKeys.hasDirectDebitKey -> "true"
            ))
            status(result) shouldBe Status.SEE_OTHER
            redirectLocation(result) shouldBe Some(routes.CheckYourAnswersFinalController.show().url)
          }
        }
      }
    }
    "vat number is not in session" should {
      "go to resolve vat number controller" in {
        mockAuthAdminRole()

        val result = TestReceiveEmailController.submit(testPostRequest("digital").withSession(
          SessionKeys.emailKey -> testEmail
        ))
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.ResolveVatNumberController.resolve().url)
      }
    }
  }
  "email is not in session" should {
    "redirect to capture your email page" in {
      mockAuthAdminRole()

      val result = TestReceiveEmailController.submit(testPostRequest("").withSession(
        SessionKeys.vatNumberKey -> testVatNumber
      ))
      status(result) shouldBe Status.SEE_OTHER
      redirectLocation(result) shouldBe Some(routes.CaptureEmailController.show().url)
    }
  }
  "store contact preference failed" should {
    "throw Internal Server Exception" in {
      mockAuthAdminRole()
      mockStoreContactPreferenceFailure(
        vatNumber = testVatNumber,
        contactPreference = Paper
      )

      val result = TestReceiveEmailController.submit(testPostRequest("paper").withSession(
        SessionKeys.vatNumberKey -> testVatNumber,
        SessionKeys.emailKey -> testEmail)
      )

      intercept[InternalServerException](result)

    }
  }
  "store email address service failed" should {
    "throw Internal Server Exception" in {
      mockAuthAdminRole()
      mockStoreContactPreferenceFailure(
        vatNumber = testVatNumber,
        contactPreference = Paper
      )
      mockStoreEmailAddressFailure(
        vatNumber = testVatNumber,
        email = testEmail
      )

      val result = TestReceiveEmailController.submit(testPostRequest("paper").withSession(
        SessionKeys.vatNumberKey -> testVatNumber,
        SessionKeys.emailKey -> testEmail)
      )

      intercept[InternalServerException](result)

    }
  }

}
