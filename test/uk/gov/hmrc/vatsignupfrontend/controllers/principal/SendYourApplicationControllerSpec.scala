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
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.http.InternalServerException
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.assets.MessageLookup
import uk.gov.hmrc.vatsignupfrontend.controllers.ControllerSpec
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.services.mocks.{MockMigratedSubmissionService, MockSubmissionService}
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.send_your_application

class SendYourApplicationControllerSpec extends ControllerSpec with MockMigratedSubmissionService with MockSubmissionService {

  val view = app.injector.instanceOf[send_your_application]

  object TestSendYourApplicationController extends SendYourApplicationController(
    mockMigratedSubmissionService,
    mockSubmissionService,
    view
  )

  lazy val testGetRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/about-to-submit")

  lazy val testPostRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("POST", "/about-to-submit")

  "Calling the show action of the Send Your Application controller" should {
    "show the Send Your Application page" in {
      mockAuthAdminRole()
      val request = testGetRequest

      val result = TestSendYourApplicationController.show(request)

      status(result) shouldBe Status.OK
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
      titleOf(result) shouldBe MessageLookup.PrincipalSendYourApplication.title
    }
  }

  "Calling the submit action of the Send Your Application controller" when {
    "a Migrated VRN is in Session" should {
      "redirect to Sign up complete page if Submit succeeds" in {
        mockAuthAdminRole()
        val vatNumber = testVatNumber
        val request = testPostRequest.withSession(
          SessionKeys.vatNumberKey -> vatNumber,
          SessionKeys.isMigratedKey -> "true"
        )
        mockMigratedSubmitSuccess(vatNumber)

        val result = TestSendYourApplicationController.submit(request)

        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(resignup.routes.SignUpCompleteController.show().url)
      }
      "throw an exception if Submit fails" in {
        mockAuthAdminRole()
        val vatNumber = testVatNumber
        val request = testPostRequest.withSession(
          SessionKeys.vatNumberKey -> vatNumber,
          SessionKeys.isMigratedKey -> "true"
        )
        mockMigratedSubmitFailure(vatNumber)

        val result = TestSendYourApplicationController.submit(request)

        intercept[InternalServerException](result)
      }
    }
    "a Non-Migrated VRN is in Session" should {
      "redirect to Confirmation page if Submit succeeds" in {
        mockAuthAdminRole()
        val vatNumber = testVatNumber
        val request = testPostRequest.withSession(SessionKeys.vatNumberKey -> vatNumber)
        mockSubmitSuccess(vatNumber)

        val result = TestSendYourApplicationController.submit(request)

        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.InformationReceivedController.show().url)
      }
      "redirect to Direct Debit Terms page is the DD Terms FS is on and user has not accepted them yet" in {
        mockAuthAdminRole()
        val vatNumber = testVatNumber
        val request = testPostRequest.withSession(
          SessionKeys.vatNumberKey -> vatNumber,
          SessionKeys.hasDirectDebitKey -> "true"
        )

        val result = TestSendYourApplicationController.submit(request)

        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.DirectDebitTermsAndConditionsController.show().url)
      }
      "throw an exception if Submit fails" in {
        mockAuthAdminRole()
        val vatNumber = testVatNumber
        val request = testPostRequest.withSession(SessionKeys.vatNumberKey -> vatNumber)
        mockSubmitFailure(vatNumber)

        val result = TestSendYourApplicationController.submit(request)

        intercept[InternalServerException](result)
      }
    }
    "there is no VRN in session" should {
      "redirect to Capture Vat Number page" in {
        mockAuthAdminRole()
        val request = testPostRequest

        val result = TestSendYourApplicationController.submit(request)

        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.ResolveVatNumberController.resolve().url)
      }
    }
  }
}




