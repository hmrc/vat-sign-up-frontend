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


import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.http.InternalServerException
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.assets.MessageLookup
import uk.gov.hmrc.vatsignupfrontend.controllers.ControllerSpec
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.services.mocks.{MockMigratedSubmissionService, MockSubmissionService}
import uk.gov.hmrc.vatsignupfrontend.views.html.agent.send_your_application

class AgentSendYourApplicationControllerSpec extends ControllerSpec with MockMigratedSubmissionService with MockSubmissionService {

  val view = app.injector.instanceOf[send_your_application]

  object TestAgentSendYourApplicationController$ extends AgentSendYourApplicationController(
    mockMigratedSubmissionService,
    mockSubmissionService,
    view
  )

  lazy val testGetRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/about-to-submit")

  lazy val testPostRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("POST", "/client/about-to-submit")

  "Calling the show action of the Send Your Application controller" should {
    "show the Send Your Application page" in {
      mockAuthRetrieveAgentEnrolment()
      val request = testGetRequest

      val result = TestAgentSendYourApplicationController$.show(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
      titleOf(result) shouldBe MessageLookup.AgentSendYourApplication.title
    }
  }

  "Calling the submit action of the Send Your Application controller" when {
    "a Migrated VRN is in Session" should {
      "redirect to Sign up complete page if Submit succeeds" in {
        mockAuthRetrieveAgentEnrolment()
        val vatNumber = testVatNumber
        val request = testPostRequest.withSession(
          SessionKeys.vatNumberKey -> vatNumber,
          SessionKeys.isMigratedKey -> "true"
        )
        mockMigratedSubmitSuccess(vatNumber)

        val result = TestAgentSendYourApplicationController$.submit(request)

        status(result) shouldBe SEE_OTHER
        redirectLocation(result) shouldBe Some(resignup.routes.SignUpCompleteController.show().url)
      }
      "throw an exception if Submit fails" in {
        mockAuthRetrieveAgentEnrolment()
        val vatNumber = testVatNumber
        val request = testPostRequest.withSession(
          SessionKeys.vatNumberKey -> vatNumber,
          SessionKeys.isMigratedKey -> "true"
        )
        mockMigratedSubmitFailure(vatNumber)

        val result = TestAgentSendYourApplicationController$.submit(request)

        intercept[InternalServerException](result)
      }
    }
    "a Non-Migrated VRN is in Session" should {
      "redirect to Confirmation page if Submit succeeds" in {
        mockAuthRetrieveAgentEnrolment()
        val vatNumber = testVatNumber
        val request = testPostRequest.withSession(SessionKeys.vatNumberKey -> vatNumber)
        mockSubmitSuccess(vatNumber)

        val result = TestAgentSendYourApplicationController$.submit(request)

        status(result) shouldBe SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.ConfirmationController.show().url)
      }
      "throw an exception if Submit fails" in {
        mockAuthRetrieveAgentEnrolment()
        val vatNumber = testVatNumber
        val request = testPostRequest.withSession(SessionKeys.vatNumberKey -> vatNumber)
        mockSubmitFailure(vatNumber)

        val result = TestAgentSendYourApplicationController$.submit(request)

        intercept[InternalServerException](result)
      }
    }
    "there is no VRN in session" should {
      "redirect to Capture Vat Number page" in {
        mockAuthRetrieveAgentEnrolment()
        val request = testPostRequest

        val result = TestAgentSendYourApplicationController$.submit(request)

        status(result) shouldBe SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.CaptureVatNumberController.show().url)
      }
    }
  }
}