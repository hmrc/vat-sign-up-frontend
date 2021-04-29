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

import org.jsoup.Jsoup
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.http.InternalServerException
import uk.gov.hmrc.vatsignupfrontend.utils.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch.FeatureSwitching
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockVatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.services.mocks.MockStoreRegisteredSocietyService
import uk.gov.hmrc.vatsignupfrontend.views.html.agent.confirm_registered_society

class ConfirmRegisteredSocietyControllerSpec extends UnitSpec with GuiceOneAppPerSuite with MockVatControllerComponents
  with MockStoreRegisteredSocietyService with FeatureSwitching {

  val view = app.injector.instanceOf[confirm_registered_society]

  object TestConfirmRegisteredSocietyController extends ConfirmRegisteredSocietyController(mockStoreRegisteredSocietyService, view)

  val testGetRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/client/confirm-registered-society")

  val testPostRequest: FakeRequest[AnyContentAsEmpty.type] =
    FakeRequest("POST", "/client/confirm-registered-society")

  "Calling the show action of the Confirm Registered Society controller" when {
    "there is a registered society name in the session" should {
      "go to the Confirm Registered Society page" in {
        mockAuthRetrieveAgentEnrolment()
        val request = testGetRequest.withSession(
          SessionKeys.registeredSocietyNameKey -> testCompanyName
        )

        val result = TestConfirmRegisteredSocietyController.show(request)
        status(result) shouldBe Status.OK
        contentType(result) shouldBe Some("text/html")
        charset(result) shouldBe Some("utf-8")

        val changeLink = Jsoup.parse(contentAsString(result)).getElementById("changeLink")
        changeLink.attr("href") shouldBe routes.CaptureRegisteredSocietyCompanyNumberController.show().url
      }
    }

    "there isn't a registered society name in the session" should {
      "go to the capture registered society company number page" in {
        mockAuthRetrieveAgentEnrolment()

        val result = TestConfirmRegisteredSocietyController.show(testGetRequest)
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.CaptureRegisteredSocietyCompanyNumberController.show().url)
      }
    }
  }

  "Calling the submit action of the Confirm Registered Society controller" should {
    "go to the capture agent email page" in {
      mockAuthRetrieveAgentEnrolment()
      mockStoreRegisteredSocietySuccess(
        vatNumber = testVatNumber,
        companyNumber = testCompanyNumber,
        companyUtr = None
      )

      val request = testPostRequest.withSession(
        SessionKeys.vatNumberKey -> testVatNumber,
        SessionKeys.registeredSocietyCompanyNumberKey -> testCompanyNumber
      )

      val result = TestConfirmRegisteredSocietyController.submit(request)
      status(result) shouldBe Status.SEE_OTHER
      redirectLocation(result) shouldBe Some(routes.CaptureAgentEmailController.show().url)
    }

    "throw internal server exception if store registered society fails" in {
      mockAuthRetrieveAgentEnrolment()
      mockStoreRegisteredSocietyFailure(testVatNumber, testCompanyNumber, None)

      val request = testPostRequest.withSession(
        SessionKeys.vatNumberKey -> testVatNumber,
        SessionKeys.registeredSocietyCompanyNumberKey -> testCompanyNumber
      )

      intercept[InternalServerException] {
        TestConfirmRegisteredSocietyController.submit(request)
      }
    }
    "go to the 'your clients vat number' page if vat number is missing" in {
      mockAuthRetrieveAgentEnrolment()

      val request = testPostRequest.withSession(
        SessionKeys.registeredSocietyCompanyNumberKey -> testCompanyNumber
      )

      val result = TestConfirmRegisteredSocietyController.submit(request)
      status(result) shouldBe Status.SEE_OTHER
      redirectLocation(result) shouldBe Some(routes.CaptureVatNumberController.show().url)

    }
    "go to the 'capture registered society company number' page if company number is missing" in {
      mockAuthRetrieveAgentEnrolment()

      val request = testPostRequest.withSession(
        SessionKeys.vatNumberKey -> testVatNumber
      )

      val result = TestConfirmRegisteredSocietyController.submit(request)
      status(result) shouldBe Status.SEE_OTHER
      redirectLocation(result) shouldBe Some(routes.CaptureRegisteredSocietyCompanyNumberController.show().url)
    }
  }

}
