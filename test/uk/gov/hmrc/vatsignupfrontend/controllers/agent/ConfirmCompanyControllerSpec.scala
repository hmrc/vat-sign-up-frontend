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
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockVatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.services.mocks.MockStoreCompanyNumberService
import uk.gov.hmrc.vatsignupfrontend.views.html.agent.confirm_company

class ConfirmCompanyControllerSpec extends UnitSpec with GuiceOneAppPerSuite with MockVatControllerComponents
  with MockStoreCompanyNumberService {

  override def beforeEach(): Unit = {
    super.beforeEach()
  }

  val view = app.injector.instanceOf[confirm_company]

  object TestConfirmCompanyController extends ConfirmCompanyController(
    mockStoreCompanyNumberService,
    view
  )

  val testGetRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/confirm-company")

  val testPostRequest: FakeRequest[AnyContentAsEmpty.type] =
    FakeRequest("POST", "/confirm-company")

  "Calling the show action of the Confirm Company controller" when {
    "there is a company name in the session" should {
      "go to the Confirm Company page" in {
        mockAuthRetrieveAgentEnrolment()
        val request = testGetRequest.withSession(
          SessionKeys.companyNameKey -> testCompanyName
        )

        val result = TestConfirmCompanyController.show(request)
        status(result) shouldBe Status.OK
        contentType(result) shouldBe Some("text/html")
        charset(result) shouldBe Some("utf-8")

        val changeLink = Jsoup.parse(contentAsString(result)).getElementById("changeLink")
        changeLink.attr("href") shouldBe routes.CaptureCompanyNumberController.show().url
      }
    }

    "there isn't a company name in the session" should {
      "go to the capture company name page" in {
        mockAuthRetrieveAgentEnrolment()

        val result = TestConfirmCompanyController.show(testGetRequest)
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.CaptureCompanyNumberController.show().url)
      }
    }
  }

  "Calling the submit action of the Confirm Company controller" should {
    "go to the capture agent email page" in {
      mockAuthRetrieveAgentEnrolment()
      mockStoreCompanyNumberSuccess(testVatNumber, testCompanyNumber, companyUtr = None)

      val request = testPostRequest.withSession(
        SessionKeys.vatNumberKey -> testVatNumber,
        SessionKeys.companyNumberKey -> testCompanyNumber
      )

      val result = TestConfirmCompanyController.submit(request)
      status(result) shouldBe Status.SEE_OTHER
      redirectLocation(result) shouldBe Some(routes.CaptureAgentEmailController.show().url)
    }

    "throw internal server exception if store company number fails" in {
      mockAuthRetrieveAgentEnrolment()
      mockStoreCompanyNumberFailure(testVatNumber, testCompanyNumber, companyUtr = None)

      val request = testPostRequest.withSession(
        SessionKeys.vatNumberKey -> testVatNumber,
        SessionKeys.companyNumberKey -> testCompanyNumber
      )

      intercept[InternalServerException] {
        TestConfirmCompanyController.submit(request)
      }
    }
    "go to the 'capture vat number' page if vat number is missing" in {
      mockAuthRetrieveAgentEnrolment()

      val request = testPostRequest.withSession(
        SessionKeys.companyNumberKey -> testCompanyNumber
      )

      val result = TestConfirmCompanyController.submit(request)
      status(result) shouldBe Status.SEE_OTHER
      redirectLocation(result) shouldBe Some(routes.CaptureVatNumberController.show().url)
    }
    "go to the 'capture company number' page if company number is missing" in {
      mockAuthRetrieveAgentEnrolment()

      val request = testPostRequest.withSession(
        SessionKeys.vatNumberKey -> testVatNumber
      )

      val result = TestConfirmCompanyController.submit(request)
      status(result) shouldBe Status.SEE_OTHER
      redirectLocation(result) shouldBe Some(routes.CaptureCompanyNumberController.show().url)
    }
  }

}
