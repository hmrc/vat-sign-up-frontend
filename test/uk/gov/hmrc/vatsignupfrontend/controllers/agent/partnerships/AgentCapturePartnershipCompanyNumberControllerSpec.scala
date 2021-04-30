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

package uk.gov.hmrc.vatsignupfrontend.controllers.agent.partnerships

import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.mvc.AnyContentAsFormUrlEncoded
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.http.InternalServerException
import uk.gov.hmrc.vatsignupfrontend.utils.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockVatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.controllers.agent.error.{routes => errorRoutes}
import uk.gov.hmrc.vatsignupfrontend.forms.CompanyNumberForm._
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.httpparsers.GetCompanyNameHttpParser.CompanyClosed
import uk.gov.hmrc.vatsignupfrontend.models.companieshouse
import uk.gov.hmrc.vatsignupfrontend.services.mocks.MockGetCompanyNameService
import uk.gov.hmrc.vatsignupfrontend.views.html.agent.partnerships.capture_partnership_company_number

import scala.concurrent.Future

class AgentCapturePartnershipCompanyNumberControllerSpec extends UnitSpec with GuiceOneAppPerSuite with MockVatControllerComponents
  with MockGetCompanyNameService {

  val view = app.injector.instanceOf[capture_partnership_company_number]

  object TestAgentCapturePartnershipCompanyNumberController extends AgentCapturePartnershipCompanyNumberController(
    mockGetCompanyNameService,
    view
  )

  lazy val testGetRequest = FakeRequest("GET", "/partnership-company-number")

  def testPostRequest(companyNumberVal: String): FakeRequest[AnyContentAsFormUrlEncoded] =
    FakeRequest("POST", "/partnership-company-number").withFormUrlEncodedBody(companyNumber -> companyNumberVal)

  "Calling the show action of the Capture Partnership Company Number controller" should {
    "go to the Capture Partnership Company Number page" in {
      mockAuthRetrieveAgentEnrolment()

      val result = TestAgentCapturePartnershipCompanyNumberController.show(testGetRequest)
      status(result) shouldBe Status.OK
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
    }
  }

  "Calling the submit action of the Capture Partnership Company Number controller" when {
    "get company name returned successfully" should {
      "Redirect to Confirm Partnership page" in {
        mockAuthRetrieveAgentEnrolment()

        mockGetCompanyNameSuccess(testCompanyNumber, companieshouse.LimitedPartnership)

        val request = testPostRequest(testCompanyNumber)

        val result = TestAgentCapturePartnershipCompanyNumberController.submit(request)
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.ConfirmPartnershipController.show().url)
        session(result) get SessionKeys.partnershipTypeKey should contain(testPartnershipType)
        session(result) get SessionKeys.companyNumberKey should contain(testCompanyNumber)
        session(result) get SessionKeys.companyNameKey should contain(testCompanyName)
      }
    }

    "get company name returned that the company is dissolved" should {
      "Redirect to dissolved company page" in {
        mockAuthRetrieveAgentEnrolment()
        mockGetCompanyName(testCompanyNumber)(Future.successful(Right(CompanyClosed(testCompanyName))))

        val request = testPostRequest(testCompanyNumber)
        val result = TestAgentCapturePartnershipCompanyNumberController.submit(request)

        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(errorRoutes.DissolvedCompanyController.show().url)

        session(result).get(SessionKeys.companyNameKey) shouldBe Some(testCompanyName)
      }
    }

    "get company name returned a NonPartnershipEntity" should {
      "Redirect to Not a limited Partnership page" in {
        mockAuthRetrieveAgentEnrolment()

        mockGetCompanyNameSuccess(testCompanyNumber, companieshouse.NonPartnershipEntity)

        val request = testPostRequest(testCompanyNumber)

        val result = TestAgentCapturePartnershipCompanyNumberController.submit(request)

        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(errorRoutes.NotALimitedPartnershipController.show().url)
      }
    }

    "form unsuccessfully submitted" should {
      "reload the page with errors" in {
        mockAuthRetrieveAgentEnrolment()

        val result = TestAgentCapturePartnershipCompanyNumberController.submit(testPostRequest("123456789"))
        status(result) shouldBe Status.BAD_REQUEST
        contentType(result) shouldBe Some("text/html")
        charset(result) shouldBe Some("utf-8")
      }
    }

    "get company name returned not found" should {
      "throw an InternalServerException" in {
        mockAuthRetrieveAgentEnrolment()

        mockGetCompanyNameNotFound(testCompanyNumber)

        val request = testPostRequest(testCompanyNumber)

        val result = TestAgentCapturePartnershipCompanyNumberController.submit(request)

        status(result) shouldBe SEE_OTHER
        redirectLocation(result) shouldBe Some(errorRoutes.CouldNotFindPartnershipController.show().url)
      }
    }

    "get company name fails" should {
      "throw an InternalServerException" in {
        mockAuthRetrieveAgentEnrolment()

        mockGetCompanyNameFailure(testCompanyNumber)

        val request = testPostRequest(testCompanyNumber)

        val result = TestAgentCapturePartnershipCompanyNumberController.submit(request)

        intercept[InternalServerException](result)
      }
    }
  }
}
