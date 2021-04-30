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
import uk.gov.hmrc.http.InternalServerException
import uk.gov.hmrc.vatsignupfrontend.utils.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockVatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.controllers.agent.error.{routes => errorRoutes}
import uk.gov.hmrc.vatsignupfrontend.forms.CompanyNumberForm._
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants.{testCompanyName, testCompanyNumber}
import uk.gov.hmrc.vatsignupfrontend.httpparsers.GetCompanyNameHttpParser.CompanyClosed
import uk.gov.hmrc.vatsignupfrontend.models.companieshouse.NonPartnershipEntity
import uk.gov.hmrc.vatsignupfrontend.services.mocks.MockGetCompanyNameService
import uk.gov.hmrc.vatsignupfrontend.views.html.agent.capture_company_number

import scala.concurrent.Future

class CaptureRegisteredSocietyCompanyNumberControllerSpec
  extends UnitSpec with GuiceOneAppPerSuite with MockVatControllerComponents with MockGetCompanyNameService {

  val view = app.injector.instanceOf[capture_company_number]

  object TestCaptureRegisteredSocietyCompanyNumberController extends CaptureRegisteredSocietyCompanyNumberController(
    mockGetCompanyNameService,
    view
  )

  lazy val testGetRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/registered-society-company-number")

  def testPostRequest(companyNumberVal: String): FakeRequest[AnyContentAsFormUrlEncoded] =
    FakeRequest("POST", "/registered-society-company-number").withFormUrlEncodedBody(companyNumber -> companyNumberVal)

  "Calling the show action of the Capture Registered Society Company Number controller" should {
    "go to the Capture Registered Society Company number page" in {
      mockAuthRetrieveAgentEnrolment()

      val result = TestCaptureRegisteredSocietyCompanyNumberController.show(testGetRequest)
      status(result) shouldBe Status.OK
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
    }
  }


  "Calling the submit action of the Capture Registered Society Company Number controller" when {
    "company was found" should {
      "redirect to Confirm Registered Society page" in {
        mockAuthRetrieveAgentEnrolment()
        mockGetCompanyNameSuccess(testCompanyNumber, NonPartnershipEntity)

        val request = testPostRequest(testCompanyNumber)

        val result = TestCaptureRegisteredSocietyCompanyNumberController.submit(request)
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.ConfirmRegisteredSocietyController.show().url)

        session(result).get(SessionKeys.registeredSocietyCompanyNumberKey) shouldBe Some(testCompanyNumber)
      }
    }

    "company is dissolved" should {
      "redirect to dissolved company page" in {
        mockAuthRetrieveAgentEnrolment()
        mockGetCompanyName(testCompanyNumber)(Future.successful(Right(CompanyClosed(testCompanyName))))

        val request = testPostRequest(testCompanyNumber)
        val result = TestCaptureRegisteredSocietyCompanyNumberController.submit(request)

        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(errorRoutes.DissolvedCompanyController.show().url)

        session(result).get(SessionKeys.companyNameKey) shouldBe Some(testCompanyName)
      }
    }

    "company number failed prefix validation - invalid prefix" should {
      "redirect to Company Name Not Found page" in {
        mockAuthRetrieveAgentEnrolment()

        val testCrn = "BR12345"
        val request = testPostRequest(testCrn)

        val result = TestCaptureRegisteredSocietyCompanyNumberController.submit(request)
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(errorRoutes.CompanyNameNotFoundController.show().url)

        session(result).get(SessionKeys.registeredSocietyCompanyNumberKey) shouldBe None
      }
    }

    "company was not found" should {
      "redirect to Registered Society Company Name Not Found page" in {

        mockAuthRetrieveAgentEnrolment()
        mockGetCompanyNameNotFound(testCompanyNumber)

        val request = testPostRequest(testCompanyNumber)

        val result = TestCaptureRegisteredSocietyCompanyNumberController.submit(request)
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(errorRoutes.RegisteredSocietyCompanyNameNotFoundController.show().url)

        session(result).get(SessionKeys.registeredSocietyCompanyNumberKey) shouldBe None
      }
    }

    "company search failed" should {
      "throw Internal Server Exception" in {

        mockAuthRetrieveAgentEnrolment()
        mockGetCompanyNameFailure(testCompanyNumber)

        val request = testPostRequest(testCompanyNumber)

        intercept[InternalServerException](TestCaptureRegisteredSocietyCompanyNumberController.submit(request))
      }
    }
  }

}
