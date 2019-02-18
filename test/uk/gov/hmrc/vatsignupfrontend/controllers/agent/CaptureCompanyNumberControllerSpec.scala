/*
 * Copyright 2019 HM Revenue & Customs
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
import play.api.mvc.AnyContentAsFormUrlEncoded
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.http.InternalServerException
import uk.gov.hmrc.play.test.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockControllerComponents
import uk.gov.hmrc.vatsignupfrontend.forms.CompanyNumberForm._
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants.testCompanyNumber
import uk.gov.hmrc.vatsignupfrontend.models.companieshouse.NonPartnershipEntity
import uk.gov.hmrc.vatsignupfrontend.services.mocks.MockGetCompanyNameService

class CaptureCompanyNumberControllerSpec extends UnitSpec with GuiceOneAppPerSuite with MockControllerComponents with MockGetCompanyNameService {

  object TestCaptureCompanyNumberController extends CaptureCompanyNumberController(
    mockControllerComponents,
    mockGetCompanyNameService
  )

  lazy val testGetRequest = FakeRequest("GET", "/company-number")

  def testPostRequest(companyNumberVal: String): FakeRequest[AnyContentAsFormUrlEncoded] =
    FakeRequest("POST", "/company-number").withFormUrlEncodedBody(companyNumber -> companyNumberVal)

  "Calling the show action of the Capture Company Number controller" should {
    "go to the Capture Company number page" in {
      mockAuthRetrieveAgentEnrolment()

      val result = TestCaptureCompanyNumberController.show(testGetRequest)
      status(result) shouldBe Status.OK
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
    }
  }


  "Calling the submit action of the Capture Company Number controller" when {
    "company was found" should {
      "redirect to Company Name page" in {

        mockAuthRetrieveAgentEnrolment()
        mockGetCompanyNameSuccess(testCompanyNumber, NonPartnershipEntity)

        val request = testPostRequest(testCompanyNumber)

        val result = TestCaptureCompanyNumberController.submit(request)
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.ConfirmCompanyController.show().url)

        result.session(request).get(SessionKeys.companyNumberKey) shouldBe Some(testCompanyNumber)

      }
    }
    "company was not found" should {
      "redirect to Company Name Not Found page" in {

        mockAuthRetrieveAgentEnrolment()
        mockGetCompanyNameNotFound(testCompanyNumber)

        val request = testPostRequest(testCompanyNumber)

        val result = TestCaptureCompanyNumberController.submit(request)
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.CompanyNameNotFoundController.show().url)

        result.session(request).get(SessionKeys.companyNumberKey) shouldBe None

      }

    }
    "company search failed" should {
      "throw Internal Server Exception" in {

        mockAuthRetrieveAgentEnrolment()
        mockGetCompanyNameFailure(testCompanyNumber)

        val request = testPostRequest(testCompanyNumber)

        intercept[InternalServerException](await(TestCaptureCompanyNumberController.submit(request)))

      }

    }

  }
}
