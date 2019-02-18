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
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch.RegisteredSocietyJourney
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockControllerComponents
import uk.gov.hmrc.vatsignupfrontend.forms.CompanyNumberForm._
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants.testCompanyNumber
import uk.gov.hmrc.vatsignupfrontend.models.companieshouse.NonPartnershipEntity
import uk.gov.hmrc.vatsignupfrontend.services.mocks.MockGetCompanyNameService

class CaptureRegisteredSocietyCompanyNumberControllerSpec
  extends UnitSpec with GuiceOneAppPerSuite with MockControllerComponents with MockGetCompanyNameService {

  object TestCaptureRegisteredSocietyCompanyNumberController extends CaptureRegisteredSocietyCompanyNumberController(
    mockControllerComponents,
    mockGetCompanyNameService
  )

  override def beforeEach(): Unit = {
    super.beforeEach()
    enable(RegisteredSocietyJourney)
  }

  override def afterEach(): Unit = {
    super.afterEach()
    enable(RegisteredSocietyJourney)
  }

  lazy val testGetRequest = FakeRequest("GET", "/registered-society-company-number")

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

        result.session(request).get(SessionKeys.registeredSocietyCompanyNumberKey) shouldBe Some(testCompanyNumber)
      }
    }
    "company was not found" should {
      "redirect to Registered Society Company Name Not Found page" in {

        mockAuthRetrieveAgentEnrolment()
        mockGetCompanyNameNotFound(testCompanyNumber)

        val request = testPostRequest(testCompanyNumber)

        val result = TestCaptureRegisteredSocietyCompanyNumberController.submit(request)
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.RegisteredSocietyCompanyNameNotFoundController.show().url)

        result.session(request).get(SessionKeys.registeredSocietyCompanyNumberKey) shouldBe None
      }
    }
    "company search failed" should {
      "throw Internal Server Exception" in {

        mockAuthRetrieveAgentEnrolment()
        mockGetCompanyNameFailure(testCompanyNumber)

        val request = testPostRequest(testCompanyNumber)

        intercept[InternalServerException](await(TestCaptureRegisteredSocietyCompanyNumberController.submit(request)))
      }
    }
  }

}
