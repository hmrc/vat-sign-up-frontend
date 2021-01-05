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

package uk.gov.hmrc.vatsignupfrontend.controllers.principal.partnerships

import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.mvc.{AnyContentAsEmpty, AnyContentAsFormUrlEncoded}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.http.InternalServerException
import uk.gov.hmrc.vatsignupfrontend.utils.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockVatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.controllers.principal.error.{routes => errorRoutes}
import uk.gov.hmrc.vatsignupfrontend.forms.CompanyNumberForm._
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.httpparsers.GetCompanyNameHttpParser.CompanyClosed
import uk.gov.hmrc.vatsignupfrontend.models.companieshouse
import uk.gov.hmrc.vatsignupfrontend.models.companieshouse.NonPartnershipEntity
import uk.gov.hmrc.vatsignupfrontend.services.mocks.MockGetCompanyNameService

import scala.concurrent.Future

class CapturePartnershipCompanyNumberControllerSpec extends UnitSpec with GuiceOneAppPerSuite with MockVatControllerComponents
  with MockGetCompanyNameService {

  object TestCaptureCompanyNumberController extends CapturePartnershipCompanyNumberController(mockGetCompanyNameService)

  lazy val testGetRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/partnership-company-number")

  def testPostRequest(companyNumberVal: String): FakeRequest[AnyContentAsFormUrlEncoded] =
    FakeRequest("POST", "/partnership-company-number").withFormUrlEncodedBody(companyNumber -> companyNumberVal)


  "Calling the show action of the Capture Partnership Company Number controller" should {
    "go to the Capture Partnership Company Number page" in {
      mockAuthAdminRole()

      val result = TestCaptureCompanyNumberController.show(testGetRequest)
      status(result) shouldBe Status.OK
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
    }
  }

  "Calling the submit action of the Capture Partnership Company Number controller" when {
    "get company name returned successfully" should {
      "Redirect to Confirm Partnership page" in {
        mockAuthAdminRole()

        mockGetCompanyNameSuccess(testCompanyNumber, companieshouse.LimitedPartnership)

        val request = testPostRequest(testCompanyNumber)

        val result = TestCaptureCompanyNumberController.submit(request)
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.ConfirmPartnershipController.show().url)
        session(result) get SessionKeys.partnershipTypeKey should contain(testPartnershipType)
        session(result) get SessionKeys.companyNumberKey should contain(testCompanyNumber)
        session(result) get SessionKeys.companyNameKey should contain(testCompanyName)
      }
    }

    "form unsuccessfully submitted" should {
      "reload the page with errors" in {
        mockAuthAdminRole()

        val result = TestCaptureCompanyNumberController.submit(testPostRequest("123456789"))
        status(result) shouldBe Status.BAD_REQUEST
        contentType(result) shouldBe Some("text/html")
        charset(result) shouldBe Some("utf-8")
      }
    }

    "company number failed prefix validation" should {
      "redirect to Could Not Confirm Company page" in {
        mockAuthAdminRole()

        val testCrn = "BR12345"
        val request = testPostRequest(testCrn)

        val result = TestCaptureCompanyNumberController.submit(request)

        status(result) shouldBe SEE_OTHER
        redirectLocation(result) shouldBe Some(errorRoutes.CouldNotConfirmCompanyController.show().url)
      }
    }

    "get company name returned not found" should {
      "redirect to Could Not Confirm Company page" in {
        mockAuthAdminRole()

        mockGetCompanyNameNotFound(testCompanyNumber)

        val request = testPostRequest(testCompanyNumber)

        val result = TestCaptureCompanyNumberController.submit(request)

        status(result) shouldBe SEE_OTHER
        redirectLocation(result) shouldBe Some(errorRoutes.CouldNotConfirmCompanyController.show().url)
      }
    }
    "get company name succeeds when non partnership entity entered" should {
      "redirect to Could Not Confirm limited Partnership page" in {
        mockAuthAdminRole()

        mockGetCompanyNameSuccess(testCompanyNumber, NonPartnershipEntity)

        val request = testPostRequest(testCompanyNumber)

        val result = TestCaptureCompanyNumberController.submit(request)

        status(result) shouldBe SEE_OTHER
        redirectLocation(result) shouldBe Some(errorRoutes.CouldNotConfirmLimitedPartnershipController.show().url)
      }
    }
    "get company name returns company closed" should {
      "redirect to dissolved company page" in {
        mockAuthAdminRole()

        mockGetCompanyName(testCompanyNumber)(Future.successful(Right(CompanyClosed(testCompanyName))))

        val request = testPostRequest(testCompanyNumber)

        val result = TestCaptureCompanyNumberController.submit(request)

        status(result) shouldBe SEE_OTHER
        redirectLocation(result) shouldBe Some(errorRoutes.DissolvedCompanyController.show().url)

        session(result).get(SessionKeys.companyNameKey) shouldBe Some(testCompanyName)
      }
    }

    "get company name fails" should {
      "throw an InternalServerException" in {
        mockAuthAdminRole()

        mockGetCompanyNameFailure(testCompanyNumber)

        val request = testPostRequest(testCompanyNumber)

        val result = TestCaptureCompanyNumberController.submit(request)

        intercept[InternalServerException](result)
      }
    }
  }

}
