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
import uk.gov.hmrc.play.test.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch._
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockControllerComponents
import uk.gov.hmrc.vatsignupfrontend.forms.BusinessEntityForm._
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants.testVatNumber
import uk.gov.hmrc.vatsignupfrontend.models.BusinessEntity.BusinessEntitySessionFormatter
import uk.gov.hmrc.vatsignupfrontend.models._
import uk.gov.hmrc.vatsignupfrontend.services.mocks.MockAdministrativeDivisionLookupService


class CaptureBusinessEntityControllerSpec extends UnitSpec with GuiceOneAppPerSuite with MockControllerComponents with MockAdministrativeDivisionLookupService {

  object TestCaptureBusinessEntityController extends CaptureBusinessEntityController(
    mockControllerComponents,
    mockAdministrativeDivisionLookupService
  )

  implicit val testGetRequest = FakeRequest("GET", "/business-type")

  def testPostRequest(entityTypeVal: String): FakeRequest[AnyContentAsFormUrlEncoded] =
    FakeRequest("POST", "/business-type").withFormUrlEncodedBody(businessEntity -> entityTypeVal)

  "Calling the show action of the Capture Entity Type controller" should {
    "redirect to capture email page" when {
      "VRN is an Administrative Divisions" in {
        mockAuthRetrieveAgentEnrolment()
        mockIsAdministrativeDivision(testVatNumber)(true)

        val result = TestCaptureBusinessEntityController.show(testGetRequest.withSession(SessionKeys.vatNumberKey -> testVatNumber))
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.CaptureAgentEmailController.show().url)
      }
    }

    "go to the Capture Entity Type page" in {
      mockAuthRetrieveAgentEnrolment()
      mockIsAdministrativeDivision(testVatNumber)(false)


      val result = TestCaptureBusinessEntityController.show(testGetRequest.withSession(SessionKeys.vatNumberKey -> testVatNumber))
      status(result) shouldBe Status.OK
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
    }

    "redirect to capture email page" when {
      "No session credentials" in {
        mockAuthRetrieveAgentEnrolment()

        val result = TestCaptureBusinessEntityController.show(testGetRequest)
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.CaptureVatNumberController.show().url)
      }
    }
  }


  "Calling the submit action of the Capture Business Entity controller" when {
    "form successfully submitted" should {

      "go to the capture company number page" when {
        "the business entity is limited company" in {

          mockAuthRetrieveAgentEnrolment()

          val result = TestCaptureBusinessEntityController.submit(testPostRequest(limitedCompany))
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.CaptureCompanyNumberController.show().url)

          result.session get SessionKeys.businessEntityKey should contain(BusinessEntitySessionFormatter.toString(LimitedCompany))
        }
      }

      "go to the capture NINO page" when {
        "the business entity is sole trader and the skip CID check feature switch is enabled" in {
          enable(SkipCidCheck)
          mockAuthRetrieveAgentEnrolment()

          val result = TestCaptureBusinessEntityController.submit(testPostRequest(soleTrader))
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(soletrader.routes.CaptureNinoController.show().url)

          result.session get SessionKeys.businessEntityKey should contain(BusinessEntitySessionFormatter.toString(SoleTrader))
        }
      }

      "go to the capture client details page" when {
        "the business entity is sole trader" in {
          mockAuthRetrieveAgentEnrolment()

          val result = TestCaptureBusinessEntityController.submit(testPostRequest(soleTrader))
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.CaptureClientDetailsController.show().url)

          result.session get SessionKeys.businessEntityKey should contain(BusinessEntitySessionFormatter.toString(SoleTrader))
        }
      }

      "redirect to resolve partnership" when {
        "the business entity is general partnership" in {
          mockAuthRetrieveAgentEnrolment()
          val result = TestCaptureBusinessEntityController.submit(testPostRequest(generalPartnership))
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(partnerships.routes.ResolvePartnershipController.resolve().url)

          result.session get SessionKeys.businessEntityKey should contain(BusinessEntitySessionFormatter.toString(GeneralPartnership))
        }
      }

      "redirect to resolve partnership when the joint venture or property feature switch" when {
        "the business entity is general partnership" in {
          mockAuthRetrieveAgentEnrolment()
          enable(OptionalSautrJourney)

          val result = TestCaptureBusinessEntityController.submit(testPostRequest(generalPartnership))
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(partnerships.routes.ResolvePartnershipController.resolve().url)

          result.session get SessionKeys.businessEntityKey should contain(BusinessEntitySessionFormatter.toString(GeneralPartnership))
        }
      }

      "redirect to resolve partnership" when {
        "the business entity is limited partnership" in {
          mockAuthRetrieveAgentEnrolment()

          val result = TestCaptureBusinessEntityController.submit(testPostRequest(limitedPartnership))
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(partnerships.routes.ResolvePartnershipController.resolve().url)

          result.session get SessionKeys.businessEntityKey should contain(BusinessEntitySessionFormatter.toString(LimitedPartnership))

        }
      }

      "go to business entity other page" when {
        "the business entity is other" in {
          mockAuthRetrieveAgentEnrolment()

          val result = TestCaptureBusinessEntityController.submit(testPostRequest(other))
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.CaptureBusinessEntityOtherController.show().url)
          result.session get SessionKeys.businessEntityKey should contain(BusinessEntitySessionFormatter.toString(Other))
        }
      }
    }


    "form unsuccessfully submitted" should {
      "reload the page with errors" in {
        mockAuthRetrieveAgentEnrolment()

        val result = TestCaptureBusinessEntityController.submit(testPostRequest("invalid"))
        status(result) shouldBe Status.BAD_REQUEST
        contentType(result) shouldBe Some("text/html")
        charset(result) shouldBe Some("utf-8")
      }
    }

  }

}
