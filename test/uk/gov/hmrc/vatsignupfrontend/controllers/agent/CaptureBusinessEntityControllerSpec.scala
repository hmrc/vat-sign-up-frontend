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
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockVatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.forms.BusinessEntityForm._
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants.testVatNumber
import uk.gov.hmrc.vatsignupfrontend.httpparsers.StoreOverseasInformationHttpParser.StoreOverseasInformationSuccess
import uk.gov.hmrc.vatsignupfrontend.models.BusinessEntity.BusinessEntitySessionFormatter
import uk.gov.hmrc.vatsignupfrontend.models._
import uk.gov.hmrc.vatsignupfrontend.services.mocks.{MockAdministrativeDivisionLookupService, MockStoreOverseasInformationService}
import uk.gov.hmrc.vatsignupfrontend.utils.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.views.html.agent.capture_business_entity

import scala.concurrent.Future


class CaptureBusinessEntityControllerSpec extends UnitSpec with GuiceOneAppPerSuite
  with MockVatControllerComponents with MockAdministrativeDivisionLookupService
  with MockStoreOverseasInformationService {

  val view = app.injector.instanceOf[capture_business_entity]

  object TestCaptureBusinessEntityController extends CaptureBusinessEntityController(mockAdministrativeDivisionLookupService, view)

  implicit val testGetRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/business-type")

  def testPostRequest(entityTypeVal: String): FakeRequest[AnyContentAsFormUrlEncoded] =
    FakeRequest("POST", "/business-type").withFormUrlEncodedBody(businessEntity -> entityTypeVal)

  "Calling the show action of the Capture Entity Type controller" should {
    "redirect to capture email page" when {
      "VRN is an Administrative Divisions" in {
        mockAuthRetrieveAgentEnrolment()
        mockIsAdministrativeDivision(testVatNumber)(isAdministrativeDivision = true)

        val result = TestCaptureBusinessEntityController.show()(testGetRequest.withSession(SessionKeys.vatNumberKey -> testVatNumber))
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.DivisionResolverController.resolve().url)
        session(result).get(SessionKeys.businessEntityKey) should contain(BusinessEntitySessionFormatter.toString(Division))
      }
    }

    "go to the Capture Entity Type page" in {
      mockAuthRetrieveAgentEnrolment()
      mockIsAdministrativeDivision(testVatNumber)(isAdministrativeDivision = false)


      val result = TestCaptureBusinessEntityController.show()(testGetRequest.withSession(SessionKeys.vatNumberKey -> testVatNumber))
      status(result) shouldBe Status.OK
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
    }

    "redirect to capture email page" when {
      "No session credentials" in {
        mockAuthRetrieveAgentEnrolment()

        val result = TestCaptureBusinessEntityController.show()(testGetRequest)
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.CaptureVatNumberController.show().url)
      }
    }

    "redirect to overseas resolver controller" when {
      "the business entity is Overseas" in {
        mockAuthRetrieveAgentEnrolment()
        mockStoreOverseasInformation(testVatNumber)(Future.successful(Right(StoreOverseasInformationSuccess)))

        val result = TestCaptureBusinessEntityController.show()(testGetRequest.withSession(
          SessionKeys.vatNumberKey -> testVatNumber,
          SessionKeys.businessEntityKey -> Overseas.toString
        ))

        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.OverseasResolverController.resolve().url)
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

          session(result).get(SessionKeys.businessEntityKey) should contain(BusinessEntitySessionFormatter.toString(LimitedCompany))
        }
      }

      "go to the capture NINO page" when {
        "the business entity is sole trader" in {
          mockAuthRetrieveAgentEnrolment()

          val result = TestCaptureBusinessEntityController.submit(testPostRequest(soleTrader))
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(soletrader.routes.CaptureNinoController.show().url)

          session(result).get(SessionKeys.businessEntityKey) should contain(BusinessEntitySessionFormatter.toString(SoleTrader))
        }
      }

      "redirect to resolve partnership" when {
        "the business entity is general partnership" in {
          mockAuthRetrieveAgentEnrolment()
          val result = TestCaptureBusinessEntityController.submit(testPostRequest(generalPartnership))
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(partnerships.routes.ResolvePartnershipController.resolve().url)

          session(result).get(SessionKeys.businessEntityKey) should contain(BusinessEntitySessionFormatter.toString(GeneralPartnership))
        }
      }

      "redirect to resolve partnership" when {
        "the business entity is limited partnership" in {
          mockAuthRetrieveAgentEnrolment()

          val result = TestCaptureBusinessEntityController.submit(testPostRequest(limitedPartnership))
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(partnerships.routes.ResolvePartnershipController.resolve().url)

          session(result).get(SessionKeys.businessEntityKey) should contain(BusinessEntitySessionFormatter.toString(LimitedPartnership))

        }
      }

      "go to business entity other page" when {
        "the business entity is other" in {
          mockAuthRetrieveAgentEnrolment()

          val result = TestCaptureBusinessEntityController.submit(testPostRequest(other))
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.CaptureBusinessEntityOtherController.show().url)
          session(result).get(SessionKeys.businessEntityKey) should contain(BusinessEntitySessionFormatter.toString(Other))
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
