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

package uk.gov.hmrc.vatsignupfrontend.controllers.principal

import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.mvc.{AnyContentAsEmpty, AnyContentAsFormUrlEncoded}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockVatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.forms.BusinessEntityForm._
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.httpparsers.StoreOverseasInformationHttpParser.StoreOverseasInformationSuccess
import uk.gov.hmrc.vatsignupfrontend.models.BusinessEntity.BusinessEntitySessionFormatter
import uk.gov.hmrc.vatsignupfrontend.models._
import uk.gov.hmrc.vatsignupfrontend.services.mocks.{MockAdministrativeDivisionLookupService, MockStoreOverseasInformationService}
import uk.gov.hmrc.vatsignupfrontend.utils.UnitSpec

import scala.concurrent.Future
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.capture_business_entity

class CaptureBusinessEntityControllerSpec extends UnitSpec with GuiceOneAppPerSuite
  with MockVatControllerComponents with MockAdministrativeDivisionLookupService
  with MockStoreOverseasInformationService {

  val view = app.injector.instanceOf[capture_business_entity]

  object TestCaptureBusinessEntityController extends CaptureBusinessEntityController(
    mockStoreOverseasInformationService,
    mockAdministrativeDivisionLookupService,
    view
  )

  lazy val testGetRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/business-type").withSession(SessionKeys.vatNumberKey -> testVatNumber)

  def testPostRequest(entityTypeVal: String): FakeRequest[AnyContentAsFormUrlEncoded] =
    FakeRequest("POST", "/business-type").withFormUrlEncodedBody(businessEntity -> entityTypeVal)

  "Calling the show action of the Capture Entity Type controller" when {
    "the VRN belongs to a VAT division" should {
      "redirect the user to the DivisionResolverController" in {
        mockAuthAdminRole()
        mockIsAdministrativeDivision(testVatNumber)(isAdministrativeDivision = true)

        val result = TestCaptureBusinessEntityController.show(testGetRequest)

        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.DivisionResolverController.resolve().url)
      }
    }

    "the VRN does not belong to a VAT division" should {
      "go to the Capture Entity Type page" in {
        mockAuthAdminRole()
        mockIsAdministrativeDivision(testVatNumber)(isAdministrativeDivision = false)

        val result = TestCaptureBusinessEntityController.show(testGetRequest)
        status(result) shouldBe Status.OK
        contentType(result) shouldBe Some("text/html")
        charset(result) shouldBe Some("utf-8")
      }
    }

    "the VRN is overseas" should {
      "redirect to overseas resolver url" in {
        mockAuthAdminRole()
        mockStoreOverseasInformation(testVatNumber)(Future.successful(Right(StoreOverseasInformationSuccess)))

        implicit lazy val testGetRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/business-type")
          .withSession(SessionKeys.vatNumberKey -> testVatNumber)
          .withSession(SessionKeys.businessEntityKey -> Overseas.toString)

        val futureResult = TestCaptureBusinessEntityController.show(testGetRequest)
        val result = await(futureResult)

        status(futureResult) shouldBe Status.SEE_OTHER
        redirectLocation(futureResult) shouldBe Some(routes.OverseasResolverController.resolve().url)
        result.session.get(SessionKeys.businessEntityKey) should contain(BusinessEntitySessionFormatter.toString(Overseas))
      }
    }
  }

  "Calling the submit action of the Capture Business Entity controller" when {
    "form successfully submitted" when {
      "the business entity is sole trader" should {
        "go to sole trader resolver with sole trader stored in session" in {
          mockAuthAdminRole()
          implicit val request: FakeRequest[AnyContentAsFormUrlEncoded] = testPostRequest(soleTrader)

          val result = TestCaptureBusinessEntityController.submit(request)

          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) should contain(soletrader.routes.SoleTraderResolverController.resolve().url)

          session(result).get(SessionKeys.businessEntityKey) should contain(BusinessEntitySessionFormatter.toString(SoleTrader))
        }
      }

      "the business entity is limited company" should {
        "go to capture company number controller" in {
          mockAuthAdminRole()
          implicit val request: FakeRequest[AnyContentAsFormUrlEncoded] = testPostRequest(limitedCompany)

          val result = TestCaptureBusinessEntityController.submit(request)

          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) should contain(routes.CaptureCompanyNumberController.show().url)

          session(result).get(SessionKeys.businessEntityKey) should contain(BusinessEntitySessionFormatter.toString(LimitedCompany))
        }
      }

      "the business entity is general partnership" should {
        "go to resolve partnership utr controller" in {
          mockAuthAdminRole()
          implicit val request: FakeRequest[AnyContentAsFormUrlEncoded] = testPostRequest(generalPartnership)

          val result = TestCaptureBusinessEntityController.submit(request)

          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) should contain(partnerships.routes.ResolvePartnershipUtrController.resolve().url)

          session(result).get(SessionKeys.businessEntityKey) should contain(BusinessEntitySessionFormatter.toString(GeneralPartnership))
        }
      }

      "the business entity is limited partnership" should {
        "go to capture partnership company number controller" in {
          mockAuthAdminRole()
          implicit val request: FakeRequest[AnyContentAsFormUrlEncoded] = testPostRequest(limitedPartnership)

          val result = TestCaptureBusinessEntityController.submit(request)

          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) should contain(partnerships.routes.CapturePartnershipCompanyNumberController.show().url)

          session(result).get(SessionKeys.businessEntityKey) should contain(BusinessEntitySessionFormatter.toString(LimitedPartnership))
        }
      }

      "the business entity is other" should {
        "go to other what type of business are you page" in {
          mockAuthAdminRole()

          implicit val request: FakeRequest[AnyContentAsFormUrlEncoded] = testPostRequest(other)

          val result = TestCaptureBusinessEntityController.submit(request)

          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) should contain(routes.CaptureBusinessEntityOtherController.show().url)

          session(result).get(SessionKeys.businessEntityKey) should contain(BusinessEntitySessionFormatter.toString(Other))
        }
      }

    }

    "form unsuccessfully submitted" should {
      "reload the page with errors" in {
        mockAuthAdminRole()

        val result = TestCaptureBusinessEntityController.submit(testPostRequest("invalid"))
        status(result) shouldBe Status.BAD_REQUEST
        contentType(result) shouldBe Some("text/html")
        charset(result) shouldBe Some("utf-8")
      }
    }

  }

}
