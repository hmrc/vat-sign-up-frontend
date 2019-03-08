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

package uk.gov.hmrc.vatsignupfrontend.controllers.principal

import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.http.InternalServerException
import uk.gov.hmrc.play.test.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockControllerComponents
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.models.BusinessEntity.BusinessEntitySessionFormatter
import uk.gov.hmrc.vatsignupfrontend.models._
import uk.gov.hmrc.vatsignupfrontend.services.mocks.MockStoreCompanyNumberService

class NoCtEnrolmentSummaryControllerSpec extends UnitSpec with GuiceOneAppPerSuite
  with MockControllerComponents
  with MockStoreCompanyNumberService {

  object TestNoCtEnrolmentSummaryController extends NoCtEnrolmentSummaryController(mockControllerComponents, mockStoreCompanyNumberService)

  def testGetRequest(companyUtr: Option[String] = Some(testCompanyUtr),
                     companyNumber: Option[String] = Some(testCompanyNumber),
                     businessType: Option[BusinessEntity] = Some(SoleTrader)
                    ): FakeRequest[AnyContentAsEmpty.type] =
    FakeRequest("GET", "/check-your-answers-company").withSession(
      SessionKeys.companyUtrKey -> companyUtr.getOrElse(""),
      SessionKeys.companyNumberKey -> companyNumber.getOrElse(""),
      SessionKeys.businessEntityKey -> businessType.map(BusinessEntitySessionFormatter.toString).getOrElse("")
    )

  def testPostRequest(companyUtr: Option[String] = Some(testCompanyUtr),
                      companyNumber: Option[String] = Some(testCompanyNumber),
                      businessType: Option[BusinessEntity] = Some(SoleTrader),
                      vatNumber: Option[String] = Some(testVatNumber)
                     ): FakeRequest[AnyContentAsEmpty.type] =
    FakeRequest("POST", "/check-your-answers-company").withSession(
      SessionKeys.vatNumberKey -> vatNumber.getOrElse(""),
      SessionKeys.companyNumberKey -> companyNumber.getOrElse(""),
      SessionKeys.companyUtrKey -> companyUtr.getOrElse(""),
      SessionKeys.businessEntityKey -> businessType.map(BusinessEntitySessionFormatter.toString).getOrElse("")
    )

  "Calling the show action of the No CT Enrolment controller" when {
    "all prerequisite data are in session" should {
      "go to the Summary page" in {
        mockAuthAdminRole()

        val result = TestNoCtEnrolmentSummaryController.show(testGetRequest())
        status(result) shouldBe Status.OK
        contentType(result) shouldBe Some("text/html")
        charset(result) shouldBe Some("utf-8")
      }
    }
    "company number is missing" should {
      "go to capture company number page" in {
        mockAuthAdminRole()

        val result = TestNoCtEnrolmentSummaryController.show(testGetRequest(companyNumber = None))
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.CaptureCompanyNumberController.show().url)
      }
    }
    "company Utr is missing" should {
      "go to capture vat registration date page" in {
        mockAuthAdminRole()

        val result = TestNoCtEnrolmentSummaryController.show(testGetRequest(companyUtr = None))
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.CaptureCompanyUtrController.show().url)
      }
    }

    "business entity is missing" should {
      "go to business entity page" in {
        mockAuthAdminRole()

        val result = TestNoCtEnrolmentSummaryController.show(testGetRequest(businessType = None))
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.CaptureBusinessEntityController.show().url)
      }
    }
  }

  "Calling the submit action of the No Ct Enrolment Summary controller" when {
    "all prerequisite data are in" when {
      "store company number returned StoreCompanyNumberSuccess" should {
        "goto agree capture email controller" in {
          mockAuthAdminRole()
          mockStoreCompanyNumberSuccess(testVatNumber, testCompanyNumber, Some(testCompanyUtr))

          val result = await(TestNoCtEnrolmentSummaryController.submit(testPostRequest()))
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) should contain(routes.DirectDebitResolverController.show().url)
        }
      }
      "store company number returned StoreCompanyNumberSuccess" should {
        //TODO confirm route
        "goto could not confirm business controller" in {
          mockAuthAdminRole()
          mockStoreCompanyNumberCtMismatch(testVatNumber, testCompanyNumber, testCompanyUtr)

          val result = await(TestNoCtEnrolmentSummaryController.submit(testPostRequest()))
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) should contain(routes.CouldNotConfirmBusinessController.show().url)
        }
      }
      "store company number returned a failure" should {
        "throw internal server exception" in {
          mockAuthAdminRole()
          mockStoreCompanyNumberFailure(testVatNumber, testCompanyNumber, Some(testCompanyUtr))

          intercept[InternalServerException] {
            await(TestNoCtEnrolmentSummaryController.submit(testPostRequest()))
          }

        }
      }
    }
    "vat number is missing" should {
      "go to capture vat number page" in {
        mockAuthAdminRole()

        val result = await(TestNoCtEnrolmentSummaryController.submit(testPostRequest(vatNumber = None)))

        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.CaptureVatNumberController.show().url)
      }
    }
    "company number is missing" should {
      "go to capture company number date page" in {
        mockAuthAdminRole()

        val result = TestNoCtEnrolmentSummaryController.submit(testPostRequest(companyNumber = None))
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.CaptureCompanyNumberController.show().url)
      }
    }
    "company Utr is missing" should {
      "go to capture company utr page" in {
        mockAuthAdminRole()

        val result = TestNoCtEnrolmentSummaryController.submit(testPostRequest(companyUtr = None))
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.CaptureCompanyUtrController.show().url)
      }
    }
  }
}