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
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.http.InternalServerException
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockVatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.controllers.principal.error.{routes => errorRoutes}
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.models.BusinessEntity.BusinessEntitySessionFormatter
import uk.gov.hmrc.vatsignupfrontend.models._
import uk.gov.hmrc.vatsignupfrontend.services.mocks.MockStoreRegisteredSocietyService
import uk.gov.hmrc.vatsignupfrontend.utils.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.check_your_answers_registered_society

class RegisteredSocietyCheckYourAnswersControllerSpec extends UnitSpec with GuiceOneAppPerSuite
  with MockVatControllerComponents
  with MockStoreRegisteredSocietyService {

  val view = app.injector.instanceOf[check_your_answers_registered_society]

  object TestRegisteredSocietyController extends RegisteredSocietyCheckYourAnswersController(mockStoreRegisteredSocietyService, view)

  def testGetRequest(companyUtr: Option[String] = Some(testCompanyUtr),
                     companyNumber: Option[String] = Some(testCompanyNumber),
                     businessType: Option[BusinessEntity] = Some(RegisteredSociety)
                    ): FakeRequest[AnyContentAsEmpty.type] =
    FakeRequest("GET", "/check-your-answers-registered-society").withSession(
      SessionKeys.registeredSocietyUtrKey -> companyUtr.getOrElse(""),
      SessionKeys.registeredSocietyCompanyNumberKey -> companyNumber.getOrElse(""),
      SessionKeys.businessEntityKey -> businessType.map(BusinessEntitySessionFormatter.toString).getOrElse("")
    )

  def testPostRequest(companyUtr: Option[String] = Some(testCompanyUtr),
                      companyNumber: Option[String] = Some(testCompanyNumber),
                      businessType: Option[BusinessEntity] = Some(RegisteredSociety),
                      vatNumber: Option[String] = Some(testVatNumber)
                     ): FakeRequest[AnyContentAsEmpty.type] =
    FakeRequest("POST", "/check-your-answers-registered-society").withSession(
      SessionKeys.vatNumberKey -> vatNumber.getOrElse(""),
      SessionKeys.registeredSocietyCompanyNumberKey -> companyNumber.getOrElse(""),
      SessionKeys.registeredSocietyUtrKey -> companyUtr.getOrElse(""),
      SessionKeys.businessEntityKey -> businessType.map(BusinessEntitySessionFormatter.toString).getOrElse("")
    )

  "Calling the show action of the Check your answers registered society controller" when {
    "all prerequisite data are in session" should {
      "go to the CYA page" in {
        mockAuthAdminRole()

        val result = TestRegisteredSocietyController.show(testGetRequest())
        status(result) shouldBe Status.OK
        contentType(result) shouldBe Some("text/html")
        charset(result) shouldBe Some("utf-8")
      }
    }
    "company number is missing" should {
      "go to capture registered society company number page" in {
        mockAuthAdminRole()

        val result = TestRegisteredSocietyController.show(testGetRequest(
          companyNumber = None
        ))
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.CaptureRegisteredSocietyCompanyNumberController.show().url)
      }
    }
    "company Utr is missing" should {
      "go to capture registered society utr page" in {
        mockAuthAdminRole()

        val result = TestRegisteredSocietyController.show(testGetRequest(companyUtr = None))
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.CaptureRegisteredSocietyUtrController.show().url)
      }
    }

    "business entity is missing" should {
      "go to business entity page" in {
        mockAuthAdminRole()

        val result = TestRegisteredSocietyController.show(testGetRequest(businessType = None))
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.CaptureBusinessEntityController.show().url)
      }
    }
  }

  "Calling the submit action of the Registered society check your answers controller" when {
    "all prerequisite data are in" when {
      "store registered society returned StoreRegisteredSocietySuccess" should {
        "goto agree capture email controller" in {
          mockAuthAdminRole()
          mockStoreRegisteredSocietySuccess(testVatNumber, testCompanyNumber, Some(testCompanyUtr))

          val result = TestRegisteredSocietyController.submit(testPostRequest())
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) should contain(routes.DirectDebitResolverController.show().url)
        }
      }
      "store company number returned StoreCompanyNumberSuccess" should {
        "goto could not confirm business controller" in {
          mockAuthAdminRole()
          mockStoreRegisteredSocietyCtMismatch(testVatNumber, testCompanyNumber, Some(testCompanyUtr))

          val result = TestRegisteredSocietyController.submit(testPostRequest())
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) should contain(errorRoutes.CouldNotConfirmBusinessController.show().url)
        }
      }
      "store registered society number returned a failure" should {
        "throw internal server exception" in {
          mockAuthAdminRole()
          mockStoreRegisteredSocietyFailure(testVatNumber, testCompanyNumber, Some(testCompanyUtr))

          intercept[InternalServerException] {
            TestRegisteredSocietyController.submit(testPostRequest())
          }

        }
      }
    }
    "vat number is missing" should {
      "go to capture vat number page" in {
        mockAuthAdminRole()

        val result = TestRegisteredSocietyController.submit(testPostRequest(vatNumber = None))

        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.CaptureVatNumberController.show().url)
      }
    }
    "company number is missing" should {
      "go to capture company number date page" in {
        mockAuthAdminRole()

        val result = TestRegisteredSocietyController.submit(testPostRequest(companyNumber = None))
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.CaptureRegisteredSocietyCompanyNumberController.show().url)
      }
    }
    "company Utr is missing" should {
      "go to capture company utr page" in {
        mockAuthAdminRole()

        val result = TestRegisteredSocietyController.submit(testPostRequest(companyUtr = None))
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.CaptureRegisteredSocietyUtrController.show().url)
      }
    }
  }
}
