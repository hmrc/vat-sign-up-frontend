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

import java.time.LocalDate

import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.libs.json.Json
import play.api.mvc.AnyContentAsFormUrlEncoded
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.play.test.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch.{AdditionalKnownFacts, FeatureSwitching}
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockControllerComponents
import uk.gov.hmrc.vatsignupfrontend.forms.VatRegistrationDateForm._
import uk.gov.hmrc.vatsignupfrontend.models.{DateModel, Overseas}

class CaptureVatRegistrationDateControllerSpec extends UnitSpec with GuiceOneAppPerSuite
  with MockControllerComponents with FeatureSwitching {

  object TestCaptureVatNumberController extends CaptureVatRegistrationDateController(mockControllerComponents)

  lazy val testGetRequest = FakeRequest("GET", "/vat-registration-date")

  def testPostRequest(registrationDate: DateModel): FakeRequest[AnyContentAsFormUrlEncoded] =
    FakeRequest("POST", "/vat-registration-date")
      .withFormUrlEncodedBody(vatRegistrationDate + ".dateDay" -> registrationDate.day,
        vatRegistrationDate + ".dateMonth" -> registrationDate.month,
        vatRegistrationDate + ".dateYear" -> registrationDate.year
      )


  "Calling the show action of the Capture Vat Registration Date controller" should {
    "go to the Capture Vat Registration Date page" in {
      mockAuthAdminRole()

      val result = TestCaptureVatNumberController.show(testGetRequest)
      status(result) shouldBe Status.OK
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
    }
  }


  "Calling the submit action of the Capture Vat Registration Date controller" when {
    "form successfully submitted with Overseas business entity not in session" should {
      "redirect to the Business Postcode page" in {
        mockAuthAdminRole()

        val yesterday = DateModel.dateConvert(LocalDate.now().minusDays(1))
        val request = testPostRequest(yesterday)

        val result = TestCaptureVatNumberController.submit(request)
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.BusinessPostCodeController.show().url)

        Json.parse(result.session(request).get(SessionKeys.vatRegistrationDateKey).get).validate[DateModel].get shouldBe yesterday
      }
     }
    "form successfully submitted with Overseas business entity in session" should {
      "redirect to the Previous Vat return page" when {
        "the additional known facts feature switch is enabled" in {
          mockAuthAdminRole()
          enable(AdditionalKnownFacts)

          def testPostRequest(registrationDate: DateModel, entity: String): FakeRequest[AnyContentAsFormUrlEncoded] =
            FakeRequest("POST", "/vat-registration-date")
              .withFormUrlEncodedBody(vatRegistrationDate + ".dateDay" -> registrationDate.day,
                vatRegistrationDate + ".dateMonth" -> registrationDate.month,
                vatRegistrationDate + ".dateYear" -> registrationDate.year
              ).withSession(SessionKeys.businessEntityKey -> entity)

          val yesterday = DateModel.dateConvert(LocalDate.now().minusDays(1))
          val request = testPostRequest(yesterday, Overseas.toString)

          val result = TestCaptureVatNumberController.submit(request)
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.PreviousVatReturnController.show().url)

          Json.parse(result.session(request).get(SessionKeys.vatRegistrationDateKey).get).validate[DateModel].get shouldBe yesterday
          result.session(request).get(SessionKeys.businessEntityKey).get shouldBe Overseas.toString
        }
      }
      "redirect to the check your answers page" when {
        "the additional known facts feature switch is not enabled" in {
          mockAuthAdminRole()
          disable(AdditionalKnownFacts)

          def testPostRequest(registrationDate: DateModel, entity: String): FakeRequest[AnyContentAsFormUrlEncoded] =
            FakeRequest("POST", "/vat-registration-date")
              .withFormUrlEncodedBody(vatRegistrationDate + ".dateDay" -> registrationDate.day,
                vatRegistrationDate + ".dateMonth" -> registrationDate.month,
                vatRegistrationDate + ".dateYear" -> registrationDate.year
              ).withSession(SessionKeys.businessEntityKey -> entity)

          val yesterday = DateModel.dateConvert(LocalDate.now().minusDays(1))
          val request = testPostRequest(yesterday, Overseas.toString)

          val result = TestCaptureVatNumberController.submit(request)
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.CheckYourAnswersController.show().url)

          Json.parse(result.session(request).get(SessionKeys.vatRegistrationDateKey).get).validate[DateModel].get shouldBe yesterday
          result.session(request).get(SessionKeys.businessEntityKey).get shouldBe Overseas.toString
        }
      }
    }

    "form unsuccessfully submitted" should {
      "reload the page with errors" in {
        mockAuthAdminRole()

        val invalidRequest = FakeRequest("POST", "/vat-registration-date")
          .withFormUrlEncodedBody("" -> "")

        val result = TestCaptureVatNumberController.submit(invalidRequest)
        status(result) shouldBe Status.BAD_REQUEST
        contentType(result) shouldBe Some("text/html")
        charset(result) shouldBe Some("utf-8")
      }
    }
  }
}