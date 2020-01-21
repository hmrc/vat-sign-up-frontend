/*
 * Copyright 2020 HM Revenue & Customs
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
import play.api.mvc.AnyContentAsFormUrlEncoded
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.http.{BadGatewayException, InternalServerException}
import uk.gov.hmrc.play.test.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockControllerComponents
import uk.gov.hmrc.vatsignupfrontend.controllers.principal.error.{routes => errorRoutes}
import uk.gov.hmrc.vatsignupfrontend.controllers.principal.{routes => principalRoutes}
import uk.gov.hmrc.vatsignupfrontend.forms.ConfirmGeneralPartnershipForm.confirmPartnershipForm
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants.{testSaUtr, testVatNumber}
import uk.gov.hmrc.vatsignupfrontend.httpparsers.StorePartnershipInformationHttpParser._
import uk.gov.hmrc.vatsignupfrontend.models.{No, Yes, YesNo}
import uk.gov.hmrc.vatsignupfrontend.services.mocks.MockStorePartnershipInformationService

import scala.concurrent.Future


class ConfirmGeneralPartnershipControllerSpec extends UnitSpec with GuiceOneAppPerSuite with MockControllerComponents
  with MockStorePartnershipInformationService {

  object TestConfirmGeneralPartnershipController extends ConfirmGeneralPartnershipController(
    mockControllerComponents,
    mockStorePartnershipInformationService
  )

  val testGetRequest = FakeRequest("GET", "/confirm-partnership-utr")

  def testPostRequest(answer: YesNo = Yes): FakeRequest[AnyContentAsFormUrlEncoded] =
    FakeRequest("POST", "/confirm-partnership-utr").withFormUrlEncodedBody(confirmPartnershipForm.fill(answer).data.toSeq: _*)

  "Calling the show action of the Confirm General Partnership controller" when {
    "there is a company number in the session" should {
      "go to the Confirm Company Number page" in {
        mockAuthAdminRole()
        val request = testGetRequest.withSession(
          SessionKeys.vatNumberKey -> testVatNumber,
          SessionKeys.partnershipSautrKey -> testSaUtr
        )

        val result = TestConfirmGeneralPartnershipController.show(request)
        status(result) shouldBe Status.OK
        contentType(result) shouldBe Some("text/html")
        charset(result) shouldBe Some("utf-8")
      }
    }
    "there isn't a vat number in the session" should {
      "go to the your vat number page" in {
        mockAuthAdminRole()

        val result = TestConfirmGeneralPartnershipController.show(testGetRequest)
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(principalRoutes.ResolveVatNumberController.resolve().url)
      }
    }
    "there isn't a partnership sautr in the session" should {
      "show technical difficulties" in {
        mockAuthAdminRole()

        intercept[InternalServerException] {
          await(TestConfirmGeneralPartnershipController.submit(testPostRequest().withSession(
            SessionKeys.vatNumberKey -> testVatNumber
          )))
        }
      }
    }
  }

  "Calling the submit action of the Confirm General Partnership controller" when {
    "vat number is in session" when {
      "User answered Yes" should {
        "go to the 'agree to receive emails' page" in {
          mockAuthAdminRole()
          mockStorePartnershipInformation(
            vatNumber = testVatNumber,
            sautr = Some(testSaUtr),
            postCode = None
          )(Future.successful(Right(StorePartnershipInformationSuccess)))

          val result = TestConfirmGeneralPartnershipController.submit(testPostRequest().withSession(
            SessionKeys.vatNumberKey -> testVatNumber,
            SessionKeys.partnershipSautrKey -> testSaUtr
          ))
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(principalRoutes.DirectDebitResolverController.show().url)
        }
      }
      "User answered No" should {
        "go to 'sign in with different details partnership' page" in {
          mockAuthAdminRole()

          val result = TestConfirmGeneralPartnershipController.submit(testPostRequest(No).withSession(
            SessionKeys.vatNumberKey -> testVatNumber,
            SessionKeys.partnershipSautrKey -> testSaUtr
          ))

          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(errorRoutes.SignInWithDifferentDetailsPartnershipController.show().url)
        }
      }
    }
  }
  "vat number is in session but store partnership information is unsuccessful" should {
    "throw bad gateway exception" in {
      mockAuthAdminRole()
      mockStorePartnershipInformation(
        vatNumber = testVatNumber,
        sautr = Some(testSaUtr),
        postCode = None
      )(Future.successful(Left(StorePartnershipInformationFailureResponse(BAD_REQUEST))))

      intercept[BadGatewayException] {
        await(TestConfirmGeneralPartnershipController.submit(testPostRequest().withSession(
          SessionKeys.vatNumberKey -> testVatNumber,
          SessionKeys.partnershipSautrKey -> testSaUtr
        )))
      }
    }
  }
  "vat number is not in session" should {
    "redirect to your vat number" in {
      mockAuthAdminRole()

      val result = TestConfirmGeneralPartnershipController.submit(testPostRequest())
      status(result) shouldBe Status.SEE_OTHER
      redirectLocation(result) shouldBe Some(principalRoutes.ResolveVatNumberController.resolve().url)
    }
  }
  "partnership sautr is not in session" should {
    "show technical difficulties" in {
      mockAuthAdminRole()

      intercept[InternalServerException] {
        await(TestConfirmGeneralPartnershipController.submit(testPostRequest().withSession(
          SessionKeys.vatNumberKey -> testVatNumber
        )))
      }
    }
  }
}