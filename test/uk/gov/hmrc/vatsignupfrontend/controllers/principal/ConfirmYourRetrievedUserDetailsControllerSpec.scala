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
import play.api.libs.json.Json
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.http.InternalServerException
import uk.gov.hmrc.play.test.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockControllerComponents
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.models.{AuthProfile, IRSA}
import uk.gov.hmrc.vatsignupfrontend.services.mocks.MockStoreNinoService

class ConfirmYourRetrievedUserDetailsControllerSpec extends UnitSpec with GuiceOneAppPerSuite with MockControllerComponents with MockStoreNinoService {

  object TestConfirmYourRetrievedUserDetailsController extends ConfirmYourRetrievedUserDetailsController(mockControllerComponents, mockStoreNinoService)

  lazy val testGetRequest = FakeRequest("GET", "/confirm-your-details")

  lazy val testPostRequest: FakeRequest[AnyContentAsEmpty.type] =
    FakeRequest("POST", "/confirm-your-details")


  "Calling the show action of the Confirm Your Retrieved User Details controller" should {
    "show the confirm your retrieved user details page when user details are in session" in {
      mockAuthAdminRole()
      val request = testGetRequest.withSession(SessionKeys.userDetailsKey -> testUserDetailsJson)

      val result = TestConfirmYourRetrievedUserDetailsController.show(request)
      status(result) shouldBe Status.OK
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
    }

    "redirect to the capture business entity page when user details are not in session" in {
      mockAuthAdminRole()
      val request = testGetRequest

      val result = TestConfirmYourRetrievedUserDetailsController.show(request)
      status(result) shouldBe Status.SEE_OTHER
      redirectLocation(result) should contain(routes.CaptureBusinessEntityController.show().url)
    }
  }

  "Calling the submit action of the Confirm Your Retrieved User Details controller" should {
    "redirect to agree to capture emails page when nino is successfully stored from IRSA" in {
      mockAuthAdminRole()
      mockStoreNinoSuccess(testVatNumber, testUserDetails.nino, IRSA)

      val result = TestConfirmYourRetrievedUserDetailsController.submit(
        testPostRequest.withSession(
          SessionKeys.userDetailsKey -> testUserDetailsJson,
          SessionKeys.vatNumberKey -> testVatNumber,
          SessionKeys.ninoSourceKey -> Json.toJson(IRSA).toString()
        )
      )
      status(result) shouldBe Status.SEE_OTHER
      redirectLocation(result) should contain(routes.DirectDebitResolverController.show().url)
    }

    "redirect to agree to capture emails page when nino is successfully stored from Auth Profile" in {
      mockAuthAdminRole()
      mockStoreNinoSuccess(testVatNumber, testUserDetails.nino, AuthProfile)

      val result = TestConfirmYourRetrievedUserDetailsController.submit(
        testPostRequest.withSession(
          SessionKeys.userDetailsKey -> testUserDetailsJson,
          SessionKeys.vatNumberKey -> testVatNumber,
          SessionKeys.ninoSourceKey -> Json.toJson(AuthProfile).toString()
        )
      )
      status(result) shouldBe Status.SEE_OTHER
      redirectLocation(result) should contain(routes.DirectDebitResolverController.show().url)
    }

    "redirect to capture vat number page when no vat number in session" in {
      mockAuthAdminRole()

      val result = TestConfirmYourRetrievedUserDetailsController.submit(testPostRequest
        .withSession(SessionKeys.userDetailsKey -> testUserDetailsJson))

      status(result) shouldBe Status.SEE_OTHER
      redirectLocation(result) should contain(routes.ResolveVatNumberController.resolve().url)
    }

    "redirect to capture business entity page when no user details in session" in {
      mockAuthAdminRole()

      val result = TestConfirmYourRetrievedUserDetailsController.submit(testPostRequest
        .withSession(SessionKeys.vatNumberKey -> testVatNumber))

      status(result) shouldBe Status.SEE_OTHER
      redirectLocation(result) should contain(routes.CaptureBusinessEntityController.show().url)
    }

    "throw exception when backend returns NoVATNumberFailure" in {
      mockAuthAdminRole()
      mockStoreNinoNoVatStored(testVatNumber, testUserDetails.nino, IRSA)

      intercept[InternalServerException] {
        await(TestConfirmYourRetrievedUserDetailsController.submit(
          testPostRequest.withSession(
            SessionKeys.userDetailsKey -> testUserDetailsJson,
            SessionKeys.vatNumberKey -> testVatNumber,
            SessionKeys.ninoSourceKey -> Json.toJson(IRSA).toString()
          )
        ))
      }
    }

    "throw exception when backend returns StoreNinoFailureResponse" in {
      mockAuthAdminRole()
      mockStoreNinoFailure(testVatNumber, testUserDetails.nino, IRSA)

      intercept[InternalServerException] {
        await(TestConfirmYourRetrievedUserDetailsController.submit(
          testPostRequest.withSession(
            SessionKeys.userDetailsKey -> testUserDetailsJson,
            SessionKeys.vatNumberKey -> testVatNumber,
            SessionKeys.ninoSourceKey -> Json.toJson(IRSA).toString()
          )
        ))
      }
    }

    "throw exception when backend returns any other response" in {
      mockAuthAdminRole()
      mockStoreNinoNoMatch(testVatNumber, testUserDetails.nino, IRSA)

      intercept[InternalServerException] {
        await(TestConfirmYourRetrievedUserDetailsController.submit(
          testPostRequest.withSession(
            SessionKeys.userDetailsKey -> testUserDetailsJson,
            SessionKeys.vatNumberKey -> testVatNumber,
            SessionKeys.ninoSourceKey -> Json.toJson(IRSA).toString()
          )
        ))
      }
    }
  }

}
