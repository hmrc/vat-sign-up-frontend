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

package uk.gov.hmrc.vatsignupfrontend.controllers.agent.soletrader

import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.mvc.{AnyContentAsEmpty, Cookies, Headers, Session}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockVatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.controllers.agent.routes._
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants.{testNino, testVatNumber}
import uk.gov.hmrc.vatsignupfrontend.models.SoleTrader
import uk.gov.hmrc.vatsignupfrontend.services.mocks.MockStoreNinoService
import uk.gov.hmrc.vatsignupfrontend.utils.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.views.html.agent.soletrader.confirm_nino

class ConfirmNinoControllerSpec extends UnitSpec
  with GuiceOneAppPerSuite
  with MockVatControllerComponents
  with MockStoreNinoService {

  val view = app.injector.instanceOf[confirm_nino]

  object TestConfirmNinoController extends ConfirmNinoController(mockStoreNinoService, view)

  val testGetRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/confirm-national-insurance-number")
  val testPostRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("POST", "/confirm-national-insurance-number")

  "Calling the Show method of Confirm NINO controller" when {
    "show the Confirm NINO view" in {
      mockAuthRetrieveAgentEnrolment()

      implicit lazy val request: FakeRequest[AnyContentAsEmpty.type] =
        testGetRequest.withSession(
          SessionKeys.vatNumberKey -> testVatNumber,
          SessionKeys.businessEntityKey -> SoleTrader.toString,
          SessionKeys.ninoKey -> testNino
        )

      val res = TestConfirmNinoController.show(request)

      status(res) shouldBe OK
      contentType(res) shouldBe Some("text/html")
      charset(res) shouldBe Some("utf-8")
    }
  }

  "Calling the submit method of the Confirm NINO controller" when {
    "there is a NINO and VAT number in the user's session" should {
      "Redirect to the capture agent e-mail page" in {
        mockAuthRetrieveAgentEnrolment()
        mockStoreNinoSuccess(testVatNumber, testNino)

        implicit lazy val request: FakeRequest[AnyContentAsEmpty.type] =
          testPostRequest.withSession(
            SessionKeys.vatNumberKey -> testVatNumber,
            SessionKeys.businessEntityKey -> SoleTrader.toString,
            SessionKeys.ninoKey -> testNino
          )

        val futureResult = TestConfirmNinoController.submit(request)
        val result = await(futureResult)

        status(futureResult) shouldBe SEE_OTHER
        redirectLocation(futureResult) should contain(CaptureAgentEmailController.show().url)

        result.session.get(SessionKeys.ninoKey) should contain(testNino)
        result.session.get(SessionKeys.vatNumberKey) should contain(testVatNumber)
        result.session.get(SessionKeys.businessEntityKey) should contain(SoleTrader.toString)
      }
    }

    "there is a VAT number, but no NINO in the user's session" should {
      "redirect to the Capture NINO page" in {
        mockAuthRetrieveAgentEnrolment()

        implicit lazy val request: FakeRequest[AnyContentAsEmpty.type] =
          testPostRequest.withSession(
            SessionKeys.vatNumberKey -> testVatNumber
          )

        val res = TestConfirmNinoController.submit(request)

        status(res) shouldBe SEE_OTHER
        redirectLocation(res) should contain(routes.CaptureNinoController.show().url)
      }
    }

    "there is a NINO, but no VAT number in the user's session" should {
      "redirect to the Capture VAT number page" in {
        mockAuthRetrieveAgentEnrolment()

        implicit lazy val request: FakeRequest[AnyContentAsEmpty.type] =
          testPostRequest.withSession(
            SessionKeys.ninoKey -> testNino
          )

        val res = TestConfirmNinoController.submit(request)

        status(res) shouldBe SEE_OTHER
        redirectLocation(res) should contain(CaptureVatNumberController.show().url)
      }
    }
  }
}
