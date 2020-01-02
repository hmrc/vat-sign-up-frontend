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

package uk.gov.hmrc.vatsignupfrontend.controllers.principal

import play.api.http.Status
import play.api.mvc.AnyContentAsFormUrlEncoded
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.play.test.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch.AdditionalKnownFacts
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockControllerComponents
import uk.gov.hmrc.vatsignupfrontend.forms.PreviousVatReturnForm._
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.models.{No, Yes}

class PreviousVatReturnControllerSpec extends UnitSpec with MockControllerComponents {

  object TestPreviousVatReturnController extends PreviousVatReturnController(mockControllerComponents)

  val testGetRequest = FakeRequest("GET", "/submitted-vat-return")

  def testPostRequest(usersChoice: String): FakeRequest[AnyContentAsFormUrlEncoded] =
    FakeRequest("POST", "/submitted-vat-return").withFormUrlEncodedBody(yesNo -> usersChoice)

  override def beforeEach(): Unit = {
    super.beforeEach()
    enable(AdditionalKnownFacts)
  }

  override def afterEach(): Unit = {
    super.afterEach()
    disable(AdditionalKnownFacts)
  }

  "Calling the show action of the Previous Vat Return controller" when {
    "go to the Previous Vat Return page" in {
      mockAuthAdminRole()

      val result = TestPreviousVatReturnController.show(testGetRequest)
      status(result) shouldBe Status.OK
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
    }
  }

  "Calling the submit action of the Previous Vat Return controller" when {
    "form successfully submitted" should {
      "the choice is YES" should {
        "go to vat number" in {
          mockAuthAdminRole()

          val testRequest = testPostRequest(usersChoice = "yes")

          val result = TestPreviousVatReturnController.submit(testRequest)
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.CaptureBox5FigureController.show().url)
          result.session(testRequest).get(SessionKeys.previousVatReturnKey) shouldBe Some(Yes.stringValue)
        }
      }

      "the choice is NO" when {
        "the VAT number is stored successfully" should {
          "go to check your answers" in {
            mockAuthAdminRole()

            val testRequest = testPostRequest(usersChoice = "no")

            val result = TestPreviousVatReturnController.submit(testRequest)
            status(result) shouldBe Status.SEE_OTHER
            redirectLocation(result) shouldBe Some(routes.CheckYourAnswersController.show().url)
            result.session(testRequest).get(SessionKeys.previousVatReturnKey) shouldBe Some(No.stringValue)
          }
        }
        "the VAT number is stored successfully" should {
          "delete the new known facts and go to check your answers" in {
            mockAuthAdminRole()

            val testRequest = testPostRequest(usersChoice = "no").withSession(
              SessionKeys.box5FigureKey -> testBox5Figure,
              SessionKeys.lastReturnMonthPeriodKey -> testLastReturnMonthPeriod
            )

            val result = TestPreviousVatReturnController.submit(testRequest)
            status(result) shouldBe Status.SEE_OTHER
            redirectLocation(result) shouldBe Some(routes.CheckYourAnswersController.show().url)
            result.session(testRequest).get(SessionKeys.previousVatReturnKey) shouldBe Some(No.stringValue)
            result.session(testRequest).get(SessionKeys.lastReturnMonthPeriodKey) shouldBe None
            result.session(testRequest).get(SessionKeys.box5FigureKey) shouldBe None
          }
        }
      }

      "form unsuccessfully submitted" should {
        "reload the page with errors" in {
          mockAuthAdminRole()

          val result = TestPreviousVatReturnController.submit(testPostRequest(usersChoice = "invalid"))
          status(result) shouldBe Status.BAD_REQUEST
          contentType(result) shouldBe Some("text/html")
          charset(result) shouldBe Some("utf-8")
        }
      }
    }
  }
}
