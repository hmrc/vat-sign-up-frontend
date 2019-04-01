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

package uk.gov.hmrc.vatsignupfrontend.controllers.agent.partnerships

import play.api.http.Status
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.http.NotFoundException
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch.OptionalSautrJourney
import uk.gov.hmrc.vatsignupfrontend.controllers.ControllerSpec
import uk.gov.hmrc.vatsignupfrontend.forms.DoYouHaveAUtrForm.yesNo
import uk.gov.hmrc.vatsignupfrontend.forms.submapping.YesNoMapping._
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants.{testSaUtr, testBusinessPostcode}

class DoesYourClientHaveAUtrControllerSpec extends ControllerSpec {

  object TestDoesYourClientHaveAUtrController extends DoesYourClientHaveAUtrController(mockControllerComponents)

  lazy val testGetRequest = FakeRequest("GET", "/client/does-your-client-have-a-utr")

  lazy val testPostRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("POST", "/client/does-your-client-have-a-utr")

  private def testPostRequest(answer: String) = {
    FakeRequest("POST", "/client/does-your-client-have-a-utr").withFormUrlEncodedBody(yesNo -> answer)
      .withSession(
        SessionKeys.partnershipSautrKey -> testSaUtr,
        SessionKeys.partnershipPostCodeKey -> testBusinessPostcode.postCode
      )
  }

  "Calling the show action of Does Your Client Have A Utr Controller" when {
    "Optional Sautr Journey is enabled" should {
      "return an OK" in {
        enable(OptionalSautrJourney)
        mockAuthRetrieveAgentEnrolment()

        val result = await(TestDoesYourClientHaveAUtrController.show(testGetRequest))
        status(result) shouldBe Status.OK
        contentType(result) shouldBe Some("text/html")
        charset(result) shouldBe Some("utf-8")
      }
    }

    "Optional Sautr Journey is disabled" should {
      "return NOT_FOUND" in {
        intercept[NotFoundException](
          await(TestDoesYourClientHaveAUtrController.show(testGetRequest))
        )
      }
    }
  }

  "Calling the submit action of Does Your Client Have A Utr Controller" when {
    "Optional Sautr Journey is enabled" when {
      "No is submitted" should {
        "redirect to Capture Partnership UTR page" in {
          enable(OptionalSautrJourney)
          mockAuthRetrieveAgentEnrolment()

          val result = await(TestDoesYourClientHaveAUtrController.submit(testPostRequest(option_no)))

          session(result).get(SessionKeys.partnershipSautrKey) shouldBe None
          session(result).get(SessionKeys.partnershipPostCodeKey) shouldBe None
          session(result).get(SessionKeys.hasOptionalSautrKey) shouldBe Some(false.toString)
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.CheckYourAnswersPartnershipController.show().url)
        }
      }

      "Yes is submitted" should {
        "redirect to Check Your Answers page" in {
          enable(OptionalSautrJourney)
          mockAuthRetrieveAgentEnrolment()

          val result = await(TestDoesYourClientHaveAUtrController.submit(testPostRequest(option_yes)))

          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.CapturePartnershipUtrController.show().url)
          session(result).get(SessionKeys.hasOptionalSautrKey) shouldBe Some(true.toString)
        }
      }

      "An error is submitted" should {
        "return status BAD_REQUEST" in {
          enable(OptionalSautrJourney)
          mockAuthRetrieveAgentEnrolment()

          val result = await(TestDoesYourClientHaveAUtrController.submit(testPostRequest("")))

          status(result) shouldBe Status.BAD_REQUEST
          contentType(result) shouldBe Some("text/html")
          charset(result) shouldBe Some("utf-8")
        }
      }
    }

    "Optional Sautr Journey is disabled" should {
      "return NOT_FOUND" in {
        intercept[NotFoundException](
          await(TestDoesYourClientHaveAUtrController.submit(testPostRequest(option_yes)))
        )
      }
    }
  }
}
