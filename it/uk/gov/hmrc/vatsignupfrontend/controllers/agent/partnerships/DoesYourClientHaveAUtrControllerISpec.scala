/*
 * Copyright 2018 HM Revenue & Customs
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

import play.api.http.Status._
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch.OptionalSautrJourney
import uk.gov.hmrc.vatsignupfrontend.forms.DoYouHaveAUtrForm
import uk.gov.hmrc.vatsignupfrontend.forms.submapping.YesNoMapping._
import uk.gov.hmrc.vatsignupfrontend.helpers.IntegrationTestConstants.{testSaUtr, testBusinessPostCode}
import uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks.AuthStub._
import uk.gov.hmrc.vatsignupfrontend.helpers.{ComponentSpecBase, CustomMatchers, SessionCookieCrumbler}

class DoesYourClientHaveAUtrControllerISpec extends ComponentSpecBase with CustomMatchers {

  override def beforeEach(): Unit = {
    super.beforeEach()
    stubAuth(OK, successfulAuthResponse(agentEnrolment))
    enable(OptionalSautrJourney)
  }

  "GET /client/does-your-client-have-a-utr" should {
    "return an OK" in {
      val res = get("/client/does-your-client-have-a-utr")

      res should have(
        httpStatus(OK)
      )
    }
  }

  "POST /does-your-client-have-a-utr" when {
    "form value is No" should {
      "redirect to Check Your Answers Partnership page" in {
        val res = post(
          uri = "/client/does-your-client-have-a-utr",
          cookies = Map(SessionKeys.partnershipSautrKey -> testSaUtr, SessionKeys.partnershipPostCodeKey -> testBusinessPostCode.postCode)
        )(DoYouHaveAUtrForm.yesNo -> option_no)

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(routes.CheckYourAnswersPartnershipController.show().url)
        )

        SessionCookieCrumbler.getSessionMap(res).get(SessionKeys.partnershipSautrKey) shouldBe None
        SessionCookieCrumbler.getSessionMap(res).get(SessionKeys.partnershipPostCodeKey) shouldBe None
        SessionCookieCrumbler.getSessionMap(res).get(SessionKeys.hasOptionalSautrKey) shouldBe Some(false.toString)
      }
    }

    "form value is Yes" should {
      "redirect to Capture partnership UTR page" in {
        val res = post("/client/does-your-client-have-a-utr")(
          DoYouHaveAUtrForm.yesNo -> option_yes
        )

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(routes.CapturePartnershipUtrController.show().url)
        )

        SessionCookieCrumbler.getSessionMap(res).get(SessionKeys.hasOptionalSautrKey) shouldBe Some(true.toString)
      }
    }
  }
}
