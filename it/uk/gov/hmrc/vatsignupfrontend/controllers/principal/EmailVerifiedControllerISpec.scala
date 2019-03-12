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

package uk.gov.hmrc.vatsignupfrontend.controllers.principal

import play.api.http.Status.{NOT_IMPLEMENTED, OK, SEE_OTHER}
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch.ContactPreferencesJourney
import uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks.AuthStub._
import uk.gov.hmrc.vatsignupfrontend.helpers.{ComponentSpecBase, CustomMatchers}

class EmailVerifiedControllerISpec extends ComponentSpecBase with CustomMatchers {

  "GET /email-verified" should {
    "return an OK" in {
      stubAuth(OK, successfulAuthResponse())

      val res = get("/email-verified")

      res should have(
        httpStatus(OK)
      )
    }
  }

  "POST /email-verified" should {
    "return a Not Implemented" when {
      "the contact preferences feature switch is enabled" in {
        stubAuth(OK, successfulAuthResponse())
        enable(ContactPreferencesJourney)

        val res = post("/email-verified")()

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(routes.ReceiveEmailNotificationsController.show().url)
        )
      }
    }
    "redirect to Terms" when {
      "the contact preferences feature switch is not enabled" in {
        stubAuth(OK, successfulAuthResponse())
        disable(ContactPreferencesJourney)

        val res = post("/email-verified")()

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(routes.TermsController.show().url)
        )
      }
    }
  }

}
