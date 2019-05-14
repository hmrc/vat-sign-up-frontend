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

package uk.gov.hmrc.vatsignupfrontend.controllers.agent

import play.api.http.Status._
import uk.gov.hmrc.vatsignupfrontend.SessionKeys._
import uk.gov.hmrc.vatsignupfrontend.forms.EmailForm
import uk.gov.hmrc.vatsignupfrontend.helpers.IntegrationTestConstants._
import uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks.AuthStub._
import uk.gov.hmrc.vatsignupfrontend.helpers.{ComponentSpecBase, CustomMatchers, SessionCookieCrumbler}

class CaptureAgentEmailControllerISpec extends ComponentSpecBase with CustomMatchers {



  "GET /email-address" should {
    "return an OK" in {
      stubAuth(OK, successfulAuthResponse(agentEnrolment))

      val res = get("/client/email-address")

      res should have(
        httpStatus(OK)
      )
    }
  }

  "GET /email-address-change" should {
    "return an OK" in {
      stubAuth(OK, successfulAuthResponse(agentEnrolment))

      val res = get("/client/email-address-change")

      res should have(
        httpStatus(SEE_OTHER)
      )
    }
  }

  "POST /email-address" when {
    "client email fs is enabled" should  {
      "return a redirect" in {
        stubAuth(OK, successfulAuthResponse(agentEnrolment))

        val res = post("/client/email-address")(EmailForm.email -> testEmail)

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(routes.ConfirmAgentEmailController.show().url)
        )

        val session = SessionCookieCrumbler.getSessionMap(res)
        session.keys should contain(transactionEmailKey)
      }
    }
  }

}
