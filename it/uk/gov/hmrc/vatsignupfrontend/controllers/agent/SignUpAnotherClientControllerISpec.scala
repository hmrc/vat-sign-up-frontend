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

package uk.gov.hmrc.vatsignupfrontend.controllers.agent

import play.api.http.Status._
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.helpers.IntegrationTestConstants._
import uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks.AuthStub._
import uk.gov.hmrc.vatsignupfrontend.helpers.{ComponentSpecBase, CustomMatchers, SessionCookieCrumbler}
import uk.gov.hmrc.vatsignupfrontend.models.SoleTrader

class SignUpAnotherClientControllerISpec extends ComponentSpecBase with CustomMatchers {

  "POST /information-received" should {
    "remove all personal data from session and redirect back to vat-number" in {
      stubAuth(OK, successfulAuthResponse(agentEnrolment))

      val res = post("/client/information-received",
        Map(
          SessionKeys.vatNumberKey -> testVatNumber,
          SessionKeys.companyNumberKey -> testCompanyNumber,
          SessionKeys.emailKey -> testEmail,
          SessionKeys.businessEntityKey -> SoleTrader.toString
        ))()

      res should have(
        httpStatus(SEE_OTHER),
        redirectUri(routes.CaptureVatNumberController.show().url)
      )

      val session = SessionCookieCrumbler.getSessionMap(res)
      session.keys should not contain SessionKeys.vatNumberKey
      session.keys should not contain SessionKeys.companyNumberKey
      session.keys should not contain SessionKeys.emailKey
      session.keys should not contain SessionKeys.businessEntityKey
    }
  }
}
