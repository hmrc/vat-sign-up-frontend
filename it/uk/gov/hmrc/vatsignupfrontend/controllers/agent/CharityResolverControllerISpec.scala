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
import uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks.AuthStub.{agentEnrolment, stubAuth, successfulAuthResponse}
import uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks.StoreCharityInformationStub._
import uk.gov.hmrc.vatsignupfrontend.helpers.{ComponentSpecBase, CustomMatchers}

class CharityResolverControllerISpec extends ComponentSpecBase with CustomMatchers {


  "GET /charity-resolver" when {
    "store charity information returned NO_CONTENT" should {
      "go to the capture agent email page" in {
        stubAuth(OK, successfulAuthResponse(agentEnrolment))

        stubStoreCharityInformation(testVatNumber)(NO_CONTENT)

        val res = get("/client/charity-resolver", Map(
          SessionKeys.vatNumberKey -> testVatNumber
        ))

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(routes.CaptureAgentEmailController.show().url)
        )
      }
    }
    "store charity information returned failure status" should {
      "return INTERNAL_SERVER_ERROR" in {
        stubAuth(OK, successfulAuthResponse(agentEnrolment))

        stubStoreCharityInformation(testVatNumber)(INTERNAL_SERVER_ERROR)

        val res = get("/client/charity-resolver", Map(
          SessionKeys.vatNumberKey -> testVatNumber
        ))

        res should have(
          httpStatus(INTERNAL_SERVER_ERROR)
        )
      }
    }
    "there is no vat number in session" should {
      "goto capture vat number" in {
        stubAuth(OK, successfulAuthResponse(agentEnrolment))

        stubStoreCharityInformation(testVatNumber)(OK)

        val res = get("/client/charity-resolver")

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(routes.CaptureVatNumberController.show().url)
        )
      }
    }
  }
}
