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

package uk.gov.hmrc.vatsignupfrontend.controllers.agent.partnerships

import play.api.http.Status._
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.controllers.agent.{routes => agentRoutes}
import uk.gov.hmrc.vatsignupfrontend.helpers.IntegrationTestConstants._
import uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks.AuthStub._
import uk.gov.hmrc.vatsignupfrontend.helpers.{ComponentSpecBase, CustomMatchers}

class ConfirmPartnershipControllerISpec extends ComponentSpecBase with CustomMatchers {

  "GET /client/confirm-partnership-company" when {
      "return an OK" in {

        stubAuth(OK, successfulAuthResponse(agentEnrolment))

        val res = get("/client/confirm-partnership-company", Map(
          SessionKeys.vatNumberKey -> testVatNumber,
          SessionKeys.companyNumberKey -> testCompanyNumber,
          SessionKeys.companyNameKey -> testCompanyName,
          SessionKeys.partnershipTypeKey -> testPartnershipType
        ))

        res should have(
          httpStatus(OK)
        )
      }

      "redirect to capture vat number" when {
        "there is no vat number in session" in{
          stubAuth(OK, successfulAuthResponse(agentEnrolment))

          val res = get("/client/confirm-partnership-company", Map(
            SessionKeys.companyNameKey -> testCompanyName,
            SessionKeys.companyNumberKey -> testCompanyNumber,
            SessionKeys.partnershipTypeKey -> testPartnershipType
          ))

          res should have(
            httpStatus(SEE_OTHER),
              redirectUri(agentRoutes.CaptureVatNumberController.show().url)
          )
        }
      }

      "throw internal server error" when {
        "there is no company name in session" in{
          stubAuth(OK, successfulAuthResponse(agentEnrolment))

          val res = get("/client/confirm-partnership-company", Map(
            SessionKeys.companyNumberKey -> testCompanyNumber,
            SessionKeys.vatNumberKey -> testVatNumber,
            SessionKeys.partnershipTypeKey -> testPartnershipType
          ))

          res should have(
            httpStatus(SEE_OTHER),
            redirectUri(routes.AgentCapturePartnershipCompanyNumberController.show().url)
          )
        }
      }
  }

  "POST /confirm-partnership-company" should {

    "redirect to capture partnership utr" in {

      stubAuth(OK, successfulAuthResponse(agentEnrolment))

      val res = post("/client/confirm-partnership-company",
        Map(
          SessionKeys.vatNumberKey -> testVatNumber,
          SessionKeys.companyNumberKey -> testCompanyNumber,
          SessionKeys.companyNameKey -> testCompanyName,
          SessionKeys.partnershipTypeKey -> testPartnershipType
        ))()

      res should have(
        httpStatus(SEE_OTHER),
        redirectUri(routes.CapturePartnershipUtrController.show().url)
      )

    }
  }

  "if there is not company name in session" should {
    "redirect to Capture Partnership Company number" in {

      stubAuth(OK, successfulAuthResponse(agentEnrolment))

      val res = post("/client/confirm-partnership-company",
        Map(
          SessionKeys.vatNumberKey -> testVatNumber,
          SessionKeys.companyNumberKey -> testCompanyNumber,
          SessionKeys.partnershipTypeKey -> testPartnershipType
        ))()

      res should have(
        httpStatus(SEE_OTHER),
        redirectUri(routes.AgentCapturePartnershipCompanyNumberController.show().url)
      )
    }
  }

}
