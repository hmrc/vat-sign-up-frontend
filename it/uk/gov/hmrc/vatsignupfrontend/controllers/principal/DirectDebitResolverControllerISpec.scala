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

import play.api.http.Status._
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch.DirectDebitTermsJourney
import uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks.AuthStub.{stubAuth, successfulAuthResponse}
import uk.gov.hmrc.vatsignupfrontend.helpers.{ComponentSpecBase, CustomMatchers}

class DirectDebitResolverControllerISpec extends ComponentSpecBase with CustomMatchers {

  "GET /direct-debit-resolver" when {

    "the user has a direct debit hasDirectDebitKey -> true" when {

      "the direct debit feature is enabled" should {

        "redirect to the Direct Debit T&Cs Agree page" in {

          enable(DirectDebitTermsJourney)
          stubAuth(OK, successfulAuthResponse())

          val result = get("/direct-debit-resolver", Map(SessionKeys.directDebitKey -> "true"))

          result should have(
            httpStatus(SEE_OTHER),
            redirectUri(routes.DirectDebitTermsAndConditionsController.show().url)
          )
        }
      }

      "the direct debit feature is disabled" should {

        "redirect to the Agree Email Capture page" in {

          disable(DirectDebitTermsJourney)
          stubAuth(OK, successfulAuthResponse())

          val result = get("/direct-debit-resolver", Map(SessionKeys.directDebitKey -> "true"))

          result should have(
            httpStatus(SEE_OTHER),
            redirectUri(routes.AgreeCaptureEmailController.show().url)
          )
        }
      }
    }

    "the user does not have direct debit" when {

      "the direct debit feature is enabled" should {

        "redirect to the Agree Email Capture page" in {

          enable(DirectDebitTermsJourney)
          stubAuth(OK, successfulAuthResponse())

          val result = get("/direct-debit-resolver")

          result should have(
            httpStatus(SEE_OTHER),
            redirectUri(routes.AgreeCaptureEmailController.show().url)
          )
        }
      }

      "the direct debit feature is disabled" should {

        "redirect to the Agree Email Capture page" in {

          disable(DirectDebitTermsJourney)
          stubAuth(OK, successfulAuthResponse())

          val result = get("/direct-debit-resolver")

          result should have(
            httpStatus(SEE_OTHER),
            redirectUri(routes.AgreeCaptureEmailController.show().url)
          )
        }
      }
    }
  }
}
