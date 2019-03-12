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
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch.ContactPreferencesJourney
import uk.gov.hmrc.vatsignupfrontend.forms.ContactPreferencesForm
import uk.gov.hmrc.vatsignupfrontend.helpers.IntegrationTestConstants._
import uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks.AuthStub._
import uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks.StoreContactPreferenceStub._
import uk.gov.hmrc.vatsignupfrontend.helpers.{ComponentSpecBase, CustomMatchers}
import uk.gov.hmrc.vatsignupfrontend.models.Digital

class ReceiveEmailNotificationsISpec extends ComponentSpecBase with CustomMatchers {

  override def beforeEach(): Unit = {
    super.beforeEach()
    enable(ContactPreferencesJourney)
  }

  override def afterEach(): Unit = {
    super.afterEach()
    disable(ContactPreferencesJourney)
  }

  "GET /receive-email-notifications" when {
    "ContactPreferencesJourney is disabled" should {
      "return an NOT_FOUND" in {
        disable(ContactPreferencesJourney)

        val res = get("/receive-email-notifications")

        res should have(
          httpStatus(NOT_FOUND)
        )
      }
    }

    "ContactPreferenceJourney is enabled" when {
      "email is in session" should {
        "return an OK" in {

          stubAuth(OK, successfulAuthResponse())

          val res = get("/receive-email-notifications", Map(
            SessionKeys.emailKey -> testEmail
          ))

          res should have(
            httpStatus(OK)
          )
        }
      }

      "email is not in session" should {
        "redirect to Capture Email page" in {
          stubAuth(OK, successfulAuthResponse())

          val res = get("/receive-email-notifications")

          res should have(
            httpStatus(SEE_OTHER),
            redirectUri(routes.CaptureEmailController.show().url)
          )
        }
      }
    }
  }

  "POST /receive-email-notifications" when {
    "ContactPreferencesJourney is disabled" should {
      "return an NOT_FOUND" in {
        disable(ContactPreferencesJourney)

        val res = post("/receive-email-notifications")()

        res should have(
          httpStatus(NOT_FOUND)
        )
      }
    }

    "ContactPreferencesJourney is enabled and vat number is in session" should {
      "redirect to the terms page" in {
        stubAuth(OK, successfulAuthResponse())
        stubStoreContactPreferenceSuccess(Digital)

        val res = post("/receive-email-notifications",
          Map(
            SessionKeys.vatNumberKey -> testVatNumber,
            SessionKeys.emailKey -> testEmail
          ))(ContactPreferencesForm.contactPreference -> ContactPreferencesForm.digital)

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(routes.TermsController.show().url)
        )
      }
    }

    "ContactPreferencesJourney is enabled and vat number is not not session" should {
      "redirect to the terms page" in {
        stubAuth(OK, successfulAuthResponse())
        stubStoreContactPreferenceSuccess(Digital)

        val res = post("/receive-email-notifications",
          Map(
            SessionKeys.emailKey -> testEmail
          ))(ContactPreferencesForm.contactPreference -> ContactPreferencesForm.paper)

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(routes.ResolveVatNumberController.resolve().url)
        )
      }
    }
  }
}

