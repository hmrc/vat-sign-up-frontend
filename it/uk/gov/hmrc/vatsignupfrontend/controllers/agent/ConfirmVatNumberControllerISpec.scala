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

import java.time.LocalDate

import play.api.http.Status._
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.controllers.agent.error.{routes => errorRoutes}
import uk.gov.hmrc.vatsignupfrontend.helpers.IntegrationTestConstants._
import uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks.AuthStub._
import uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks.StoreVatNumberStub._
import uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks.VatNumberEligibilityStub._
import uk.gov.hmrc.vatsignupfrontend.helpers.{ComponentSpecBase, CustomMatchers}
import uk.gov.hmrc.vatsignupfrontend.models.MigratableDates

class ConfirmVatNumberControllerISpec extends ComponentSpecBase with CustomMatchers {

  "GET /confirm-vat-number" should {
    "return an OK" in {
      stubAuth(OK, successfulAuthResponse(agentEnrolment))

      val res = get("/client/confirm-vat-number", Map(SessionKeys.vatNumberKey -> testVatNumber))

      res should have(
        httpStatus(OK)
      )
    }
  }

  "POST /vat-number" should {
    "redirect to the capture client business entity page" when {
      "the vat number is successfully stored" in {
        stubAuth(OK, successfulAuthResponse(agentEnrolment))
        stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(Eligible))
        stubStoreVatNumberSuccess(isFromBta = false)

        val res = post("/client/confirm-vat-number", Map(SessionKeys.vatNumberKey -> testVatNumber))()

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(routes.CaptureBusinessEntityController.show().url)
        )
      }

      "the vat number is successfully stored and isMigrated" in {
        stubAuth(OK, successfulAuthResponse(agentEnrolment))
        stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(Migrated))
        stubStoreMigratedVatNumber(testVatNumber)(status = OK)

        val res = post("/client/confirm-vat-number", Map(SessionKeys.vatNumberKey -> testVatNumber))()

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(routes.CaptureBusinessEntityController.show().url)
        )
      }

      "the overseas vat number is successfully stored" in {
        stubAuth(OK, successfulAuthResponse(agentEnrolment))
        stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(Overseas(isMigrated = false)))
        stubStoreVatNumberSuccess(isFromBta = false, isOverseasTrader = true)

        val res = post("/client/confirm-vat-number", Map(SessionKeys.vatNumberKey -> testVatNumber))()

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(routes.CaptureBusinessEntityController.show().url)
        )
      }

      "the overseas migrated vat number is successfully stored" in {
        stubAuth(OK, successfulAuthResponse(agentEnrolment))
        stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(Overseas(isMigrated = true)))
        stubStoreMigratedVatNumber(testVatNumber)(status = OK)

        val res = post("/client/confirm-vat-number", Map(SessionKeys.vatNumberKey -> testVatNumber))()

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(routes.CaptureBusinessEntityController.show().url)
        )
      }
    }

    "redirect to no agent client relationship page" when {
      "the vat number is unsuccessfully stored as there is no client agent relationship" in {
        stubAuth(OK, successfulAuthResponse(agentEnrolment))
        stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(Eligible))
        stubStoreVatNumberNoRelationship(isFromBta = false)

        val res = post("/client/confirm-vat-number", Map(SessionKeys.vatNumberKey -> testVatNumber))()

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(errorRoutes.NoAgentClientRelationshipController.show().url)
        )
      }
    }

    "redirect to cannot use service page" when {
      "the vat number is unsuccessfully stored as the client is ineligible" in {
        stubAuth(OK, successfulAuthResponse(agentEnrolment))
        stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(Ineligible))

        val res = post("/client/confirm-vat-number", Map(SessionKeys.vatNumberKey -> testVatNumber))()

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(errorRoutes.CannotUseServiceController.show().url)
        )
      }
    }

    "redirect to deregistered VAT number page" when {
      "the vat number is unsuccessfully stored as the client is Deregistered" in {
        stubAuth(OK, successfulAuthResponse(agentEnrolment))
        stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(Deregistered))

        val res = post("/client/confirm-vat-number", Map(SessionKeys.vatNumberKey -> testVatNumber))()

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(errorRoutes.DeregisteredVatNumberController.show().url)
        )
      }
    }

    "redirect to recently registered VRN page" when {
      "the vat number is unsuccessfully stored as the vrn is registered less than a week ago" in {
        stubAuth(OK, successfulAuthResponse(agentEnrolment))
        stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(RecentlyRegistered))

        val res = post("/client/confirm-vat-number", Map(SessionKeys.vatNumberKey -> testVatNumber))()

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(errorRoutes.RecentlyRegisteredVatNumberController.show().url)
        )
      }
    }

    "redirect to the sign up after this date page" when {
      "the vat number is unsuccessfully stored as the client is ineligible for mtd vat and one date is available" in {
        stubAuth(OK, successfulAuthResponse(agentEnrolment))
        stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(Inhibited(MigratableDates(Some(LocalDate.now)))))

        val res = post("/client/confirm-vat-number", Map(SessionKeys.vatNumberKey -> testVatNumber))()

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(errorRoutes.MigratableDatesController.show().url)
        )

        getSessionMap(res).get(SessionKeys.migratableDatesKey) shouldBe defined
      }
    }

    "redirect to the sign up after this date page" when {
      "the vat number is unsuccessfully stored as the client is ineligible for mtd vat and two dates is available" in {
        stubAuth(OK, successfulAuthResponse(agentEnrolment))
        stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(Inhibited(MigratableDates(Some(LocalDate.now()), Some(LocalDate.now())))))

        val res = post("/client/confirm-vat-number", Map(SessionKeys.vatNumberKey -> testVatNumber))()

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(errorRoutes.MigratableDatesController.show().url)
        )

        getSessionMap(res).get(SessionKeys.migratableDatesKey) shouldBe defined
      }
    }

    "redirect to the already signed up page" when {
      "the vat number has already been signed up" in {
        stubAuth(OK, successfulAuthResponse(agentEnrolment))
        stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(AlreadySubscribed(isOverseas = false)))

        val res = post("/client/confirm-vat-number", Map(SessionKeys.vatNumberKey -> testVatNumber))()

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(errorRoutes.AlreadySignedUpController.show().url)
        )
      }
    }

    "redirect to the migration in progress error page" when {
      "the vat number has already been signed up and migration is in progress" in {
        stubAuth(OK, successfulAuthResponse(agentEnrolment))
        stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(MigrationInProgress))

        val res = post("/client/confirm-vat-number", Map(SessionKeys.vatNumberKey -> testVatNumber))()

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(errorRoutes.MigrationInProgressErrorController.show().url)
        )
      }
    }

    "redirect to the CouldNotConfirmVatNumber page" when {
      "the eligibility check returns Not Found" in {
        stubAuth(OK, successfulAuthResponse(agentEnrolment))
        stubVatNumberEligibility(testVatNumber)(status = NOT_FOUND, optEligibilityResponse = None)

        val res = post("/client/confirm-vat-number", Map(SessionKeys.vatNumberKey -> testVatNumber))()

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(errorRoutes.CouldNotConfirmVatNumberController.show().url)
        )
      }
    }

    "throw an internal server error" when {
      "the vat number cannot be stored" in {
        stubAuth(OK, successfulAuthResponse(agentEnrolment))
        stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(Eligible))
        stubStoreVatNumberFailure(isFromBta = false)

        val res = post("/client/confirm-vat-number", Map(SessionKeys.vatNumberKey -> testVatNumber))()

        res should have(
          httpStatus(INTERNAL_SERVER_ERROR)
        )
      }
    }
  }
}
