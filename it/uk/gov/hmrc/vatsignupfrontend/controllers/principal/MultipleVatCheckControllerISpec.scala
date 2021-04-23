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

package uk.gov.hmrc.vatsignupfrontend.controllers.principal

import play.api.http.Status._
import uk.gov.hmrc.vatsignupfrontend.SessionKeys._
import uk.gov.hmrc.vatsignupfrontend.controllers.principal.error.{routes => errorRoutes}
import uk.gov.hmrc.vatsignupfrontend.forms.MultipleVatCheckForm
import uk.gov.hmrc.vatsignupfrontend.forms.submapping.YesNoMapping
import uk.gov.hmrc.vatsignupfrontend.helpers.IntegrationTestConstants._
import uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks.AuthStub._
import uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks.ClaimSubscriptionStub._
import uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks.StoreVatNumberStub._
import uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks.VatNumberEligibilityStub._
import uk.gov.hmrc.vatsignupfrontend.helpers.{ComponentSpecBase, CustomMatchers}
import uk.gov.hmrc.vatsignupfrontend.models.MigratableDates

class MultipleVatCheckControllerISpec extends ComponentSpecBase with CustomMatchers {

  "GET /more-than-one-vat-business" should {
    "return an OK" in {
      stubAuth(OK, successfulAuthResponse())

      val res = get("/more-than-one-vat-business")

      res should have(
        httpStatus(OK)
      )
    }
  }

  "POST /more-than-one-vat-business" when {
    "return a redirect to vat number" when {
      "form value is YES" in {
        stubAuth(OK, successfulAuthResponse(vatDecEnrolment))

        val res = post("/more-than-one-vat-business")(MultipleVatCheckForm.yesNo -> YesNoMapping.option_yes)

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(routes.CaptureVatNumberController.show().url)
        )
      }
    }

    "return a redirect to business type with isMigrated in session" when {
      "form value is NO and the VRN is migrated" in {
        stubAuth(OK, successfulAuthResponse(vatDecEnrolment))
        stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(Migrated))
        stubStoreMigratedVatNumber(testVatNumber)(status = OK)

        val res = post("/more-than-one-vat-business")(MultipleVatCheckForm.yesNo -> YesNoMapping.option_no)

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(routes.CaptureBusinessEntityController.show().url)
        )

        val session = getSessionMap(res)
        session.get(isMigratedKey) shouldBe Some("true")
      }
    }

    "return a redirect to business type with isDirectDebit in session" when {
      "form value is NO and the VRN is not migrated and does not have direct debit when there is only vat dec enrolment" in {
        stubAuth(OK, successfulAuthResponse(vatDecEnrolment))
        stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(Migrated))
        stubStoreMigratedVatNumber(testVatNumber)(status = OK)

        val res = post("/more-than-one-vat-business")(MultipleVatCheckForm.yesNo -> YesNoMapping.option_no)

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(routes.CaptureBusinessEntityController.show().url)
        )

        val session = getSessionMap(res)
        session.get(hasDirectDebitKey) shouldBe Some("false")
      }
    }

    "return a redirect to business type with isDirectDebit in session" when {
      "form value is NO and the VRN is not migrated and does not have direct debit when there is only mtd vat enrolment" in {
        stubAuth(OK, successfulAuthResponse(mtdVatEnrolment))
        stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(Migrated))
        stubStoreMigratedVatNumber(testVatNumber)(status = OK)

        val res = post("/more-than-one-vat-business")(MultipleVatCheckForm.yesNo -> YesNoMapping.option_no)

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(routes.CaptureBusinessEntityController.show().url)
        )

        val session = getSessionMap(res)
        session.get(hasDirectDebitKey) shouldBe Some("false")
      }
    }

    "return a redirect to business type with isDirectDebit in session" when {
      "form value is NO and the VRN is not migrated and does not have direct debit when both enrolments exist" in {
        stubAuth(OK, successfulAuthResponse(vatDecEnrolment, mtdVatEnrolment))
        stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(Migrated))
        stubStoreMigratedVatNumber(testVatNumber)(status = OK)

        val res = post("/more-than-one-vat-business")(MultipleVatCheckForm.yesNo -> YesNoMapping.option_no)

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(routes.CaptureBusinessEntityController.show().url)
        )

        val session = getSessionMap(res)
        session.get(hasDirectDebitKey) shouldBe Some("false")
      }
    }

    "return a redirect to business type" when {
      "form value is NO and the VRN is not migrated and is overseas" in {
        stubAuth(OK, successfulAuthResponse(vatDecEnrolment))
        stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(Overseas(isMigrated = false)))
        stubStoreVatNumberSuccess(isFromBta = false, isOverseasTrader = true)

        val res = post("/more-than-one-vat-business")(MultipleVatCheckForm.yesNo -> YesNoMapping.option_no)

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(routes.CaptureBusinessEntityController.show().url)
        )
      }
    }

    "return a redirect to sign up after this date page" when {
      "form value is NO and the VRN is inhibited" in {
        stubAuth(OK, successfulAuthResponse(vatDecEnrolment))
        stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(Inhibited(MigratableDates(Some(testStartDate)))))

        val res = post("/more-than-one-vat-business")(MultipleVatCheckForm.yesNo -> YesNoMapping.option_no)

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(errorRoutes.MigratableDatesController.show().url)
        )
      }
    }

    "return a redirect to recently registered vrn page" when {
      "form value is NO and the VRN is registered less than a week ago" in {
        stubAuth(OK, successfulAuthResponse(vatDecEnrolment))
        stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(RecentlyRegistered))

        val res = post("/more-than-one-vat-business")(MultipleVatCheckForm.yesNo -> YesNoMapping.option_no)

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(errorRoutes.RecentlyRegisteredVatNumberController.show().url)
        )
      }
    }

    "return a redirect to sign up between these dates page" when {
      "form value is NO and the VRN is inhibited" in {
        stubAuth(OK, successfulAuthResponse(vatDecEnrolment))
        stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(Inhibited(MigratableDates(Some(testStartDate), Some(testEndDate)))))

        val res = post("/more-than-one-vat-business")(MultipleVatCheckForm.yesNo -> YesNoMapping.option_no)

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(errorRoutes.MigratableDatesController.show().url)
        )
      }
    }

    "return a redirect to cannot use service error page" when {
      "form value is NO and the VRN is ineligible" in {
        stubAuth(OK, successfulAuthResponse(vatDecEnrolment))
        stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(Ineligible))

        val res = post("/more-than-one-vat-business")(MultipleVatCheckForm.yesNo -> YesNoMapping.option_no)

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(errorRoutes.CannotUseServiceController.show().url)
        )
      }
    }

    "return a redirect to deregistered VAT number error page" when {
      "form value is NO and the VRN is Deregistered" in {
        stubAuth(OK, successfulAuthResponse(vatDecEnrolment))
        stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(Deregistered))

        val res = post("/more-than-one-vat-business")(MultipleVatCheckForm.yesNo -> YesNoMapping.option_no)

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(errorRoutes.DeregisteredVatNumberController.show().url)
        )
      }
    }

    "redirect to migration in progress error page" when {
      "form value is NO and the VRN is currently being migrated" in {
        stubAuth(OK, successfulAuthResponse(vatDecEnrolment))
        stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(MigrationInProgress))

        val res = post("/more-than-one-vat-business")(MultipleVatCheckForm.yesNo -> YesNoMapping.option_no)

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(errorRoutes.MigrationInProgressErrorController.show().url)
        )
      }
    }

    "redirect to business already signed up error page" when {
      "form value is NO and the vrn is already subscribed with the subscription successfully claimed" in {
        stubAuth(OK, successfulAuthResponse(vatDecEnrolment))
        stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(AlreadySubscribed(isOverseas = false)))
        stubClaimSubscription(testVatNumber, isFromBta = false)(NO_CONTENT)

        val res = post("/more-than-one-vat-business")(MultipleVatCheckForm.yesNo -> YesNoMapping.option_no)

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(routes.SignUpCompleteClientController.show().url)
        )
      }
    }

    "redirect to business already signed up error page" when {
      "form value is NO and the vrn is already enrolled on a different cred" in {
        stubAuth(OK, successfulAuthResponse(vatDecEnrolment))
        stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(AlreadySubscribed(isOverseas = false)))
        stubClaimSubscription(testVatNumber, isFromBta = false)(CONFLICT)

        val res = post("/more-than-one-vat-business")(MultipleVatCheckForm.yesNo -> YesNoMapping.option_no)

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(errorRoutes.BusinessAlreadySignedUpController.show().url)
        )
      }
    }

    "redirect to business already signed up error page" when {
      "form value is NO and the vrn is already signed up" in {
        stubAuth(OK, successfulAuthResponse(vatDecEnrolment, mtdVatEnrolment))
        stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(AlreadySubscribed(isOverseas = false)))

        val res = post("/more-than-one-vat-business")(MultipleVatCheckForm.yesNo -> YesNoMapping.option_no)

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(errorRoutes.AlreadySignedUpController.show().url)
        )
      }
    }
  }
}
