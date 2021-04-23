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
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.controllers.principal.error.{routes => errorRoutes}
import uk.gov.hmrc.vatsignupfrontend.forms.VatNumberForm
import uk.gov.hmrc.vatsignupfrontend.helpers.IntegrationTestConstants._
import uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks.AuthStub._
import uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks.ClaimSubscriptionStub._
import uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks.StoreVatNumberStub._
import uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks.VatNumberEligibilityStub._
import uk.gov.hmrc.vatsignupfrontend.helpers.{ComponentSpecBase, CustomMatchers, IntegrationTestConstantsGenerator}
import uk.gov.hmrc.vatsignupfrontend.models.MigratableDates

class CaptureVatNumberControllerISpec extends ComponentSpecBase with CustomMatchers {

  "GET /vat-number" when {
    "the KnownFactsJourney feature switch is enabled" should {
      "return an OK" in {
        stubAuth(OK, successfulAuthResponse())

        val res = get("/vat-number")

        res should have(
          httpStatus(OK)
        )
      }
    }
  }

  "POST /vat-number" when {

    "the user has a VAT-DEC enrolment" when {
      "the vat eligibility is successful" when {
        "the enrolment vat number matches the inserted one" should {
          "redirect to the business entity type page" in {
            stubAuth(OK, successfulAuthResponse(vatDecEnrolment))
            stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(Migrated))
            stubStoreMigratedVatNumber(testVatNumber)(status = OK)

            val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)

            res should have(
              httpStatus(SEE_OTHER),
              redirectUri(routes.CaptureBusinessEntityController.show().url)
            )
          }
        }

        "the enrolment vat number doesn't match the inserted one" should {
          "redirect to error page" in {
            val nonMatchingVat = IntegrationTestConstantsGenerator.randomVatNumber
            stubAuth(OK, successfulAuthResponse(vatDecEnrolment))

            val res = post("/vat-number")(VatNumberForm.vatNumber -> nonMatchingVat)

            res should have(
              httpStatus(SEE_OTHER),
              redirectUri(errorRoutes.IncorrectEnrolmentVatNumberController.show().url)
            )
          }
        }
      }

      "the vat eligibility is unsuccessful" should {
        "redirect to the invalid vat number page" when {
          "the vat number is fails the checksum validation" in {
            stubAuth(OK, successfulAuthResponse(vatDecEnrolment))

            val res = post("/vat-number")(VatNumberForm.vatNumber -> testInvalidVatNumber)

            res should have(
              httpStatus(SEE_OTHER),
              redirectUri(errorRoutes.InvalidVatNumberController.show().url)
            )
          }
        }

        "redirect to the Cannot Use Service page" when {
          "the vat number is ineligible for mtd vat" in {
            stubAuth(OK, successfulAuthResponse(vatDecEnrolment))
            stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(Ineligible))
            stubStoreVatNumberIneligible(isFromBta = false, migratableDates = MigratableDates(None))

            val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)

            res should have(
              httpStatus(SEE_OTHER),
              redirectUri(errorRoutes.CannotUseServiceController.show().url)
            )
          }
        }

        "redirect to the Deregistered Vat Number page" when {
          "the vat number is deregistered for mtd vat" in {
            stubAuth(OK, successfulAuthResponse(vatDecEnrolment))
            stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(Deregistered))
            stubStoreVatNumberIneligible(isFromBta = false, migratableDates = MigratableDates(None))

            val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)

            res should have(
              httpStatus(SEE_OTHER),
              redirectUri(errorRoutes.DeregisteredVatNumberController.show().url)
            )
          }
        }

        "redirect to the sign up after this date page" when {
          "the vat number is ineligible for mtd vat and one date is available" in {
            val testDates = MigratableDates(Some(testStartDate))
            stubAuth(OK, successfulAuthResponse(vatDecEnrolment))
            stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(Inhibited(testDates)))
            stubStoreVatNumberIneligible(isFromBta = false, migratableDates = testDates)

            val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)

            res should have(
              httpStatus(SEE_OTHER),
              redirectUri(errorRoutes.MigratableDatesController.show().url)
            )
          }
        }

        "redirect to the sign up between these dates page" when {
          "the vat number is ineligible for mtd vat and two dates are available" in {
            val testDates = MigratableDates(Some(testStartDate), Some(testEndDate))
            stubAuth(OK, successfulAuthResponse(vatDecEnrolment))
            stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(Inhibited(testDates)))
            stubStoreVatNumberIneligible(isFromBta = false, migratableDates = testDates)

            val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)

            res should have(
              httpStatus(SEE_OTHER),
              redirectUri(errorRoutes.MigratableDatesController.show().url)
            )
          }
        }

        "redirect to the migration in progress error page" when {
          "the vat number is already signed up and migrating" in {
            stubAuth(OK, successfulAuthResponse(vatDecEnrolment))
            stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(MigrationInProgress))
            stubStoreVatNumberMigrationInProgress(isFromBta = false)

            val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)

            res should have(
              httpStatus(SEE_OTHER),
              redirectUri(errorRoutes.MigrationInProgressErrorController.show().url)
            )
          }
        }

        "redirect to the recently registered error page" when {
          "the vat number has been registered less than a week ago" in {
            stubAuth(OK, successfulAuthResponse(vatDecEnrolment))
            stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(RecentlyRegistered))
            stubStoreVatNumberMigrationInProgress(isFromBta = false)

            val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)

            res should have(
              httpStatus(SEE_OTHER),
              redirectUri(errorRoutes.RecentlyRegisteredVatNumberController.show().url)
            )
          }
        }

        "redirect to the business already signed up error page" when {
          "the vat number is already signed up and a user is attempting to claim subscription" in {
            stubAuth(OK, successfulAuthResponse(vatDecEnrolment))
            stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(AlreadySubscribed(isOverseas = false)))
            stubStoreVatNumberAlreadySignedUp(isFromBta = false)
            stubClaimSubscription(testVatNumber, isFromBta = false)(CONFLICT)

            val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)

            res should have(
              httpStatus(SEE_OTHER),
              redirectUri(errorRoutes.BusinessAlreadySignedUpController.show().url)
            )
          }
        }

        "redirect to the overseas resolver controller" when {
          "the vat number is overseas" in {
            stubAuth(OK, successfulAuthResponse(vatDecEnrolment))
            stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(Overseas(isMigrated = false)))
            stubStoreVatNumberSuccess(isFromBta = false, isOverseasTrader = true)

            val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)

            res should have(
              httpStatus(SEE_OTHER),
              redirectUri(routes.CaptureBusinessEntityController.show().url)
            )
          }
        }

        "Redirect to the InvalidVatNumber page" when {
          "The eligibility check returns 404" in {
            stubAuth(OK, successfulAuthResponse(vatDecEnrolment))
            stubVatNumberEligibility(testVatNumber)(status = NOT_FOUND, optEligibilityResponse = None)

            val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)

            res should have(
              httpStatus(SEE_OTHER),
              redirectUri(errorRoutes.InvalidVatNumberController.show().url)
            )
          }
        }

        "throw an internal server error" when {
          "any other failure occurs" in {
            stubAuth(OK, successfulAuthResponse(vatDecEnrolment))
            stubVatNumberEligibility(testVatNumber)(status = INTERNAL_SERVER_ERROR, optEligibilityResponse = None)

            val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)

            res should have(
              httpStatus(INTERNAL_SERVER_ERROR)
            )
          }
        }
      }
    }

    "the user has an MTD-VAT enrolment" when {
      "the vat eligibility is successful" when {
        "the enrolment vat number matches the inserted one" should {
          "redirect to the business entity type page" in {
            stubAuth(OK, successfulAuthResponse(mtdVatEnrolment))
            stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(Migrated))
            stubStoreMigratedVatNumber(testVatNumber)(status = OK)

            val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)

            res should have(
              httpStatus(SEE_OTHER),
              redirectUri(routes.CaptureBusinessEntityController.show().url)
            )
          }
        }

        "the enrolment vat number doesn't match the inserted one" should {
          "redirect to error page" in {
            val nonMatchingVat = IntegrationTestConstantsGenerator.randomVatNumber
            stubAuth(OK, successfulAuthResponse(mtdVatEnrolment))

            val res = post("/vat-number")(VatNumberForm.vatNumber -> nonMatchingVat)

            res should have(
              httpStatus(SEE_OTHER),
              redirectUri(errorRoutes.IncorrectEnrolmentVatNumberController.show().url)
            )
          }
        }
      }
    }

    "the user has both MTD-VAT and VAT-DEC enrolments" when {
      "the vat eligibility is successful" when {
        "the enrolment vat number matches the inserted one" should {
          "redirect to the business entity type page" in {
            stubAuth(OK, successfulAuthResponse(mtdVatEnrolment, vatDecEnrolment))
            stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(Migrated))
            stubStoreMigratedVatNumber(testVatNumber)(status = OK)

            val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)

            res should have(
              httpStatus(SEE_OTHER),
              redirectUri(routes.CaptureBusinessEntityController.show().url)
            )
          }
        }

        "the enrolment vat number doesn't match the inserted one" should {
          "redirect to error page" in {
            val nonMatchingVat = IntegrationTestConstantsGenerator.randomVatNumber
            stubAuth(OK, successfulAuthResponse(mtdVatEnrolment, vatDecEnrolment))

            val res = post("/vat-number")(VatNumberForm.vatNumber -> nonMatchingVat)

            res should have(
              httpStatus(SEE_OTHER),
              redirectUri(errorRoutes.IncorrectEnrolmentVatNumberController.show().url)
            )
          }
        }
      }
    }

    "the user doesn't have an enrolment" when {
      "the vat eligibility is successful" should {
        "redirect to Capture Vat Registration Date page" in {
          stubAuth(OK, successfulAuthResponse())
          stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(Migrated))

          val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)

          res should have(
            httpStatus(SEE_OTHER),
            redirectUri(routes.CaptureVatRegistrationDateController.show().url)
          )
        }

        "the vat number is eligible and overseas" should {
          "redirect to the Vat Registration Date" in {
            stubAuth(OK, successfulAuthResponse())
            stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(Overseas(isMigrated = false)))

            val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)

            res should have(
              httpStatus(SEE_OTHER),
              redirectUri(routes.CaptureVatRegistrationDateController.show().url)
            )
          }
        }

        "the vat number is AlreadySubscribed" when {
          "the company is overseas" should {
            "redirect to the Vat Registration Date" in {
              stubAuth(OK, successfulAuthResponse())
              stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(AlreadySubscribed(isOverseas = true)))

              val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)
              res should have(
                httpStatus(SEE_OTHER),
                redirectUri(routes.CaptureVatRegistrationDateController.show().url)
              )
            }
          }
          "the company is not overseas" should {
            "redirect to the Vat Registration Date" in {
              stubAuth(OK, successfulAuthResponse())
              stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(AlreadySubscribed(isOverseas = false)))

              val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)

              res should have(
                httpStatus(SEE_OTHER),
                redirectUri(routes.CaptureVatRegistrationDateController.show().url)
              )
            }
          }
        }
      }

      "the vat eligibility is unsuccessful" when {
        "redirect to the invalid vat number page" when {
          "the vat number is fails the checksum validation" in {
            stubAuth(OK, successfulAuthResponse())

            val res = post("/vat-number")(VatNumberForm.vatNumber -> testInvalidVatNumber)

            res should have(
              httpStatus(SEE_OTHER),
              redirectUri(errorRoutes.InvalidVatNumberController.show().url)
            )
          }
        }

        "redirect to the Cannot Use Service page" when {
          "the vat number is ineligible for mtd vat" in {
            stubAuth(OK, successfulAuthResponse())
            stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(Ineligible))

            val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)

            res should have(
              httpStatus(SEE_OTHER),
              redirectUri(errorRoutes.CannotUseServiceController.show().url)
            )
          }
        }

        "redirect to the Deregistered Vat Number page" when {
          "the vat number is deregistered for mtd vat" in {
            stubAuth(OK, successfulAuthResponse())
            stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(Deregistered))

            val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)

            res should have(
              httpStatus(SEE_OTHER),
              redirectUri(errorRoutes.DeregisteredVatNumberController.show().url)
            )
          }
        }

        "redirect to the sign up after this date page" when {
          "the vat number is ineligible for mtd vat and one date is available" in {
            stubAuth(OK, successfulAuthResponse())
            stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(Inhibited(MigratableDates(Some(testStartDate)))))

            val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)

            res should have(
              httpStatus(SEE_OTHER),
              redirectUri(errorRoutes.MigratableDatesController.show().url)
            )

            getSessionMap(res).get(SessionKeys.migratableDatesKey) shouldBe defined
          }
        }

        "redirect to the sign up between these dates page" when {
          "the vat number is ineligible for mtd vat and two dates are available" in {
            stubAuth(OK, successfulAuthResponse())
            stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(Inhibited(MigratableDates(Some(testStartDate), Some(testEndDate)))))

            val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)

            res should have(
              httpStatus(SEE_OTHER),
              redirectUri(errorRoutes.MigratableDatesController.show().url)
            )

            getSessionMap(res).get(SessionKeys.migratableDatesKey) shouldBe defined
          }
        }

        "throw an internal server error" when {
          "any other failure occurs" in {
            stubAuth(OK, successfulAuthResponse())
            stubVatNumberEligibility(testVatNumber)(status = INTERNAL_SERVER_ERROR, optEligibilityResponse = None)

            val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)

            res should have(
              httpStatus(INTERNAL_SERVER_ERROR)
            )
          }
        }
      }
    }
    
  }
}
