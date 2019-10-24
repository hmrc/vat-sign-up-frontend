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
import play.api.libs.json.Json
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch.{FeatureSwitching, ReSignUpJourney}
import uk.gov.hmrc.vatsignupfrontend.forms.VatNumberForm
import uk.gov.hmrc.vatsignupfrontend.helpers.IntegrationTestConstants._
import uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks.AuthStub._
import uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks.ClaimSubscriptionStub._
import uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks.StoreVatNumberStub._
import uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks.VatNumberEligibilityStub._
import uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks.VatEligibilityStub._
import uk.gov.hmrc.vatsignupfrontend.helpers.{ComponentSpecBase, CustomMatchers, IntegrationTestConstantsGenerator, SessionCookieCrumbler}
import uk.gov.hmrc.vatsignupfrontend.models.MigratableDates

class CaptureVatNumberControllerISpec extends ComponentSpecBase with CustomMatchers with FeatureSwitching {

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
    "the ReSignUpJourney feature switch is disabled" when {
      "the user has a VAT-DEC enrolment" when {
        "the vat eligibility is successful" when {
          "the enrolment vat number matches the inserted one" should {
            "redirect to the business entity type page" in {
              stubAuth(OK, successfulAuthResponse(vatDecEnrolment))
              stubStoreVatNumberSuccess(isFromBta = false)

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
                redirectUri(routes.IncorrectEnrolmentVatNumberController.show().url)
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
                redirectUri(routes.InvalidVatNumberController.show().url)
              )
            }
          }

          "redirect to the Cannot Use Service page" when {
            "the vat number is ineligible for mtd vat" in {
              stubAuth(OK, successfulAuthResponse(vatDecEnrolment))
              stubStoreVatNumberIneligible(isFromBta = false, migratableDates = MigratableDates(None))

              val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)

              res should have(
                httpStatus(SEE_OTHER),
                redirectUri(routes.CannotUseServiceController.show().url)
              )
            }
          }

          "redirect to the sign up after this date page" when {
            "the vat number is ineligible for mtd vat and one date is available" in {
              stubAuth(OK, successfulAuthResponse(vatDecEnrolment))
              stubStoreVatNumberIneligible(isFromBta = false, migratableDates = MigratableDates(Some(testStartDate)))

              val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)

              res should have(
                httpStatus(SEE_OTHER),
                redirectUri(routes.MigratableDatesController.show().url)
              )
            }
          }

          "redirect to the sign up between these dates page" when {
            "the vat number is ineligible for mtd vat and two dates are available" in {
              stubAuth(OK, successfulAuthResponse(vatDecEnrolment))
              stubStoreVatNumberIneligible(isFromBta = false, migratableDates = MigratableDates(Some(testStartDate), Some(testEndDate)))

              val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)

              res should have(
                httpStatus(SEE_OTHER),
                redirectUri(routes.MigratableDatesController.show().url)
              )
            }
          }

          "redirect to the migration in progress error page" when {
            "the vat number is already signed up and migrating" in {
              stubAuth(OK, successfulAuthResponse(vatDecEnrolment))
              stubStoreVatNumberMigrationInProgress(isFromBta = false)

              val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)

              res should have(
                httpStatus(SEE_OTHER),
                redirectUri(routes.MigrationInProgressErrorController.show().url)
              )
            }
          }

          "redirect to the business already signed up error page" when {
            "the vat number is already signed up and a user is attempting to claim subscription" in {
              stubAuth(OK, successfulAuthResponse(vatDecEnrolment))
              stubStoreVatNumberAlreadySignedUp(isFromBta = false)
              stubClaimSubscription(testVatNumber, isFromBta = false)(CONFLICT)

              val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)

              res should have(
                httpStatus(SEE_OTHER),
                redirectUri(bta.routes.BusinessAlreadySignedUpController.show().url)
              )
            }
          }

          "redirect to the overseas resolver controller" when {
            "the vat number is overseas" in {
              stubAuth(OK, successfulAuthResponse(vatDecEnrolment))
              stubStoreVatNumberSuccess(isFromBta = false, isOverseasTrader = true)

              val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)

              res should have(
                httpStatus(SEE_OTHER),
                redirectUri(routes.OverseasResolverController.resolve().url)
              )
            }
          }

          "throw an internal server error" when {
            "any other failure occurs" in {
              stubAuth(OK, successfulAuthResponse(vatDecEnrolment))
              stubVatNumberEligibilityFailure(testVatNumber)

              val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)

              res should have(
                httpStatus(INTERNAL_SERVER_ERROR)
              )
            }
          }
        }
      }

      "the user doesn't have an enrolment" when {
        "the vat eligibility is successful" should {
          "redirect to Capture Vat Registration Date page" in {
            stubAuth(OK, successfulAuthResponse())
            stubVatNumberEligibilitySuccess(testVatNumber)

            val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)

            res should have(
              httpStatus(SEE_OTHER),
              redirectUri(routes.CaptureVatRegistrationDateController.show().url)
            )
          }

          "the vat number is eligible and overseas" should {
            "redirect to the Vat Registration Date" in {
              stubAuth(OK, successfulAuthResponse())
              stubVatNumberEligibilityOverseas(testVatNumber)

              val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)

              res should have(
                httpStatus(SEE_OTHER),
                redirectUri(routes.CaptureVatRegistrationDateController.show().url)
              )
            }
          }
        }

        "the vat eligibility is unsuccessful" when {
          "redirect to the invalid vat number page" when {
            "the vat number is fails the checksum validation" in {
              stubAuth(OK, successfulAuthResponse())
              stubVatNumberEligibilitySuccess(testVatNumber)

              val res = post("/vat-number")(VatNumberForm.vatNumber -> testInvalidVatNumber)

              res should have(
                httpStatus(SEE_OTHER),
                redirectUri(routes.InvalidVatNumberController.show().url)
              )
            }
          }

          "redirect to the invalid vat number page" when {
            "the eligibility returns the vat number as invalid" in {
              stubAuth(OK, successfulAuthResponse())
              stubVatNumberEligibilityInvalid(testVatNumber)

              val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)

              res should have(
                httpStatus(SEE_OTHER),
                redirectUri(routes.InvalidVatNumberController.show().url)
              )
            }
          }

          "redirect to the Cannot Use Service page" when {
            "the vat number is ineligible for mtd vat" in {
              stubAuth(OK, successfulAuthResponse())
              stubVatNumberIneligibleForMtd(testVatNumber)

              val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)

              res should have(
                httpStatus(SEE_OTHER),
                redirectUri(routes.CannotUseServiceController.show().url)
              )
            }
          }

          "redirect to the sign up after this date page" when {
            "the vat number is ineligible for mtd vat and one date is available" in {
              stubAuth(OK, successfulAuthResponse())
              stubVatNumberIneligibleForMtd(testVatNumber, migratableDates = MigratableDates(Some(testStartDate)))

              val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)

              res should have(
                httpStatus(SEE_OTHER),
                redirectUri(routes.MigratableDatesController.show().url)
              )

              SessionCookieCrumbler.getSessionMap(res).get(SessionKeys.migratableDatesKey) shouldBe defined
            }
          }

          "redirect to the sign up between these dates page" when {
            "the vat number is ineligible for mtd vat and two dates are available" in {
              stubAuth(OK, successfulAuthResponse())
              stubVatNumberIneligibleForMtd(testVatNumber, migratableDates = MigratableDates(Some(testStartDate), Some(testEndDate)))

              val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)

              res should have(
                httpStatus(SEE_OTHER),
                redirectUri(routes.MigratableDatesController.show().url)
              )

              SessionCookieCrumbler.getSessionMap(res).get(SessionKeys.migratableDatesKey) shouldBe defined
            }
          }

          "throw an internal server error" when {
            "any other failure occurs" in {
              stubAuth(OK, successfulAuthResponse())
              stubVatNumberEligibilityFailure(testVatNumber)

              val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)

              res should have(
                httpStatus(INTERNAL_SERVER_ERROR)
              )
            }
          }
        }
      }
    }

    "the ReSignUpJourney feature switch is enabled" when {
      "the user has a VAT-DEC enrolment" when {
        "the vat eligibility is successful" when {
          "the enrolment vat number matches the inserted one" should {
            "redirect to the business entity type page" in {
              enable(ReSignUpJourney)
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
              enable(ReSignUpJourney)
              val nonMatchingVat = IntegrationTestConstantsGenerator.randomVatNumber
              stubAuth(OK, successfulAuthResponse(vatDecEnrolment))

              val res = post("/vat-number")(VatNumberForm.vatNumber -> nonMatchingVat)

              res should have(
                httpStatus(SEE_OTHER),
                redirectUri(routes.IncorrectEnrolmentVatNumberController.show().url)
              )
            }
          }
        }

        "the vat eligibility is unsuccessful" should {
          "redirect to the invalid vat number page" when {
            "the vat number is fails the checksum validation" in {
              enable(ReSignUpJourney)
              stubAuth(OK, successfulAuthResponse(vatDecEnrolment))

              val res = post("/vat-number")(VatNumberForm.vatNumber -> testInvalidVatNumber)

              res should have(
                httpStatus(SEE_OTHER),
                redirectUri(routes.InvalidVatNumberController.show().url)
              )
            }
          }

          "redirect to the Cannot Use Service page" when {
            "the vat number is ineligible for mtd vat" in {
              enable(ReSignUpJourney)
              stubAuth(OK, successfulAuthResponse(vatDecEnrolment))
              stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(Ineligible))
              stubStoreVatNumberIneligible(isFromBta = false, migratableDates = MigratableDates(None))

              val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)

              res should have(
                httpStatus(SEE_OTHER),
                redirectUri(routes.CannotUseServiceController.show().url)
              )
            }
          }

          "redirect to the sign up after this date page" when {
            "the vat number is ineligible for mtd vat and one date is available" in {
              val testDates = MigratableDates(Some(testStartDate))
              enable(ReSignUpJourney)
              stubAuth(OK, successfulAuthResponse(vatDecEnrolment))
              stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(Inhibited(testDates)))
              stubStoreVatNumberIneligible(isFromBta = false, migratableDates = testDates)

              val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)

              res should have(
                httpStatus(SEE_OTHER),
                redirectUri(routes.MigratableDatesController.show().url)
              )
            }
          }

          "redirect to the sign up between these dates page" when {
            "the vat number is ineligible for mtd vat and two dates are available" in {
              val testDates = MigratableDates(Some(testStartDate), Some(testEndDate))
              enable(ReSignUpJourney)
              stubAuth(OK, successfulAuthResponse(vatDecEnrolment))
              stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(Inhibited(testDates)))
              stubStoreVatNumberIneligible(isFromBta = false, migratableDates = testDates)

              val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)

              res should have(
                httpStatus(SEE_OTHER),
                redirectUri(routes.MigratableDatesController.show().url)
              )
            }
          }

          "redirect to the migration in progress error page" when {
            "the vat number is already signed up and migrating" in {
              enable(ReSignUpJourney)
              stubAuth(OK, successfulAuthResponse(vatDecEnrolment))
              stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(MigrationInProgress))
              stubStoreVatNumberMigrationInProgress(isFromBta = false)

              val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)

              res should have(
                httpStatus(SEE_OTHER),
                redirectUri(routes.MigrationInProgressErrorController.show().url)
              )
            }
          }

          "redirect to the business already signed up error page" when {
            "the vat number is already signed up and a user is attempting to claim subscription" in {
              enable(ReSignUpJourney)
              stubAuth(OK, successfulAuthResponse(vatDecEnrolment))
              stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(AlreadySubscribed))
              stubStoreVatNumberAlreadySignedUp(isFromBta = false)
              stubClaimSubscription(testVatNumber, isFromBta = false)(CONFLICT)

              val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)

              res should have(
                httpStatus(SEE_OTHER),
                redirectUri(bta.routes.BusinessAlreadySignedUpController.show().url)
              )
            }
          }

          "redirect to the overseas resolver controller" when {
            "the vat number is overseas" in {
              enable(ReSignUpJourney)
              stubAuth(OK, successfulAuthResponse(vatDecEnrolment))
              stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(Overseas))
              stubStoreVatNumberSuccess(isFromBta = false, isOverseasTrader = true)

              val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)

              res should have(
                httpStatus(SEE_OTHER),
                redirectUri(routes.OverseasResolverController.resolve().url)
              )
            }
          }

          "throw an internal server error" when {
            "any other failure occurs" in {
              enable(ReSignUpJourney)
              stubAuth(OK, successfulAuthResponse(vatDecEnrolment))
              stubVatNumberEligibilityFailure(testVatNumber)

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
              enable(ReSignUpJourney)
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
              enable(ReSignUpJourney)
              val nonMatchingVat = IntegrationTestConstantsGenerator.randomVatNumber
              stubAuth(OK, successfulAuthResponse(mtdVatEnrolment))

              val res = post("/vat-number")(VatNumberForm.vatNumber -> nonMatchingVat)

              res should have(
                httpStatus(SEE_OTHER),
                redirectUri(routes.IncorrectEnrolmentVatNumberController.show().url)
              )
            }
          }
        }
      }

      "the user has both MTD-VAT and VAT-DEC enrolments" when {
        "the vat eligibility is successful" when {
          "the enrolment vat number matches the inserted one" should {
            "redirect to the business entity type page" in {
              enable(ReSignUpJourney)
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
              enable(ReSignUpJourney)
              val nonMatchingVat = IntegrationTestConstantsGenerator.randomVatNumber
              stubAuth(OK, successfulAuthResponse(mtdVatEnrolment, vatDecEnrolment))

              val res = post("/vat-number")(VatNumberForm.vatNumber -> nonMatchingVat)

              res should have(
                httpStatus(SEE_OTHER),
                redirectUri(routes.IncorrectEnrolmentVatNumberController.show().url)
              )
            }
          }
        }
      }

      "the user doesn't have an enrolment" when {
        "the vat eligibility is successful" should {
          "redirect to Capture Vat Registration Date page" in {
            enable(ReSignUpJourney)
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
              enable(ReSignUpJourney)
              stubAuth(OK, successfulAuthResponse())
              stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(Overseas))

              val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)

              res should have(
                httpStatus(SEE_OTHER),
                redirectUri(routes.CaptureVatRegistrationDateController.show().url)
              )
            }
          }
        }

        "the vat eligibility is unsuccessful" when {
          "redirect to the invalid vat number page" when {
            "the vat number is fails the checksum validation" in {
              enable(ReSignUpJourney)
              stubAuth(OK, successfulAuthResponse())

              val res = post("/vat-number")(VatNumberForm.vatNumber -> testInvalidVatNumber)

              res should have(
                httpStatus(SEE_OTHER),
                redirectUri(routes.InvalidVatNumberController.show().url)
              )
            }
          }

          "redirect to the Cannot Use Service page" when {
            "the vat number is ineligible for mtd vat" in {
              enable(ReSignUpJourney)
              stubAuth(OK, successfulAuthResponse())
              stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(Ineligible))

              val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)

              res should have(
                httpStatus(SEE_OTHER),
                redirectUri(routes.CannotUseServiceController.show().url)
              )
            }
          }

          "redirect to the sign up after this date page" when {
            "the vat number is ineligible for mtd vat and one date is available" in {
              enable(ReSignUpJourney)
              stubAuth(OK, successfulAuthResponse())
              stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(Inhibited(MigratableDates(Some(testStartDate)))))

              val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)

              res should have(
                httpStatus(SEE_OTHER),
                redirectUri(routes.MigratableDatesController.show().url)
              )

              SessionCookieCrumbler.getSessionMap(res).get(SessionKeys.migratableDatesKey) shouldBe defined
            }
          }

          "redirect to the sign up between these dates page" when {
            "the vat number is ineligible for mtd vat and two dates are available" in {
              enable(ReSignUpJourney)
              stubAuth(OK, successfulAuthResponse())
              stubVatNumberEligibility(testVatNumber)(status = OK, optEligibilityResponse = Some(Inhibited(MigratableDates(Some(testStartDate), Some(testEndDate)))))

              val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)

              res should have(
                httpStatus(SEE_OTHER),
                redirectUri(routes.MigratableDatesController.show().url)
              )

              SessionCookieCrumbler.getSessionMap(res).get(SessionKeys.migratableDatesKey) shouldBe defined
            }
          }

          "throw an internal server error" when {
            "any other failure occurs" in {
              enable(ReSignUpJourney)
              stubAuth(OK, successfulAuthResponse())

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
}