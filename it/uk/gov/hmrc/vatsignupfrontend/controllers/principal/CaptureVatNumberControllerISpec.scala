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
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch.FeatureSwitching
import uk.gov.hmrc.vatsignupfrontend.forms.VatNumberForm
import uk.gov.hmrc.vatsignupfrontend.helpers.IntegrationTestConstants._
import uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks.AuthStub._
import uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks.StoreVatNumberStub.stubStoreVatNumberSuccess
import uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks.VatEligibilityStub._
import uk.gov.hmrc.vatsignupfrontend.helpers.{ComponentSpecBase, CustomMatchers, IntegrationTestConstantsGenerator}
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

    "we have an enrolment" when {
      "the vat eligibility is successful" when {
        "the enrolment vat number matches the inserted one" should {
          "redirect to the business entity type page" in {
            stubAuth(OK, successfulAuthResponse(vatDecEnrolment))
            stubStoreVatNumberSuccess(isFromBta = Some(false))

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
            stubAuth(OK, successfulAuthResponse())

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

        "redirect to the Already Signed up page" when {
          "the vat number is already signed up" in {
            stubAuth(OK, successfulAuthResponse())
            stubVatNumberEligibilityAlreadySubscribed(testVatNumber)

            val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)

            res should have(
              httpStatus(SEE_OTHER),
              redirectUri(routes.AlreadySignedUpController.show().url)
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
    "we don't have an enrolment" when {
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

      }
      "the vat eligibility is unsuccessful" when {
        "redirect to the invalid vat number page" when {
          "the vat number is fails the checksum validation" in {
            stubAuth(OK, successfulAuthResponse())
            stubVatNumberEligibilitySuccess(testInvalidVatNumber)

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

        "redirect to the Already Signed up page" when {
          "the vat number is already signed up" in {
            stubAuth(OK, successfulAuthResponse())
            stubVatNumberEligibilityAlreadySubscribed(testVatNumber)

            val res = post("/vat-number")(VatNumberForm.vatNumber -> testVatNumber)

            res should have(
              httpStatus(SEE_OTHER),
              redirectUri(routes.AlreadySignedUpController.show().url)
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

}
