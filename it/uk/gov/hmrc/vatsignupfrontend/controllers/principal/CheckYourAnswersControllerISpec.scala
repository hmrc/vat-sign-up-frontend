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

import java.time.LocalDate

import play.api.http.Status._
import play.api.libs.json.Json
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch.FeatureSwitching
import uk.gov.hmrc.vatsignupfrontend.controllers.principal.error.{routes => errorRoutes}
import uk.gov.hmrc.vatsignupfrontend.helpers.IntegrationTestConstants._
import uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks.AuthStub._
import uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks.ClaimSubscriptionStub.stubClaimSubscription
import uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks.StoreOverseasInformationStub.stubStoreOverseasInformation
import uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks.StoreVatNumberStub._
import uk.gov.hmrc.vatsignupfrontend.helpers.{ComponentSpecBase, CustomMatchers}
import uk.gov.hmrc.vatsignupfrontend.models.{DateModel, MigratableDates, Overseas}

class CheckYourAnswersControllerISpec extends ComponentSpecBase with CustomMatchers with FeatureSwitching {

  val testDate: DateModel = DateModel.dateConvert(LocalDate.now())

  "GET /check-your-answers" should {
    "return an OK" in {
      stubAuth(OK, successfulAuthResponse())

      val res = get("/check-your-answers",
        Map(
          SessionKeys.vatNumberKey -> testVatNumber,
          SessionKeys.vatRegistrationDateKey -> Json.toJson(testDate).toString(),
          SessionKeys.businessPostCodeKey -> Json.toJson(testBusinessPostCode).toString(),
          SessionKeys.box5FigureKey -> testBox5Figure,
          SessionKeys.lastReturnMonthPeriodKey -> testLastReturnMonth,
          SessionKeys.previousVatReturnKey -> testPreviousVatSubmitted
        )
      )

      res should have(
        httpStatus(OK)
      )
    }
  }

  "POST /check-your-answers" when {
    "store vat is successful when not overseas " should {
      "redirect to business entity" in {
        stubAuth(OK, successfulAuthResponse())
        stubStoreVatNumberSuccess(
          Some(testBusinessPostCode),
          testDate,
          Some(testBox5Figure),
          Some(testLastReturnMonth),
          isFromBta = false
        )

        val res = post("/check-your-answers",
          Map(
            SessionKeys.vatNumberKey -> testVatNumber,
            SessionKeys.vatRegistrationDateKey -> Json.toJson(testDate).toString(),
            SessionKeys.businessPostCodeKey -> Json.toJson(testBusinessPostCode).toString(),
            SessionKeys.box5FigureKey -> testBox5Figure,
            SessionKeys.lastReturnMonthPeriodKey -> testLastReturnMonth,
            SessionKeys.previousVatReturnKey -> testPreviousVatSubmitted
          )
        )()

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(routes.CaptureBusinessEntityController.show().url)
        )
      }
    }

    "store vat is successful with an overseas VRN" should {
      "redirect to business entity" in {
        stubAuth(OK, successfulAuthResponse())
        stubStoreVatNumberSuccess(
          None,
          testDate,
          Some(testBox5Figure),
          Some(testLastReturnMonth),
          isFromBta = false
        )
        stubStoreOverseasInformation(testVatNumber)(NO_CONTENT)

        val res = post("/check-your-answers",
          Map(
            SessionKeys.vatNumberKey -> testVatNumber,
            SessionKeys.vatRegistrationDateKey -> Json.toJson(testDate).toString(),
            SessionKeys.businessPostCodeKey -> Json.toJson(testBusinessPostCode).toString(),
            SessionKeys.box5FigureKey -> testBox5Figure,
            SessionKeys.lastReturnMonthPeriodKey -> testLastReturnMonth,
            SessionKeys.previousVatReturnKey -> testPreviousVatSubmitted,
            SessionKeys.businessEntityKey -> Overseas.toString
          )
        )()

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(routes.CaptureBusinessEntityController.show().url)
        )

        getSessionMap(res).get(SessionKeys.businessEntityKey) should contain(Overseas.toString)
      }
    }

    "isAlreadySubscribed is in session" when {
      "the VAT subscription has been claimed" should {
        "redirect to sign up complete client" in {
          stubAuth(OK, successfulAuthResponse())
          stubClaimSubscription(
            testVatNumber,
            Some(testBusinessPostCode),
            testDate,
            isFromBta = false
          )(NO_CONTENT)

          val res = post("/check-your-answers",
            Map(
              SessionKeys.vatNumberKey -> testVatNumber,
              SessionKeys.vatRegistrationDateKey -> Json.toJson(testDate).toString(),
              SessionKeys.businessPostCodeKey -> Json.toJson(testBusinessPostCode).toString(),
              SessionKeys.isAlreadySubscribedKey -> "true"
            )
          )()

          res should have(
            httpStatus(SEE_OTHER),
            redirectUri(routes.SignUpCompleteClientController.show().url)
          )

          getSessionMap(res).get(SessionKeys.businessEntityKey) shouldNot contain(Overseas.toString)
        }
      }

      "the VAT subscription has been claimed and the user is overseas" should {
        "redirect to sign up complete client" in {
          stubAuth(OK, successfulAuthResponse())
          stubClaimSubscription(
            testVatNumber,
            Some(testBusinessPostCode),
            testDate,
            isFromBta = false
          )(NO_CONTENT)

          val res = post("/check-your-answers",
            Map(
              SessionKeys.vatNumberKey -> testVatNumber,
              SessionKeys.vatRegistrationDateKey -> Json.toJson(testDate).toString(),
              SessionKeys.businessPostCodeKey -> Json.toJson(testBusinessPostCode).toString(),
              SessionKeys.isAlreadySubscribedKey -> "true",
              SessionKeys.businessEntityKey -> Overseas.toString
            )
          )()

          res should have(
            httpStatus(SEE_OTHER),
            redirectUri(routes.SignUpCompleteClientController.show().url)
          )
        }
      }


      "the VAT subscription has been claimed on another cred" should {
        "redirect to business already signed up error page" in {
          stubAuth(OK, successfulAuthResponse())
          stubClaimSubscription(
            testVatNumber,
            Some(testBusinessPostCode),
            testDate,
            isFromBta = false
          )(CONFLICT)

          val res = post("/check-your-answers",
            Map(
              SessionKeys.vatNumberKey -> testVatNumber,
              SessionKeys.vatRegistrationDateKey -> Json.toJson(testDate).toString(),
              SessionKeys.businessPostCodeKey -> Json.toJson(testBusinessPostCode).toString(),
              SessionKeys.isAlreadySubscribedKey -> "true"
            )
          )()

          res should have(
            httpStatus(SEE_OTHER),
            redirectUri(errorRoutes.BusinessAlreadySignedUpController.show().url)
          )
        }
      }

      "store vat returned known facts mismatch" should {
        "redirect to could not confirm business" in {
          stubAuth(OK, successfulAuthResponse())
          stubClaimSubscription(
            testVatNumber,
            Some(testBusinessPostCode),
            testDate,
            isFromBta = false
          )(FORBIDDEN)

          val res = post("/check-your-answers",
            Map(
              SessionKeys.vatNumberKey -> testVatNumber,
              SessionKeys.vatRegistrationDateKey -> Json.toJson(testDate).toString(),
              SessionKeys.businessPostCodeKey -> Json.toJson(testBusinessPostCode).toString(),
              SessionKeys.isAlreadySubscribedKey -> "true"
            )
          )()

          res should have(
            httpStatus(SEE_OTHER),
            redirectUri(errorRoutes.VatCouldNotConfirmBusinessController.show().url)
          )
        }
      }
    }

    "the VAT subscription has been claimed" should {
      "redirect to sign up complete client" in {
        stubAuth(OK, successfulAuthResponse())
        stubStoreVatNumberAlreadySignedUp(
          Some(testBusinessPostCode),
          testDate,
          Some(testBox5Figure),
          Some(testLastReturnMonth),
          isFromBta = false
        )

        stubClaimSubscription(testVatNumber, Some(testBusinessPostCode), testDate, isFromBta = false)(NO_CONTENT)

        val res = post("/check-your-answers",
          Map(
            SessionKeys.vatNumberKey -> testVatNumber,
            SessionKeys.vatRegistrationDateKey -> Json.toJson(testDate).toString(),
            SessionKeys.businessPostCodeKey -> Json.toJson(testBusinessPostCode).toString(),
            SessionKeys.box5FigureKey -> testBox5Figure,
            SessionKeys.previousVatReturnKey -> testPreviousVatSubmitted,
            SessionKeys.lastReturnMonthPeriodKey -> testLastReturnMonth
          )
        )()

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(routes.SignUpCompleteClientController.show().url)
        )

        getSessionMap(res).get(SessionKeys.businessEntityKey) shouldNot contain(Overseas.toString)
      }
    }

    "the VAT subscription has been claimed on another cred" should {
      "redirect to business already signed up error page" in {
        stubAuth(OK, successfulAuthResponse())
        stubStoreVatNumberAlreadySignedUp(
          Some(testBusinessPostCode),
          testDate,
          Some(testBox5Figure),
          Some(testLastReturnMonth),
          isFromBta = false
        )

        stubClaimSubscription(testVatNumber, Some(testBusinessPostCode), testDate, isFromBta = false)(CONFLICT)

        val res = post("/check-your-answers",
          Map(
            SessionKeys.vatNumberKey -> testVatNumber,
            SessionKeys.vatRegistrationDateKey -> Json.toJson(testDate).toString(),
            SessionKeys.businessPostCodeKey -> Json.toJson(testBusinessPostCode).toString(),
            SessionKeys.box5FigureKey -> testBox5Figure,
            SessionKeys.previousVatReturnKey -> testPreviousVatSubmitted,
            SessionKeys.lastReturnMonthPeriodKey -> testLastReturnMonth
          )
        )()

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(errorRoutes.BusinessAlreadySignedUpController.show().url)
        )
      }
    }

    "store vat returned known facts mismatch" should {
      "redirect to could not confirm business" in {
        stubAuth(OK, successfulAuthResponse())
        stubStoreVatNumberKnownFactsMismatch(
          Some(testBusinessPostCode),
          testDate,
          Some(testBox5Figure),
          Some(testLastReturnMonth),
          isFromBta = false
        )

        val res = post("/check-your-answers",
          Map(
            SessionKeys.vatNumberKey -> testVatNumber,
            SessionKeys.vatRegistrationDateKey -> Json.toJson(testDate).toString(),
            SessionKeys.businessPostCodeKey -> Json.toJson(testBusinessPostCode).toString(),
            SessionKeys.box5FigureKey -> testBox5Figure,
            SessionKeys.previousVatReturnKey -> testPreviousVatSubmitted,
            SessionKeys.lastReturnMonthPeriodKey -> testLastReturnMonth
          )
        )()

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(errorRoutes.VatCouldNotConfirmBusinessController.show().url)
        )
      }
    }

    "store vat returned invalid vat number" should {
      "redirect to invalid vat number" in {
        stubAuth(OK, successfulAuthResponse())
        stubStoreVatNumberInvalid(
          Some(testBusinessPostCode),
          testDate,
          Some(testBox5Figure),
          Some(testLastReturnMonth),
          isFromBta = false
        )

        val res = post("/check-your-answers",
          Map(
            SessionKeys.vatNumberKey -> testVatNumber,
            SessionKeys.vatRegistrationDateKey -> Json.toJson(testDate).toString(),
            SessionKeys.businessPostCodeKey -> Json.toJson(testBusinessPostCode).toString(),
            SessionKeys.box5FigureKey -> testBox5Figure,
            SessionKeys.previousVatReturnKey -> testPreviousVatSubmitted,
            SessionKeys.lastReturnMonthPeriodKey -> testLastReturnMonth
          )
        )()

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(errorRoutes.InvalidVatNumberController.show().url)
        )
      }
    }

    "store vat returned ineligible vat number" should {
      "redirect to cannot use service" in {
        stubAuth(OK, successfulAuthResponse())
        stubStoreVatNumberIneligible(
          Some(testBusinessPostCode),
          testDate,
          Some(testBox5Figure),
          Some(testLastReturnMonth),
          isFromBta = false,
          MigratableDates()
        )

        val res = post("/check-your-answers",
          Map(
            SessionKeys.vatNumberKey -> testVatNumber,
            SessionKeys.vatRegistrationDateKey -> Json.toJson(testDate).toString(),
            SessionKeys.businessPostCodeKey -> Json.toJson(testBusinessPostCode).toString(),
            SessionKeys.box5FigureKey -> testBox5Figure,
            SessionKeys.lastReturnMonthPeriodKey -> testLastReturnMonth,
            SessionKeys.previousVatReturnKey -> testPreviousVatSubmitted
          )
        )()

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(errorRoutes.CannotUseServiceController.show().url)
        )
      }
    }

    "store vat returned vat migration in progress" should {
      "redirect to migration in progress error page" in {
        stubAuth(OK, successfulAuthResponse())
        stubStoreVatNumberMigrationInProgress(
          Some(testBusinessPostCode),
          testDate,
          Some(testBox5Figure),
          Some(testLastReturnMonth),
          isFromBta = false
        )

        val res = post("/check-your-answers",
          Map(
            SessionKeys.vatNumberKey -> testVatNumber,
            SessionKeys.vatRegistrationDateKey -> Json.toJson(testDate).toString(),
            SessionKeys.businessPostCodeKey -> Json.toJson(testBusinessPostCode).toString(),
            SessionKeys.box5FigureKey -> testBox5Figure,
            SessionKeys.lastReturnMonthPeriodKey -> testLastReturnMonth,
            SessionKeys.previousVatReturnKey -> testPreviousVatSubmitted
          )
        )()

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(errorRoutes.MigrationInProgressErrorController.show().url)
        )
      }
    }

    "handle attempting to store a Unenrolled migrated vat number" should {
      "redirect to the correct location if the known facts mismatch" in {
        stubAuth(OK, successfulAuthResponse())
        stubStoreVatNumberKnownFactsMismatchMigrated(
          testVatNumber,
          testDate,
          Some(testBusinessPostCode)
        )


        val res = post("/check-your-answers",
          Map(
            SessionKeys.vatNumberKey -> testVatNumber,
            SessionKeys.vatRegistrationDateKey -> Json.toJson(testDate).toString(),
            SessionKeys.businessPostCodeKey -> Json.toJson(testBusinessPostCode).toString(),
            SessionKeys.isMigratedKey -> "true"
          )
        )()

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(errorRoutes.VatCouldNotConfirmBusinessController.show().toString)
        )
      }

      "throw an error when a failure to store the vat number occurs" in {
        stubAuth(OK, successfulAuthResponse())
        stubStoreMigratedVatnumberKF(
          testVatNumber,
          testDate.toDesDateFormat,
          testBusinessPostCode
        )(INTERNAL_SERVER_ERROR)

        val res = post("/check-your-answers",
          Map(
            SessionKeys.vatNumberKey -> testVatNumber,
            SessionKeys.vatRegistrationDateKey -> Json.toJson(testDate).toString(),
            SessionKeys.businessPostCodeKey -> Json.toJson(testBusinessPostCode).toString(),
            SessionKeys.isMigratedKey -> "true"
          )
        )()

        res should have(
          httpStatus(INTERNAL_SERVER_ERROR)
        )
      }

      "redirect to business entity when a vat number is successfully stored" in {
        stubAuth(OK, successfulAuthResponse())
        stubStoreMigratedVatnumberKF(
          testVatNumber,
          testDate.toDesDateFormat,
          testBusinessPostCode
        )(OK)

        val res = post("/check-your-answers",
          Map(
            SessionKeys.vatNumberKey -> testVatNumber,
            SessionKeys.vatRegistrationDateKey -> Json.toJson(testDate).toString(),
            SessionKeys.businessPostCodeKey -> Json.toJson(testBusinessPostCode).toString(),
            SessionKeys.isMigratedKey -> "true"
          )
        )()

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(routes.CaptureBusinessEntityController.show().url)
        )
      }
    }

    "handle attempting to store a Unenrolled migrated overseas vat number" should {
      "redirect to the correct location if the known facts mismatch" in {
        stubAuth(OK, successfulAuthResponse())
        stubStoreVatNumberKnownFactsMismatchMigrated(
          testVatNumber,
          testDate,
          None
        )


        val res = post("/check-your-answers",
          Map(
            SessionKeys.vatNumberKey -> testVatNumber,
            SessionKeys.vatRegistrationDateKey -> Json.toJson(testDate).toString(),
            SessionKeys.businessEntityKey -> Overseas.toString,
            SessionKeys.isMigratedKey -> "true"
          )
        )()

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(errorRoutes.VatCouldNotConfirmBusinessController.show().toString)
        )
      }

      "throw an error when a failure to store the vat number occurs" in {
        stubAuth(OK, successfulAuthResponse())
        stubStoreMigratedOverseasVatnumberKF(
          testVatNumber,
          testDate.toDesDateFormat
        )(INTERNAL_SERVER_ERROR)

        val res = post("/check-your-answers",
          Map(
            SessionKeys.vatNumberKey -> testVatNumber,
            SessionKeys.vatRegistrationDateKey -> Json.toJson(testDate).toString(),
            SessionKeys.businessEntityKey -> Overseas.toString,
            SessionKeys.isMigratedKey -> "true"
          )
        )()

        res should have(
          httpStatus(INTERNAL_SERVER_ERROR)
        )
      }

      "redirect to business entity when a vat number is successfully stored" in {
        stubAuth(OK, successfulAuthResponse())
        stubStoreMigratedOverseasVatnumberKF(
          testVatNumber,
          testDate.toDesDateFormat
        )(OK)

        val res = post("/check-your-answers",
          Map(
            SessionKeys.vatNumberKey -> testVatNumber,
            SessionKeys.vatRegistrationDateKey -> Json.toJson(testDate).toString(),
            SessionKeys.businessEntityKey -> Overseas.toString,
            SessionKeys.isMigratedKey -> "true"
          )
        )()

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(routes.CaptureBusinessEntityController.show().url)
        )
      }
    }
  }
}
