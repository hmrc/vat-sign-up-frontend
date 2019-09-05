/*
 * Copyright 2019 HM Revenue & Customs
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

import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.libs.json.Json
import play.api.mvc.AnyContentAsFormUrlEncoded
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.retrieve.{Retrievals, ~}
import uk.gov.hmrc.auth.core.{Admin, Enrolments}
import uk.gov.hmrc.http.InternalServerException
import uk.gov.hmrc.play.test.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.SessionKeys._
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch.FeatureSwitching
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockControllerComponents
import uk.gov.hmrc.vatsignupfrontend.forms.VatNumberForm
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstantsGenerator
import uk.gov.hmrc.vatsignupfrontend.models.{DateModel, MigratableDates, Overseas, Yes}
import uk.gov.hmrc.vatsignupfrontend.services.mocks.{MockAdministrativeDivisionLookupService, MockStoreVatNumberService,
  MockVatNumberEligibilityService}

import scala.concurrent.Future

class CaptureVatNumberControllerSpec extends UnitSpec with GuiceOneAppPerSuite
  with MockControllerComponents with MockVatNumberEligibilityService
  with MockStoreVatNumberService with MockAdministrativeDivisionLookupService with FeatureSwitching {

  object TestCaptureVatNumberController extends CaptureVatNumberController(
    mockControllerComponents,
    mockVatNumberEligibilityService,
    mockStoreVatNumberService,
    mockAdministrativeDivisionLookupService
  )

  lazy val testGetRequest = FakeRequest("GET", "/vat-number")

  def testPostRequest(vatNumber: String): FakeRequest[AnyContentAsFormUrlEncoded] =
    FakeRequest("POST", "/vat-number").withFormUrlEncodedBody(VatNumberForm.vatNumber -> vatNumber)

  "Calling the show action of the Capture Vat Number controller" when {
    "redirect to resolve VAT number controller" in {
      mockAuthAdminRole()

      val result = TestCaptureVatNumberController.show(testGetRequest)

      status(result) shouldBe Status.OK
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
    }

  }

  "Calling the submit action of the Capture Vat Number controller" when {
    "form successfully submitted" when {
      "the vat number passes checksum validation" when {
        "the user has a VAT-DEC enrolment" when {
          "the vat eligibility is successful" when {
            "the inserted vat number matches the enrolment one" when {
              "the VAT number is not overseas and is not a VAT division and is stored successfully" should {
                "redirect to the business entity type page" in {
                  mockAuthRetrieveVatDecEnrolment(hasIRSAEnrolment = false)
                  mockStoreVatNumberSuccess(testVatNumber, isFromBta = false)
                  mockIsAdministrativeDivision(testVatNumber)(false)

                  val result = TestCaptureVatNumberController.submit(testPostRequest(testVatNumber))

                  status(result) shouldBe Status.SEE_OTHER
                  redirectLocation(result) shouldBe Some(routes.CaptureBusinessEntityController.show().url)
                  session(result) get vatNumberKey should contain(testVatNumber)
                }
              }

              "the VAT number is not overseas but is a VAT division and is stored successfully" should {
                "redirect to the division resolver controller resolve action" in {
                  mockAuthRetrieveVatDecEnrolment(hasIRSAEnrolment = false)
                  mockStoreVatNumberSuccess(testVatNumber, isFromBta = false)
                  mockIsAdministrativeDivision(testVatNumber)(true)

                  val result = TestCaptureVatNumberController.submit(testPostRequest(testVatNumber))

                  status(result) shouldBe Status.SEE_OTHER
                  redirectLocation(result) shouldBe Some(routes.DivisionResolverController.resolve().url)
                  session(result) get vatNumberKey should contain(testVatNumber)
                }
              }

              "the VAT number is overseas and is stored successfully" should {
                "redirect to the overseas resolver controller" in {
                  mockAuthRetrieveVatDecEnrolment(hasIRSAEnrolment = false)
                  mockStoreVatNumberOverseasSuccess(testVatNumber, isFromBta = false)

                  val result = TestCaptureVatNumberController.submit(testPostRequest(testVatNumber))

                  status(result) shouldBe Status.SEE_OTHER
                  redirectLocation(result) shouldBe Some(routes.OverseasResolverController.resolve().url)
                  session(result) get vatNumberKey should contain(testVatNumber)
                }
              }

              "the user's information is being migrated" should {
                "redirect to migration in progress error page" in {
                  mockAuthRetrieveVatDecEnrolment(hasIRSAEnrolment = false)
                  mockStoreVatNumberMigrationInProgress(testVatNumber, isFromBta = false)

                  val result = TestCaptureVatNumberController.submit(testPostRequest(testVatNumber))

                  status(result) shouldBe Status.SEE_OTHER
                  redirectLocation(result) shouldBe Some(routes.MigrationInProgressErrorController.show().url)
                }
              }

              "the user's subscription has been claimed" should {
                "redirect to claimed subscription confirmation page" in {
                  mockAuthRetrieveVatDecEnrolment(hasIRSAEnrolment = false)
                  mockStoreVatNumberSubscriptionClaimed(testVatNumber, isFromBta = false)

                  val result = TestCaptureVatNumberController.submit(testPostRequest(testVatNumber))

                  status(result) shouldBe Status.SEE_OTHER
                  redirectLocation(result) shouldBe Some(routes.SignUpCompleteClientController.show().url)
                }
              }

              "the user tried to claim a subscription that is already enrolled" should {
                "redirect to business already signed up page" in {
                  mockAuthRetrieveVatDecEnrolment(hasIRSAEnrolment = false)
                  mockStoreVatNumberAlreadyEnrolled(testVatNumber, isFromBta = false)

                  val result = TestCaptureVatNumberController.submit(testPostRequest(testVatNumber))

                  status(result) shouldBe Status.SEE_OTHER
                  redirectLocation(result) shouldBe Some(bta.routes.BusinessAlreadySignedUpController.show().url)
                }
              }
            }

            "the inserted vat number doesn't match the enrolment one" should {
              "redirect to error page" in {
                val testNonMatchingVat = TestConstantsGenerator.randomVatNumber
                mockAuthRetrieveVatDecEnrolment(hasIRSAEnrolment = false)

                val result = TestCaptureVatNumberController.submit(testPostRequest(testNonMatchingVat))

                status(result) shouldBe Status.SEE_OTHER
                redirectLocation(result) shouldBe Some(routes.IncorrectEnrolmentVatNumberController.show().url)
              }
            }
          }

          "the vat eligibility is unsuccessful" should {
            "redirect to Cannot use service yet when the vat number is ineligible for Making Tax Digital" in {
              mockAuthRetrieveVatDecEnrolment(hasIRSAEnrolment = false)
              mockStoreVatNumberIneligible(testVatNumber, isFromBta = false, migratableDates = MigratableDates())

              val request = testPostRequest(testVatNumber)

              val result = TestCaptureVatNumberController.submit(request)
              status(result) shouldBe Status.SEE_OTHER
              redirectLocation(result) shouldBe Some(routes.CannotUseServiceController.show().url)
            }

            "the vat eligibility is unsuccessful" should {
              "redirect to sign up after this date page when the vat number is ineligible and one date is available" in {
                val testDates = MigratableDates(Some(testStartDate))

                mockAuthRetrieveVatDecEnrolment(hasIRSAEnrolment = false)
                mockStoreVatNumberIneligible(testVatNumber, isFromBta = false, migratableDates = testDates)

                val request = testPostRequest(testVatNumber)

                val result = TestCaptureVatNumberController.submit(request)
                status(result) shouldBe Status.SEE_OTHER
                redirectLocation(result) shouldBe Some(routes.MigratableDatesController.show().url)

                await(result).session(request).get(SessionKeys.migratableDatesKey) shouldBe Some(Json.toJson(testDates).toString)
              }
            }

            "the vat eligibility is unsuccessful" should {
              "redirect to sign up between these dates page when the vat number is ineligible and two dates are available" in {
                val testDates = MigratableDates(Some(testStartDate), Some(testEndDate))

                mockAuthRetrieveVatDecEnrolment(hasIRSAEnrolment = false)
                mockStoreVatNumberIneligible(testVatNumber, isFromBta = false, migratableDates = testDates)

                val request = testPostRequest(testVatNumber)

                val result = TestCaptureVatNumberController.submit(request)
                status(result) shouldBe Status.SEE_OTHER
                redirectLocation(result) shouldBe Some(routes.MigratableDatesController.show().url)

                await(result).session(request).get(SessionKeys.migratableDatesKey) shouldBe Some(Json.toJson(testDates).toString)
              }
            }
          }
        }

        "the user has an MTD-VAT enrolment" when {
          "the user attempts to sign up the same vat number that is already on their enrolment" in {
            mockAuthRetrieveMtdVatEnrolment()

            val request = testPostRequest(testVatNumber)

            val result = TestCaptureVatNumberController.submit(request)

            status(result) shouldBe Status.SEE_OTHER
            redirectLocation(result) shouldBe Some(routes.AlreadySignedUpController.show().url)
          }

          "the user attempts to sign up a different vat number" in {
            mockAuthRetrieveMtdVatEnrolment()

            val request = testPostRequest(TestConstantsGenerator.randomVatNumber)

            val result = TestCaptureVatNumberController.submit(request)

            status(result) shouldBe Status.SEE_OTHER
            redirectLocation(result) shouldBe Some(routes.CannotSignUpAnotherAccountController.show().url)
          }
        }

        "the user has an MTD-VAT enrolment and a VAT-DEC enrolment" should {
          "display the already signed up error page" when {
            "the user attempts to sign up the same vat number that is already on their enrolment" in {
              mockAuthRetrieveAllVatEnrolments()

              val request = testPostRequest(testVatNumber)

              val result = TestCaptureVatNumberController.submit(request)

              status(result) shouldBe Status.SEE_OTHER
              redirectLocation(result) shouldBe Some(routes.AlreadySignedUpController.show().url)
            }
          }

          "display the cannot sign up another account error page" when {
            "the user attempts to sign up a different vat number" in {
              mockAuthRetrieveAllVatEnrolments()

              val request = testPostRequest(TestConstantsGenerator.randomVatNumber)

              val result = TestCaptureVatNumberController.submit(request)

              status(result) shouldBe Status.SEE_OTHER
              redirectLocation(result) shouldBe Some(routes.CannotSignUpAnotherAccountController.show().url)
            }
          }
        }

        "the user does not have a VAT-DEC enrolment" when {
          "the Vat number is not for an overseas business" should {
            "redirect to the Capture Vat Registration Date page when the vat number is eligible" in {
              mockAuthRetrieveEmptyEnrolment()
              mockVatNumberEligibilitySuccess(testVatNumber)

              implicit val request = testPostRequest(testVatNumber)

              val result = TestCaptureVatNumberController.submit(request)
              status(result) shouldBe Status.SEE_OTHER
              redirectLocation(result) shouldBe Some(routes.CaptureVatRegistrationDateController.show().url)

              result.session get vatNumberKey should contain(testVatNumber)
            }

            "delete the known facts when they have been entered previously" in {
              mockAuthRetrieveEmptyEnrolment()
              mockVatNumberEligibilitySuccess(testVatNumber)

              implicit val request = testPostRequest(testVatNumber)
                .withSession(
                  vatRegistrationDateKey -> Json.toJson(DateModel.dateConvert(LocalDate.now())).toString,
                  businessPostCodeKey -> testBusinessPostcode.postCode,
                  previousVatReturnKey -> Yes.stringValue,
                  lastReturnMonthPeriodKey -> testLastReturnMonthPeriod,
                  box5FigureKey -> testBox5Figure
                )

              val result = TestCaptureVatNumberController.submit(request)
              status(result) shouldBe Status.SEE_OTHER
              redirectLocation(result) shouldBe Some(routes.CaptureVatRegistrationDateController.show().url)

              result.session get vatNumberKey should contain(testVatNumber)
              result.session get vatRegistrationDateKey shouldBe empty
              result.session get businessPostCodeKey shouldBe empty
              result.session get previousVatReturnKey shouldBe empty
              result.session get lastReturnMonthPeriodKey shouldBe empty
              result.session get box5FigureKey shouldBe empty
            }

            "overseas user changes vat number to non overseas vat number" in {
              mockAuthRetrieveEmptyEnrolment()
              mockVatNumberEligibilitySuccess(testVatNumber)

              implicit val request = testPostRequest(testVatNumber)
                .withSession(
                  vatRegistrationDateKey -> Json.toJson(DateModel.dateConvert(LocalDate.now())).toString,
                  businessPostCodeKey -> testBusinessPostcode.postCode,
                  previousVatReturnKey -> Yes.stringValue,
                  businessEntityKey -> Overseas.toString,
                  lastReturnMonthPeriodKey -> testLastReturnMonthPeriod,
                  box5FigureKey -> testBox5Figure
                )

              val result = TestCaptureVatNumberController.submit(request)
              status(result) shouldBe Status.SEE_OTHER
              redirectLocation(result) shouldBe Some(routes.CaptureVatRegistrationDateController.show().url)

              result.session get vatNumberKey should contain(testVatNumber)
              result.session get vatRegistrationDateKey shouldBe empty
              result.session get businessPostCodeKey shouldBe empty
              result.session get businessEntityKey shouldBe empty
              result.session get previousVatReturnKey shouldBe empty
              result.session get lastReturnMonthPeriodKey shouldBe empty
              result.session get box5FigureKey shouldBe empty
            }
          }

          "the vat number is for an overseas business" should {
            "redirect to the Capture Vat registration date controller" in {
              mockAuthRetrieveEmptyEnrolment()
              mockVatNumberEligibilityOverseas(testVatNumber)

              implicit val request = testPostRequest(testVatNumber)

              val result = TestCaptureVatNumberController.submit(request)
              status(result) shouldBe Status.SEE_OTHER
              redirectLocation(result) shouldBe Some(routes.CaptureVatRegistrationDateController.show().url)

              result.session get vatNumberKey should contain(testVatNumber)
              result.session get businessEntityKey should contain(Overseas.toString)
            }
          }

          "redirect to Cannot use service yet when the vat number is ineligible for Making Tax Digital" in {
            mockAuthRetrieveEmptyEnrolment()
            mockVatNumberIneligibleForMtd(testVatNumber)

            val request = testPostRequest(testVatNumber)

            val result = TestCaptureVatNumberController.submit(request)
            status(result) shouldBe Status.SEE_OTHER
            redirectLocation(result) shouldBe Some(routes.CannotUseServiceController.show().url)
          }

          "redirect to sign up after this date when the vat number is ineligible and one date is available" in {
            mockAuthRetrieveEmptyEnrolment()
            mockVatNumberIneligibleForMtd(testVatNumber, migratableDates = MigratableDates(Some(testStartDate)))

            val request = testPostRequest(testVatNumber)

            val result = TestCaptureVatNumberController.submit(request)
            status(result) shouldBe Status.SEE_OTHER
            redirectLocation(result) shouldBe Some(routes.MigratableDatesController.show().url)
          }

          "redirect to sign up between these dates when the vat number is ineligible and two dates are available" in {
            mockAuthRetrieveEmptyEnrolment()
            mockVatNumberIneligibleForMtd(testVatNumber, migratableDates = MigratableDates(Some(testStartDate), Some(testEndDate)))

            val request = testPostRequest(testVatNumber)

            val result = TestCaptureVatNumberController.submit(request)
            status(result) shouldBe Status.SEE_OTHER
            redirectLocation(result) shouldBe Some(routes.MigratableDatesController.show().url)
          }

          "redirect to Invalid Vat Number page when the vat number is invalid" in {
            mockAuthRetrieveEmptyEnrolment()
            mockVatNumberEligibilityInvalid(testVatNumber)

            val request = testPostRequest(testVatNumber)

            val result = TestCaptureVatNumberController.submit(request)
            status(result) shouldBe Status.SEE_OTHER
            redirectLocation(result) shouldBe Some(routes.InvalidVatNumberController.show().url)
          }

          "throw an exception for any other scenario" in {
            mockAuthRetrieveEmptyEnrolment()
            mockVatNumberEligibilityFailure(testVatNumber)

            val request = testPostRequest(testVatNumber)
            intercept[InternalServerException] {
              await(TestCaptureVatNumberController.submit(request))
            }
          }
        }
      }
    }

    "the vat number fails checksum validation" should {
      "redirect to Invalid Vat Number page" in {
        mockAuthRetrieveEmptyEnrolment()
        mockVatNumberEligibilitySuccess(testInvalidVatNumber)

        implicit val request = testPostRequest(testInvalidVatNumber)

        val result = TestCaptureVatNumberController.submit(request)
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.InvalidVatNumberController.show().url)
      }
    }
  }

  "form unsuccessfully submitted" should {
    "reload the page with errors" in {
      mockAuthRetrieveEmptyEnrolment()

      val result = TestCaptureVatNumberController.submit(testPostRequest("invalid"))
      status(result) shouldBe Status.BAD_REQUEST
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
    }
  }
}
