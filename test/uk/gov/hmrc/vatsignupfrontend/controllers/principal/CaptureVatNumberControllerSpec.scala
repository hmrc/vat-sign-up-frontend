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

import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.libs.json.Json
import play.api.mvc.AnyContentAsFormUrlEncoded
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.http.InternalServerException
import uk.gov.hmrc.play.test.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.SessionKeys._
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch.{FeatureSwitching, ReSignUpJourney}
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockControllerComponents
import uk.gov.hmrc.vatsignupfrontend.forms.VatNumberForm
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstantsGenerator
import uk.gov.hmrc.vatsignupfrontend.models.{DateModel, MigratableDates, Overseas, Yes}
import uk.gov.hmrc.vatsignupfrontend.services.VatNumberOrchestrationService._
import uk.gov.hmrc.vatsignupfrontend.services.mocks.MockVatNumberOrchestrationService

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


class CaptureVatNumberControllerSpec extends UnitSpec
  with GuiceOneAppPerSuite
  with MockControllerComponents
  with MockVatNumberOrchestrationService
  with BeforeAndAfterEach
  with FeatureSwitching {

  override def beforeEach(): Unit = {
    disable(ReSignUpJourney)
    super.beforeEach()
  }

  object TestCaptureVatNumberController extends CaptureVatNumberController(
    mockControllerComponents,
    mockVatNumberOrchestrationService
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
    "the ReSignUpJourney feature switch is disabled" when {
      "form successfully submitted" when {
        "the vat number passes checksum validation" when {
          "the user has a VAT-DEC enrolment" when {
            "the vat eligibility is successful" when {
              "the inserted vat number matches the enrolment one" when {
                "the VAT number is not overseas and is stored successfully" should {
                  "redirect to the business entity type page" in {
                    mockAuthRetrieveVatDecEnrolment()
                    mockStoreVatNumber(testVatNumber, isFromBta = false)(Future.successful(NonMigratedVatNumberStored(isOverseas = false, isDirectDebit = false)))

                    val result = TestCaptureVatNumberController.submit(testPostRequest(testVatNumber))

                    status(result) shouldBe Status.SEE_OTHER
                    redirectLocation(result) shouldBe Some(routes.CaptureBusinessEntityController.show().url)
                    session(result) get vatNumberKey should contain(testVatNumber)
                  }
                }

                "the VAT number is overseas and is stored successfully" should {
                  "redirect to the overseas resolver controller" in {
                    mockAuthRetrieveVatDecEnrolment()
                    mockStoreVatNumber(testVatNumber, isFromBta = false)(Future.successful(NonMigratedVatNumberStored(isOverseas = true, isDirectDebit = false)))

                    val result = TestCaptureVatNumberController.submit(testPostRequest(testVatNumber))

                    status(result) shouldBe Status.SEE_OTHER
                    redirectLocation(result) shouldBe Some(routes.OverseasResolverController.resolve().url)
                    session(result) get vatNumberKey should contain(testVatNumber)
                  }
                }

                "the user's information is being migrated" should {
                  "redirect to migration in progress error page" in {
                    mockAuthRetrieveVatDecEnrolment()
                    mockStoreVatNumber(testVatNumber, isFromBta = false)(Future.successful(MigrationInProgress))

                    val result = TestCaptureVatNumberController.submit(testPostRequest(testVatNumber))

                    status(result) shouldBe Status.SEE_OTHER
                    redirectLocation(result) shouldBe Some(routes.MigrationInProgressErrorController.show().url)
                  }
                }

                "the user's subscription has been claimed" should {
                  "redirect to claimed subscription confirmation page" in {
                    mockAuthRetrieveVatDecEnrolment()
                    mockStoreVatNumber(testVatNumber, isFromBta = false)(Future.successful(ClaimedSubscription))

                    val result = TestCaptureVatNumberController.submit(testPostRequest(testVatNumber))

                    status(result) shouldBe Status.SEE_OTHER
                    redirectLocation(result) shouldBe Some(routes.SignUpCompleteClientController.show().url)
                  }
                }

                "the user tried to claim a subscription that is already enrolled" should {
                  "redirect to business already signed up page" in {
                    mockAuthRetrieveVatDecEnrolment()
                    mockStoreVatNumber(testVatNumber, isFromBta = false)(Future.successful(AlreadyEnrolledOnDifferentCredential))

                    val result = TestCaptureVatNumberController.submit(testPostRequest(testVatNumber))

                    status(result) shouldBe Status.SEE_OTHER
                    redirectLocation(result) shouldBe Some(bta.routes.BusinessAlreadySignedUpController.show().url)
                  }
                }
              }

              "the inserted vat number doesn't match the enrolment one" should {
                "redirect to error page" in {
                  val testNonMatchingVat = TestConstantsGenerator.randomVatNumber
                  mockAuthRetrieveVatDecEnrolment()

                  val result = TestCaptureVatNumberController.submit(testPostRequest(testNonMatchingVat))

                  status(result) shouldBe Status.SEE_OTHER
                  redirectLocation(result) shouldBe Some(routes.IncorrectEnrolmentVatNumberController.show().url)
                }
              }
            }

            "the vat eligibility is unsuccessful" should {
              "redirect to Cannot use service yet when the vat number is ineligible for Making Tax Digital" in {
                mockAuthRetrieveVatDecEnrolment()
                mockStoreVatNumber(testVatNumber, isFromBta = false)(Future.successful(Ineligible))

                val request = testPostRequest(testVatNumber)

                val result = TestCaptureVatNumberController.submit(request)
                status(result) shouldBe Status.SEE_OTHER
                redirectLocation(result) shouldBe Some(routes.CannotUseServiceController.show().url)
              }

              "the vat eligibility is unsuccessful" should {
                "redirect to sign up after this date page when the vat number is ineligible and one date is available" in {
                  val testDates = MigratableDates(Some(testStartDate))

                  mockAuthRetrieveVatDecEnrolment()
                  mockStoreVatNumber(testVatNumber, isFromBta = false)(Future.successful(Inhibited(testDates)))

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

                  mockAuthRetrieveVatDecEnrolment()
                  mockStoreVatNumber(testVatNumber, isFromBta = false)(Future.successful(Inhibited(testDates)))

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
                mockCheckVatNumberEligibility(testVatNumber)(Future.successful(Eligible(isOverseas = false, isMigrated = false)))

                implicit val request = testPostRequest(testVatNumber)

                val result = TestCaptureVatNumberController.submit(request)
                status(result) shouldBe Status.SEE_OTHER
                redirectLocation(result) shouldBe Some(routes.CaptureVatRegistrationDateController.show().url)

                result.session get vatNumberKey should contain(testVatNumber)
              }

              "delete the known facts when they have been entered previously" in {
                mockAuthRetrieveEmptyEnrolment()
                mockCheckVatNumberEligibility(testVatNumber)(Future.successful(Eligible(isOverseas = false, isMigrated = false)))

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
                mockCheckVatNumberEligibility(testVatNumber)(Future.successful(Eligible(isOverseas = false, isMigrated = false)))

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
                mockCheckVatNumberEligibility(testVatNumber)(Future.successful(Eligible(isOverseas = true, isMigrated = false)))

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
              mockCheckVatNumberEligibility(testVatNumber)(Future.successful(Ineligible))

              val request = testPostRequest(testVatNumber)

              val result = TestCaptureVatNumberController.submit(request)
              status(result) shouldBe Status.SEE_OTHER
              redirectLocation(result) shouldBe Some(routes.CannotUseServiceController.show().url)
            }

            "redirect to sign up after this date when the vat number is ineligible and one date is available" in {
              mockAuthRetrieveEmptyEnrolment()
              mockCheckVatNumberEligibility(testVatNumber)(Future.successful(Inhibited(MigratableDates(Some(testStartDate)))))

              val request = testPostRequest(testVatNumber)

              val result = TestCaptureVatNumberController.submit(request)
              status(result) shouldBe Status.SEE_OTHER
              redirectLocation(result) shouldBe Some(routes.MigratableDatesController.show().url)
            }

            "redirect to sign up between these dates when the vat number is ineligible and two dates are available" in {
              val testDates = MigratableDates(Some(testStartDate), Some(testEndDate))
              mockAuthRetrieveEmptyEnrolment()
              mockCheckVatNumberEligibility(testVatNumber)(Future.successful(Inhibited(testDates)))

              val request = testPostRequest(testVatNumber)

              val result = TestCaptureVatNumberController.submit(request)
              status(result) shouldBe Status.SEE_OTHER
              redirectLocation(result) shouldBe Some(routes.MigratableDatesController.show().url)
            }

            "redirect to Invalid Vat Number page when the vat number is invalid" in {
              mockAuthRetrieveEmptyEnrolment()
              mockCheckVatNumberEligibility(testVatNumber)(Future.successful(InvalidVatNumber))

              val request = testPostRequest(testVatNumber)

              val result = TestCaptureVatNumberController.submit(request)
              status(result) shouldBe Status.SEE_OTHER
              redirectLocation(result) shouldBe Some(routes.InvalidVatNumberController.show().url)
            }

            "throw an exception for MigrationInProgress" in {
              mockAuthRetrieveEmptyEnrolment()
              mockCheckVatNumberEligibility(testVatNumber)(Future.successful(MigrationInProgress))

              val request = testPostRequest(testVatNumber)
              intercept[InternalServerException] {
                await(TestCaptureVatNumberController.submit(request))
              }
            }

            "throw an exception for AlreadySubscribed" in {
              mockAuthRetrieveEmptyEnrolment()
              mockCheckVatNumberEligibility(testVatNumber)(Future.successful(AlreadySubscribed))

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

          implicit val request = testPostRequest(testInvalidVatNumber)

          val result = TestCaptureVatNumberController.submit(request)
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.InvalidVatNumberController.show().url)
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

    "the ReSignUpJourney feature switch is enabled" when {
      "form successfully submitted" when {
        "the vat number passes checksum validation" when {
          "the user has an MTD-VAT enrolment" when {
            "the vat eligibility check is returns that the vat number is migrated" when {
              "the entered vat number matches the one on the enrolment" when {
                "the VAT number is not overseas and is stored successfully" should {
                  "redirect to the business entity type page" in {
                    enable(ReSignUpJourney)
                    mockAuthRetrieveMtdVatEnrolment()
                    mockStoreVatNumber(testVatNumber, isFromBta = false)(Future.successful(MigratedVatNumberStored))

                    val result = TestCaptureVatNumberController.submit(testPostRequest(testVatNumber))

                    status(result) shouldBe Status.SEE_OTHER
                    redirectLocation(result) shouldBe Some(routes.CaptureBusinessEntityController.show().url)
                    session(result) get vatNumberKey should contain(testVatNumber)
                    session(result) get isMigratedKey should contain(true.toString)
                  }
                }
              }
            }

            "the vat eligibility check is returns that the vat number is not migrated" when {
              "the entered vat number matches the one on the enrolment" when {
                "the VAT number is not overseas and is stored successfully" should {
                  "redirect to the business entity type page" in {
                    enable(ReSignUpJourney)
                    mockAuthRetrieveMtdVatEnrolment()
                    mockStoreVatNumber(testVatNumber, isFromBta = false)(Future.successful(NonMigratedVatNumberStored(isOverseas = false, isDirectDebit = false)))

                    val result = TestCaptureVatNumberController.submit(testPostRequest(testVatNumber))

                    status(result) shouldBe Status.SEE_OTHER
                    redirectLocation(result) shouldBe Some(routes.CaptureBusinessEntityController.show().url)
                    session(result) get vatNumberKey should contain(testVatNumber)
                  }
                }
              }

              "the entered vat number matches the one on the enrolment" when {
                "the VAT number is overseas and is stored successfully" should {
                  "redirect to the business entity type page" in {
                    enable(ReSignUpJourney)
                    mockAuthRetrieveMtdVatEnrolment()
                    mockStoreVatNumber(testVatNumber, isFromBta = false)(Future.successful(NonMigratedVatNumberStored(isOverseas = true, isDirectDebit = false)))

                    val result = TestCaptureVatNumberController.submit(testPostRequest(testVatNumber))

                    status(result) shouldBe Status.SEE_OTHER
                    redirectLocation(result) shouldBe Some(routes.OverseasResolverController.resolve().url)
                    session(result) get vatNumberKey should contain(testVatNumber)
                  }
                }
              }
            }

            "the entered vat number does not match the one on the enrolment" should {
              "redirect to the Incorrect enrolment VAT number controller" in {
                enable(ReSignUpJourney)
                mockAuthRetrieveMtdVatEnrolment()

                val result = TestCaptureVatNumberController.submit(testPostRequest(TestConstantsGenerator.randomVatNumber))

                status(result) shouldBe Status.SEE_OTHER
                redirectLocation(result) shouldBe Some(routes.IncorrectEnrolmentVatNumberController.show().url)
              }
            }
          }

        "the user has a legacy VAT enrolment" when {
          "the vat eligibility check is returns that the vat number is migrated" when {
            "the entered vat number matches the one on the enrolment" when {
              "the VAT number is not overseas and is stored successfully" should {
                "redirect to the business entity type page" in {
                  enable(ReSignUpJourney)
                  mockAuthRetrieveVatDecEnrolment()
                  mockStoreVatNumber(testVatNumber, isFromBta = false)(Future.successful(MigratedVatNumberStored))

                  val result = TestCaptureVatNumberController.submit(testPostRequest(testVatNumber))

                  status(result) shouldBe Status.SEE_OTHER
                  redirectLocation(result) shouldBe Some(routes.CaptureBusinessEntityController.show().url)
                  session(result) get vatNumberKey should contain(testVatNumber)
                  session(result) get isMigratedKey should contain(true.toString)
                }
              }
            }

            "the entered vat number does not match the one on the enrolment" should {
              "redirect to the Incorrect enrolment VAT number controller" in {
                enable(ReSignUpJourney)
                mockAuthRetrieveVatDecEnrolment()

                val result = TestCaptureVatNumberController.submit(testPostRequest(TestConstantsGenerator.randomVatNumber))

                status(result) shouldBe Status.SEE_OTHER
                redirectLocation(result) shouldBe Some(routes.IncorrectEnrolmentVatNumberController.show().url)
              }
            }
          }

          "the vat eligibility check is returns that the vat number is not migrated" when {
            "the entered vat number matches the one on the enrolment" when {
              "the VAT number is not overseas and is stored successfully" should {
                "redirect to the business entity type page" in {
                  enable(ReSignUpJourney)
                  mockAuthRetrieveVatDecEnrolment()
                  mockStoreVatNumber(testVatNumber, isFromBta = false)(Future.successful(NonMigratedVatNumberStored(isOverseas = false, isDirectDebit = false)))

                  val result = TestCaptureVatNumberController.submit(testPostRequest(testVatNumber))

                  status(result) shouldBe Status.SEE_OTHER
                  redirectLocation(result) shouldBe Some(routes.CaptureBusinessEntityController.show().url)
                  session(result) get vatNumberKey should contain(testVatNumber)
                }
              }
            }
          }

          "the vat eligibility check is returns that the vat number is not migrated" when {
            "the entered vat number matches the one on the enrolment" when {
              "the VAT number is overseas and is stored successfully" should {
                "redirect to the business entity type page" in {
                  enable(ReSignUpJourney)
                  mockAuthRetrieveVatDecEnrolment()
                  mockStoreVatNumber(testVatNumber, isFromBta = false)(Future.successful(NonMigratedVatNumberStored(isOverseas = true, isDirectDebit = false)))

                  val result = TestCaptureVatNumberController.submit(testPostRequest(testVatNumber))

                  status(result) shouldBe Status.SEE_OTHER
                  redirectLocation(result) shouldBe Some(routes.OverseasResolverController.resolve().url)
                  session(result) get vatNumberKey should contain(testVatNumber)
                }
              }
            }
          }

          "the user's information is being migrated" should {
            "redirect to migration in progress error page" in {
              enable(ReSignUpJourney)
              mockAuthRetrieveVatDecEnrolment()
              mockStoreVatNumber(testVatNumber, isFromBta = false)(Future.successful(MigrationInProgress))

              val result = TestCaptureVatNumberController.submit(testPostRequest(testVatNumber))

              status(result) shouldBe Status.SEE_OTHER
              redirectLocation(result) shouldBe Some(routes.MigrationInProgressErrorController.show().url)
            }
          }

          "the user's subscription has been claimed" should {
            "redirect to claimed subscription confirmation page" in {
              enable(ReSignUpJourney)
              mockAuthRetrieveVatDecEnrolment()
              mockStoreVatNumber(testVatNumber, isFromBta = false)(Future.successful(ClaimedSubscription))

              val result = TestCaptureVatNumberController.submit(testPostRequest(testVatNumber))

              status(result) shouldBe Status.SEE_OTHER
              redirectLocation(result) shouldBe Some(routes.SignUpCompleteClientController.show().url)
            }
          }

          "the user tried to claim a subscription that is already enrolled" should {
            "redirect to business already signed up page" in {
              enable(ReSignUpJourney)
              mockAuthRetrieveVatDecEnrolment()
              mockStoreVatNumber(testVatNumber, isFromBta = false)(Future.successful(AlreadyEnrolledOnDifferentCredential))

              val result = TestCaptureVatNumberController.submit(testPostRequest(testVatNumber))

              status(result) shouldBe Status.SEE_OTHER
              redirectLocation(result) shouldBe Some(bta.routes.BusinessAlreadySignedUpController.show().url)
            }
          }

          "the vat eligibility is unsuccessful" should {
            "redirect to Cannot use service yet when the vat number is ineligible for Making Tax Digital" in {
              enable(ReSignUpJourney)
              mockAuthRetrieveVatDecEnrolment()
              mockStoreVatNumber(testVatNumber, isFromBta = false)(Future.successful(Ineligible))

              val request = testPostRequest(testVatNumber)

              val result = TestCaptureVatNumberController.submit(request)
              status(result) shouldBe Status.SEE_OTHER
              redirectLocation(result) shouldBe Some(routes.CannotUseServiceController.show().url)
            }
          }

          "the vat eligibility is unsuccessful" should {
            "redirect to sign up after this date page when the vat number is ineligible and one date is available" in {
              enable(ReSignUpJourney)
              val testDates = MigratableDates(Some(testStartDate))

              mockAuthRetrieveVatDecEnrolment()
              mockStoreVatNumber(testVatNumber, isFromBta = false)(Future.successful(Inhibited(testDates)))

              val request = testPostRequest(testVatNumber)

              val result = TestCaptureVatNumberController.submit(request)
              status(result) shouldBe Status.SEE_OTHER
              redirectLocation(result) shouldBe Some(routes.MigratableDatesController.show().url)

              await(result).session(request).get(SessionKeys.migratableDatesKey) shouldBe Some(Json.toJson(testDates).toString)
            }
          }

          "the vat eligibility is unsuccessful" should {
            "redirect to sign up between these dates page when the vat number is ineligible and two dates are available" in {
              enable(ReSignUpJourney)
              val testDates = MigratableDates(Some(testStartDate), Some(testEndDate))

              mockAuthRetrieveVatDecEnrolment()
              mockStoreVatNumber(testVatNumber, isFromBta = false)(Future.successful(Inhibited(testDates)))

              val request = testPostRequest(testVatNumber)

              val result = TestCaptureVatNumberController.submit(request)
              status(result) shouldBe Status.SEE_OTHER
              redirectLocation(result) shouldBe Some(routes.MigratableDatesController.show().url)

              await(result).session(request).get(SessionKeys.migratableDatesKey) shouldBe Some(Json.toJson(testDates).toString)
            }
          }
        }

        "the user have both VAT enrolments" when {
          "the vat eligibility check is returns that the vat number is migrated" when {
            "the entered vat number matches the one on the enrolment" when {
              "the VAT number is not overseas and is stored successfully" should {
                "redirect to the business entity type page" in {
                  enable(ReSignUpJourney)
                  mockAuthRetrieveAllVatEnrolments()
                  mockStoreVatNumber(testVatNumber, isFromBta = false)(Future.successful(MigratedVatNumberStored))

                  val result = TestCaptureVatNumberController.submit(testPostRequest(testVatNumber))

                  status(result) shouldBe Status.SEE_OTHER
                  redirectLocation(result) shouldBe Some(routes.CaptureBusinessEntityController.show().url)
                  session(result) get vatNumberKey should contain(testVatNumber)
                  session(result) get isMigratedKey should contain(true.toString)
                }
              }
            }
          }

          "the vat eligibility check is returns that the vat number is not migrated" when {
            "the entered vat number matches the one on the enrolment" when {
              "the VAT number is not overseas and is stored successfully" should {
                "redirect to the business entity type page" in {
                  enable(ReSignUpJourney)
                  mockAuthRetrieveAllVatEnrolments()
                  mockStoreVatNumber(testVatNumber, isFromBta = false)(Future.successful(NonMigratedVatNumberStored(isOverseas = false, isDirectDebit = false)))

                  val result = TestCaptureVatNumberController.submit(testPostRequest(testVatNumber))

                  status(result) shouldBe Status.SEE_OTHER
                  redirectLocation(result) shouldBe Some(routes.CaptureBusinessEntityController.show().url)
                  session(result) get vatNumberKey should contain(testVatNumber)
                }
              }
            }
          }

          "the vat eligibility check is returns that the vat number is not migrated" when {
            "the entered vat number matches the one on the enrolment" when {
              "the VAT number is overseas and is stored successfully" should {
                "redirect to the business entity type page" in {
                  enable(ReSignUpJourney)
                  mockAuthRetrieveAllVatEnrolments()
                  mockStoreVatNumber(testVatNumber, isFromBta = false)(Future.successful(NonMigratedVatNumberStored(isOverseas = true, isDirectDebit = false)))

                  val result = TestCaptureVatNumberController.submit(testPostRequest(testVatNumber))

                  status(result) shouldBe Status.SEE_OTHER
                  redirectLocation(result) shouldBe Some(routes.OverseasResolverController.resolve().url)
                  session(result) get vatNumberKey should contain(testVatNumber)
                }
              }
            }
          }

          "the entered vat number does not match the one on the enrolment" should {
            "redirect to the Incorrect enrolment VAT number controller" in {
              enable(ReSignUpJourney)
              mockAuthRetrieveAllVatEnrolments()

              val result = TestCaptureVatNumberController.submit(testPostRequest(TestConstantsGenerator.randomVatNumber))

              status(result) shouldBe Status.SEE_OTHER
              redirectLocation(result) shouldBe Some(routes.IncorrectEnrolmentVatNumberController.show().url)
            }
          }
        }
      }
        "the user does not have an enrolment" when {
          "the Vat number is not for an overseas business" should {
            "redirect to the Capture Vat Registration Date page when the vat number is eligible" in {
              enable(ReSignUpJourney)
              mockAuthRetrieveEmptyEnrolment()
              mockCheckVatNumberEligibility(testVatNumber)(Future.successful(Eligible(isOverseas = false, isMigrated = false)))

              implicit val request = testPostRequest(testVatNumber)

              val result = TestCaptureVatNumberController.submit(request)
              status(result) shouldBe Status.SEE_OTHER
              redirectLocation(result) shouldBe Some(routes.CaptureVatRegistrationDateController.show().url)

              result.session get vatNumberKey should contain(testVatNumber)
            }

            "delete the known facts when they have been entered previously" in {
              enable(ReSignUpJourney)
              mockAuthRetrieveEmptyEnrolment()
              mockCheckVatNumberEligibility(testVatNumber)(Future.successful(Eligible(isOverseas = false, isMigrated = false)))

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
              enable(ReSignUpJourney)
              mockAuthRetrieveEmptyEnrolment()
              mockCheckVatNumberEligibility(testVatNumber)(Future.successful(Eligible(isOverseas = false, isMigrated = false)))

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
              enable(ReSignUpJourney)
              mockAuthRetrieveEmptyEnrolment()
              mockCheckVatNumberEligibility(testVatNumber)(Future.successful(Eligible(isOverseas = true, isMigrated = false)))

              implicit val request = testPostRequest(testVatNumber)

              val result = TestCaptureVatNumberController.submit(request)
              status(result) shouldBe Status.SEE_OTHER
              redirectLocation(result) shouldBe Some(routes.CaptureVatRegistrationDateController.show().url)

              result.session get vatNumberKey should contain(testVatNumber)
              result.session get businessEntityKey should contain(Overseas.toString)
            }
          }

          "redirect to Cannot use service yet when the vat number is ineligible for Making Tax Digital" in {
            enable(ReSignUpJourney)
            mockAuthRetrieveEmptyEnrolment()
            mockCheckVatNumberEligibility(testVatNumber)(Future.successful(Ineligible))

            val request = testPostRequest(testVatNumber)

            val result = TestCaptureVatNumberController.submit(request)
            status(result) shouldBe Status.SEE_OTHER
            redirectLocation(result) shouldBe Some(routes.CannotUseServiceController.show().url)
          }

          "redirect to sign up after this date when the vat number is ineligible and one date is available" in {
            enable(ReSignUpJourney)
            mockAuthRetrieveEmptyEnrolment()
            mockCheckVatNumberEligibility(testVatNumber)(Future.successful(Inhibited(MigratableDates(Some(testStartDate)))))

            val request = testPostRequest(testVatNumber)

            val result = TestCaptureVatNumberController.submit(request)
            status(result) shouldBe Status.SEE_OTHER
            redirectLocation(result) shouldBe Some(routes.MigratableDatesController.show().url)
          }

          "redirect to sign up between these dates when the vat number is ineligible and two dates are available" in {
            enable(ReSignUpJourney)
            val testDates = MigratableDates(Some(testStartDate), Some(testEndDate))
            mockAuthRetrieveEmptyEnrolment()
            mockCheckVatNumberEligibility(testVatNumber)(Future.successful(Inhibited(testDates)))

            val request = testPostRequest(testVatNumber)

            val result = TestCaptureVatNumberController.submit(request)
            status(result) shouldBe Status.SEE_OTHER
            redirectLocation(result) shouldBe Some(routes.MigratableDatesController.show().url)
          }

          "redirect to Invalid Vat Number page when the vat number is invalid" in {
            enable(ReSignUpJourney)
            mockAuthRetrieveEmptyEnrolment()
            mockCheckVatNumberEligibility(testVatNumber)(Future.successful(InvalidVatNumber))

            val request = testPostRequest(testVatNumber)

            val result = TestCaptureVatNumberController.submit(request)
            status(result) shouldBe Status.SEE_OTHER
            redirectLocation(result) shouldBe Some(routes.InvalidVatNumberController.show().url)
          }

          "throw an exception for MigrationInProgress" in {
            enable(ReSignUpJourney)
            mockAuthRetrieveEmptyEnrolment()
            mockCheckVatNumberEligibility(testVatNumber)(Future.successful(MigrationInProgress))

            val request = testPostRequest(testVatNumber)
            intercept[InternalServerException] {
              await(TestCaptureVatNumberController.submit(request))
            }
          }

          "throw an exception for AlreadySubscribed" in {
            enable(ReSignUpJourney)
            mockAuthRetrieveEmptyEnrolment()
            mockCheckVatNumberEligibility(testVatNumber)(Future.successful(AlreadySubscribed))

            val request = testPostRequest(testVatNumber)
            intercept[InternalServerException] {
              await(TestCaptureVatNumberController.submit(request))
            }
          }
        }
      }
    }
  }
}