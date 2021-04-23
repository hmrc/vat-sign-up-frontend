/*
 * Copyright 2021 HM Revenue & Customs
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
import play.api.mvc.{AnyContentAsEmpty, AnyContentAsFormUrlEncoded}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.Enrolments
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.SessionKeys._
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch.FeatureSwitching
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockVatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.controllers.principal.error.{routes => errorRoutes}
import uk.gov.hmrc.vatsignupfrontend.forms.VatNumberForm
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstantsGenerator
import uk.gov.hmrc.vatsignupfrontend.models.{DateModel, MigratableDates, Overseas, Yes}
import uk.gov.hmrc.vatsignupfrontend.services.StoreVatNumberOrchestrationService._
import uk.gov.hmrc.vatsignupfrontend.services.mocks.MockStoreVatNumberOrchestrationService
import uk.gov.hmrc.vatsignupfrontend.utils.UnitSpec

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class CaptureVatNumberControllerSpec extends UnitSpec
  with GuiceOneAppPerSuite
  with MockVatControllerComponents
  with MockStoreVatNumberOrchestrationService
  with BeforeAndAfterEach
  with FeatureSwitching {

  override def beforeEach(): Unit = {
    super.beforeEach()
  }

  object TestCaptureVatNumberController extends CaptureVatNumberController(mockStoreVatNumberOrchestrationService)

  lazy val testGetRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/vat-number")

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
              "the VAT number is not overseas and is stored successfully" should {
                "redirect to the business entity type page" in {
                  mockAuthRetrieveVatDecEnrolment()
                  mockOrchestrate(
                    enrolments = Enrolments(Set(testVatDecEnrolment)),
                    vatNumber = testVatNumber
                  )(Future.successful(VatNumberStored(isOverseas = false, isDirectDebit = false, isMigrated = false)))

                  val result = TestCaptureVatNumberController.submit(testPostRequest(testVatNumber))

                  status(result) shouldBe Status.SEE_OTHER
                  redirectLocation(result) shouldBe Some(routes.CaptureBusinessEntityController.show().url)
                  session(result) get vatNumberKey should contain(testVatNumber)
                  session(result) get isMigratedKey should contain(false.toString)
                }
              }

              "the VAT number is not overseas and is stored successfully for a migrated user" should {
                "redirect to the business entity type page" in {
                  mockAuthRetrieveVatDecEnrolment()
                  mockOrchestrate(
                    enrolments = Enrolments(Set(testVatDecEnrolment)),
                    vatNumber = testVatNumber
                  )(Future.successful(VatNumberStored(isOverseas = false, isDirectDebit = false, isMigrated = true)))

                  val result = TestCaptureVatNumberController.submit(testPostRequest(testVatNumber))

                  status(result) shouldBe Status.SEE_OTHER
                  redirectLocation(result) shouldBe Some(routes.CaptureBusinessEntityController.show().url)
                  session(result) get vatNumberKey should contain(testVatNumber)
                  session(result) get isMigratedKey should contain(true.toString)
                }
              }

              "the VAT number is overseas and is stored successfully" should {
                "redirect to the capture business entity controller" in {
                  mockAuthRetrieveVatDecEnrolment()
                  mockOrchestrate(
                    enrolments = Enrolments(Set(testVatDecEnrolment)),
                    vatNumber = testVatNumber
                  )(Future.successful(VatNumberStored(isOverseas = true, isDirectDebit = false, isMigrated = false)))

                  val result = TestCaptureVatNumberController.submit(testPostRequest(testVatNumber))

                  status(result) shouldBe Status.SEE_OTHER
                  redirectLocation(result) shouldBe Some(routes.CaptureBusinessEntityController.show().url)
                  session(result) get vatNumberKey should contain(testVatNumber)
                  session(result) get businessEntityKey should contain(Overseas.toString)
                  session(result) get isMigratedKey should contain(false.toString)
                }
              }


              "the VAT number is overseas and is stored successfully for a migrated user" should {
                "redirect to the capture business entity controller" in {
                  mockAuthRetrieveVatDecEnrolment()
                  mockOrchestrate(
                    enrolments = Enrolments(Set(testVatDecEnrolment)),
                    vatNumber = testVatNumber
                  )(Future.successful(VatNumberStored(isOverseas = true, isDirectDebit = false, isMigrated = true)))

                  val result = TestCaptureVatNumberController.submit(testPostRequest(testVatNumber))

                  status(result) shouldBe Status.SEE_OTHER
                  redirectLocation(result) shouldBe Some(routes.CaptureBusinessEntityController.show().url)
                  session(result) get vatNumberKey should contain(testVatNumber)
                  session(result) get businessEntityKey should contain(Overseas.toString)
                  session(result) get isMigratedKey should contain(true.toString)
                }
              }

              "the user's information is being migrated" should {
                "redirect to migration in progress error page" in {
                  mockAuthRetrieveVatDecEnrolment()
                  mockOrchestrate(
                    enrolments = Enrolments(Set(testVatDecEnrolment)),
                    vatNumber = testVatNumber
                  )(Future.successful(MigrationInProgress))

                  val result = TestCaptureVatNumberController.submit(testPostRequest(testVatNumber))

                  status(result) shouldBe Status.SEE_OTHER
                  redirectLocation(result) shouldBe Some(errorRoutes.MigrationInProgressErrorController.show().url)
                }
              }

              "the user's subscription has been claimed" should {
                "redirect to claimed subscription confirmation page" in {
                  mockAuthRetrieveVatDecEnrolment()
                  mockOrchestrate(
                    enrolments = Enrolments(Set(testVatDecEnrolment)),
                    vatNumber = testVatNumber
                  )(Future.successful(SubscriptionClaimed))

                  val result = TestCaptureVatNumberController.submit(testPostRequest(testVatNumber))

                  status(result) shouldBe Status.SEE_OTHER
                  redirectLocation(result) shouldBe Some(routes.SignUpCompleteClientController.show().url)
                }
              }

              "the user tried to claim a subscription that is already enrolled" should {
                "redirect to business already signed up page" in {
                  mockAuthRetrieveVatDecEnrolment()
                  mockOrchestrate(
                    enrolments = Enrolments(Set(testVatDecEnrolment)),
                    vatNumber = testVatNumber
                  )(Future.successful(AlreadyEnrolledOnDifferentCredential))

                  val result = TestCaptureVatNumberController.submit(testPostRequest(testVatNumber))

                  status(result) shouldBe Status.SEE_OTHER
                  redirectLocation(result) shouldBe Some(errorRoutes.BusinessAlreadySignedUpController.show().url)
                }
              }
            }

            "the inserted vat number doesn't match the enrolment one" should {
              "redirect to error page" in {
                val testNonMatchingVat = TestConstantsGenerator.randomVatNumber
                mockAuthRetrieveVatDecEnrolment()

                val result = TestCaptureVatNumberController.submit(testPostRequest(testNonMatchingVat))

                status(result) shouldBe Status.SEE_OTHER
                redirectLocation(result) shouldBe Some(errorRoutes.IncorrectEnrolmentVatNumberController.show().url)
              }
            }
          }

          "the vat eligibility is unsuccessful" should {
            "redirect to Deregistered Vat Number when the vat number is deregistered" in {
              mockAuthRetrieveVatDecEnrolment()
              mockOrchestrate(
                enrolments = Enrolments(Set(testVatDecEnrolment)),
                vatNumber = testVatNumber
              )(Future.successful(Deregistered))

              val request = testPostRequest(testVatNumber)

              val result = TestCaptureVatNumberController.submit(request)
              status(result) shouldBe Status.SEE_OTHER
              redirectLocation(result) shouldBe Some(errorRoutes.DeregisteredVatNumberController.show().url)
            }

            "redirect to Cannot use service yet when the vat number is ineligible for Making Tax Digital" in {
              mockAuthRetrieveVatDecEnrolment()
              mockOrchestrate(
                enrolments = Enrolments(Set(testVatDecEnrolment)),
                vatNumber = testVatNumber
              )(Future.successful(Ineligible))

              val request = testPostRequest(testVatNumber)

              val result = TestCaptureVatNumberController.submit(request)
              status(result) shouldBe Status.SEE_OTHER
              redirectLocation(result) shouldBe Some(errorRoutes.CannotUseServiceController.show().url)
            }

            "redirect to sign up after this date page when the vat number is ineligible and one date is available" in {
              val testDates = MigratableDates(Some(testStartDate))

              mockAuthRetrieveVatDecEnrolment()
              mockOrchestrate(
                enrolments = Enrolments(Set(testVatDecEnrolment)),
                vatNumber = testVatNumber
              )(Future.successful(Inhibited(testDates)))

              val request = testPostRequest(testVatNumber)

              val result = TestCaptureVatNumberController.submit(request)
              status(result) shouldBe Status.SEE_OTHER
              redirectLocation(result) shouldBe Some(errorRoutes.MigratableDatesController.show().url)

              session(result).get(SessionKeys.migratableDatesKey) shouldBe Some(Json.toJson(testDates).toString)
            }

            "redirect to sign up between these dates page when the vat number is ineligible and two dates are available" in {
              val testDates = MigratableDates(Some(testStartDate), Some(testEndDate))

              mockAuthRetrieveVatDecEnrolment()
              mockOrchestrate(
                enrolments = Enrolments(Set(testVatDecEnrolment)),
                vatNumber = testVatNumber
              )(Future.successful(Inhibited(testDates)))

              val request = testPostRequest(testVatNumber)

              val result = TestCaptureVatNumberController.submit(request)
              status(result) shouldBe Status.SEE_OTHER
              redirectLocation(result) shouldBe Some(errorRoutes.MigratableDatesController.show().url)

              session(result).get(SessionKeys.migratableDatesKey) shouldBe Some(Json.toJson(testDates).toString)
            }
          }
        }

        "the user has an MTD-VAT enrolment" when {
          "the user attempts to sign up the same vat number that is already on their enrolment" in {
            mockAuthRetrieveMtdVatEnrolment()
            mockOrchestrate(
              enrolments = Enrolments(Set(testMtdVatEnrolment)),
              vatNumber = testVatNumber
            )(Future.successful(AlreadySubscribed(isOverseas = false)))

            val request = testPostRequest(testVatNumber)

            val result = TestCaptureVatNumberController.submit(request)

            status(result) shouldBe Status.SEE_OTHER
            redirectLocation(result) shouldBe Some(errorRoutes.AlreadySignedUpController.show().url)
          }

          "the user attempts to sign up a different vat number" in {
            mockAuthRetrieveMtdVatEnrolment()

            val request = testPostRequest(TestConstantsGenerator.randomVatNumber)

            val result = TestCaptureVatNumberController.submit(request)

            status(result) shouldBe Status.SEE_OTHER
            redirectLocation(result) shouldBe Some(errorRoutes.IncorrectEnrolmentVatNumberController.show().url)
          }
        }

        "the user has an MTD-VAT enrolment and a VAT-DEC enrolment" should {
          "display the already signed up error page" when {
            "the user attempts to sign up the same vat number that is already on their enrolment" in {
              mockAuthRetrieveAllVatEnrolments()
              mockOrchestrate(
                enrolments = Enrolments(Set(testVatDecEnrolment, testMtdVatEnrolment)),
                vatNumber = testVatNumber
              )(Future.successful(AlreadySubscribed(isOverseas = false)))

              val request = testPostRequest(testVatNumber)

              val result = TestCaptureVatNumberController.submit(request)

              status(result) shouldBe Status.SEE_OTHER
              redirectLocation(result) shouldBe Some(errorRoutes.AlreadySignedUpController.show().url)
            }
          }

          "display the cannot sign up another account error page" when {
            "the user attempts to sign up a different vat number" in {
              mockAuthRetrieveAllVatEnrolments()

              val request = testPostRequest(TestConstantsGenerator.randomVatNumber)

              val result = TestCaptureVatNumberController.submit(request)

              status(result) shouldBe Status.SEE_OTHER
              redirectLocation(result) shouldBe Some(errorRoutes.IncorrectEnrolmentVatNumberController.show().url)
            }
          }
        }

        "the user does not have a vat enrolment" should {
          "redirect to CaptureVatRegistrationDateController with already subscribed tag in session" when {
            "the user attempts to sign up an unclaimed vat number" in {
              mockAuthRetrieveEmptyEnrolment()
              mockOrchestrate(
                enrolments = Enrolments(Set()),
                vatNumber = testVatNumber
              )(Future.successful(AlreadySubscribed(isOverseas = false)))

              val request = testPostRequest(testVatNumber)

              val result = TestCaptureVatNumberController.submit(request)

              status(result) shouldBe Status.SEE_OTHER
              redirectLocation(result) shouldBe Some(routes.CaptureVatRegistrationDateController.show().url)
              session(result).get(isAlreadySubscribedKey) should contain("true")
            }
          }
        }

        "the user does not have a vat enrolment and is overseas" should {
          "redirect to CaptureVatRegistrationDateController with already subscribed tag and overseas BE in session" when {
            "the user attempts to sign up an unclaimed vat number" in {
              mockAuthRetrieveEmptyEnrolment()
              mockOrchestrate(
                enrolments = Enrolments(Set()),
                vatNumber = testVatNumber
              )(Future.successful(AlreadySubscribed(isOverseas = true)))

              val request = testPostRequest(testVatNumber)

              val result = TestCaptureVatNumberController.submit(request)

              status(result) shouldBe Status.SEE_OTHER
              redirectLocation(result) shouldBe Some(routes.CaptureVatRegistrationDateController.show().url)
              session(result).get(isAlreadySubscribedKey) should contain("true")
              session(result).get(businessEntityKey) should contain(Overseas.toString)
            }
          }
        }

        "the user does not have a VAT-DEC enrolment" when {
          "the Vat number is not for an overseas business" should {
            "redirect to the Capture Vat Registration Date page when the vat number is eligible" in {
              mockAuthRetrieveEmptyEnrolment()
              mockOrchestrate(
                enrolments = Enrolments(Set()),
                vatNumber = testVatNumber
              )(Future.successful(Eligible(isOverseas = false, isMigrated = false, isNew = false)))

              implicit val request: FakeRequest[AnyContentAsFormUrlEncoded] = testPostRequest(testVatNumber)

              val result = TestCaptureVatNumberController.submit(request)
              status(result) shouldBe Status.SEE_OTHER
              redirectLocation(result) shouldBe Some(routes.CaptureVatRegistrationDateController.show().url)

              session(result).get(vatNumberKey) should contain(testVatNumber)
            }

            "delete the known facts when they have been entered previously" in {
              mockAuthRetrieveEmptyEnrolment()
              mockOrchestrate(
                enrolments = Enrolments(Set()),
                vatNumber = testVatNumber
              )(Future.successful(Eligible(isOverseas = false, isMigrated = false, isNew = false)))

              implicit val request: FakeRequest[AnyContentAsFormUrlEncoded] = testPostRequest(testVatNumber)
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

              session(result).get(vatNumberKey) should contain(testVatNumber)
              session(result).get(vatRegistrationDateKey) shouldBe empty
              session(result).get(businessPostCodeKey) shouldBe empty
              session(result).get(previousVatReturnKey) shouldBe empty
              session(result).get(lastReturnMonthPeriodKey) shouldBe empty
              session(result).get(box5FigureKey) shouldBe empty
            }

            "overseas user changes vat number to non overseas vat number" in {
              mockAuthRetrieveEmptyEnrolment()
              mockOrchestrate(
                enrolments = Enrolments(Set()),
                vatNumber = testVatNumber
              )(Future.successful(Eligible(isOverseas = false, isMigrated = false, isNew = false)))

              implicit val request: FakeRequest[AnyContentAsFormUrlEncoded] = testPostRequest(testVatNumber)
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

              session(result).get(vatNumberKey) should contain(testVatNumber)
              session(result).get(vatRegistrationDateKey) shouldBe empty
              session(result).get(businessPostCodeKey) shouldBe empty
              session(result).get(businessEntityKey) shouldBe empty
              session(result).get(previousVatReturnKey) shouldBe empty
              session(result).get(lastReturnMonthPeriodKey) shouldBe empty
              session(result).get(box5FigureKey) shouldBe empty

            }
          }

          "the vat number is for an overseas business" should {
            "redirect to the Capture Vat registration date controller" in {
              mockAuthRetrieveEmptyEnrolment()
              mockOrchestrate(
                enrolments = Enrolments(Set()),
                vatNumber = testVatNumber
              )(Future.successful(Eligible(isOverseas = true, isMigrated = false, isNew = false)))

              implicit val request: FakeRequest[AnyContentAsFormUrlEncoded] = testPostRequest(testVatNumber)

              val result = TestCaptureVatNumberController.submit(request)
              status(result) shouldBe Status.SEE_OTHER
              redirectLocation(result) shouldBe Some(routes.CaptureVatRegistrationDateController.show().url)

              session(result).get(vatNumberKey) should contain(testVatNumber)
              session(result).get(businessEntityKey) should contain(Overseas.toString)
              session(result).get(isMigratedKey) should contain(false.toString)
            }
          }


          "the vat number is for an overseas business who has already been migrated" should {
            "redirect to the Capture Vat registration date controller" in {
              mockAuthRetrieveEmptyEnrolment()
              mockOrchestrate(
                enrolments = Enrolments(Set()),
                vatNumber = testVatNumber
              )(Future.successful(Eligible(isOverseas = true, isMigrated = true, isNew = false)))

              implicit val request: FakeRequest[AnyContentAsFormUrlEncoded] = testPostRequest(testVatNumber)

              val result = TestCaptureVatNumberController.submit(request)
              status(result) shouldBe Status.SEE_OTHER
              redirectLocation(result) shouldBe Some(routes.CaptureVatRegistrationDateController.show().url)

              session(result).get(vatNumberKey) should contain(testVatNumber)
              session(result).get(businessEntityKey) should contain(Overseas.toString)
              session(result).get(isMigratedKey) should contain(true.toString)
            }
          }

          "the vat number has been registered less than a week ago" should {
            "redirect to the Recently Registered controller" in {
              mockAuthRetrieveEmptyEnrolment()
              mockOrchestrate(
                enrolments = Enrolments(Set()),
                vatNumber = testVatNumber
              )(Future.successful(RecentlyRegistered))

              implicit val request: FakeRequest[AnyContentAsFormUrlEncoded] = testPostRequest(testVatNumber)

              val result = TestCaptureVatNumberController.submit(request)
              status(result) shouldBe Status.SEE_OTHER
              redirectLocation(result) shouldBe Some(errorRoutes.RecentlyRegisteredVatNumberController.show().url)
            }
          }

          "redirect to Deregistered Vat Number when the vat number is deregistered" in {
            mockAuthRetrieveEmptyEnrolment()
            mockOrchestrate(
              enrolments = Enrolments(Set()),
              vatNumber = testVatNumber
            )(Future.successful(Deregistered))

            val request = testPostRequest(testVatNumber)

            val result = TestCaptureVatNumberController.submit(request)
            status(result) shouldBe Status.SEE_OTHER
            redirectLocation(result) shouldBe Some(errorRoutes.DeregisteredVatNumberController.show().url)
          }

          "redirect to Cannot use service yet when the vat number is ineligible for Making Tax Digital" in {
            mockAuthRetrieveEmptyEnrolment()
            mockOrchestrate(
              enrolments = Enrolments(Set()),
              vatNumber = testVatNumber
            )(Future.successful(Ineligible))

            val request = testPostRequest(testVatNumber)

            val result = TestCaptureVatNumberController.submit(request)
            status(result) shouldBe Status.SEE_OTHER
            redirectLocation(result) shouldBe Some(errorRoutes.CannotUseServiceController.show().url)
          }

          "redirect to sign up after this date when the vat number is ineligible and one date is available" in {
            mockAuthRetrieveEmptyEnrolment()
            mockOrchestrate(
              enrolments = Enrolments(Set()),
              vatNumber = testVatNumber
            )(Future.successful(Inhibited(MigratableDates(Some(testStartDate)))))

            val request = testPostRequest(testVatNumber)

            val result = TestCaptureVatNumberController.submit(request)
            status(result) shouldBe Status.SEE_OTHER
            redirectLocation(result) shouldBe Some(errorRoutes.MigratableDatesController.show().url)
          }

          "redirect to sign up between these dates when the vat number is ineligible and two dates are available" in {
            val testDates = MigratableDates(Some(testStartDate), Some(testEndDate))
            mockAuthRetrieveEmptyEnrolment()
            mockOrchestrate(
              enrolments = Enrolments(Set()),
              vatNumber = testVatNumber
            )(Future.successful(Inhibited(testDates)))

            val request = testPostRequest(testVatNumber)

            val result = TestCaptureVatNumberController.submit(request)
            status(result) shouldBe Status.SEE_OTHER
            redirectLocation(result) shouldBe Some(errorRoutes.MigratableDatesController.show().url)
          }

          "redirect to Invalid Vat Number page when the vat number is invalid" in {
            mockAuthRetrieveEmptyEnrolment()
            mockOrchestrate(
              enrolments = Enrolments(Set()),
              vatNumber = testVatNumber
            )(Future.successful(InvalidVatNumber))

            val request = testPostRequest(testVatNumber)

            val result = TestCaptureVatNumberController.submit(request)
            status(result) shouldBe Status.SEE_OTHER
            redirectLocation(result) shouldBe Some(errorRoutes.InvalidVatNumberController.show().url)
          }

          "redirect to the migration in progress error page when a migration is in progress for the entered vrn" in {
            mockAuthRetrieveEmptyEnrolment()
            mockOrchestrate(
              enrolments = Enrolments(Set()),
              vatNumber = testVatNumber
            )(Future.successful(MigrationInProgress))

            val request = testPostRequest(testVatNumber)

            val result = TestCaptureVatNumberController.submit(request)

            status(result) shouldBe Status.SEE_OTHER
            redirectLocation(result) shouldBe Some(errorRoutes.MigrationInProgressErrorController.show().url)
          }
        }
      }
    }

    "the vat number fails checksum validation" should {
      "redirect to Invalid Vat Number page" in {
        mockAuthRetrieveEmptyEnrolment()

        implicit val request: FakeRequest[AnyContentAsFormUrlEncoded] = testPostRequest(testInvalidVatNumber)

        val result = TestCaptureVatNumberController.submit(request)
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(errorRoutes.InvalidVatNumberController.show().url)
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
}
