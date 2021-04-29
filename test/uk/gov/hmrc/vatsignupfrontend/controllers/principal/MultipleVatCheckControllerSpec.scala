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

import play.api.http.Status
import play.api.mvc.{AnyContentAsEmpty, AnyContentAsFormUrlEncoded}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.Enrolments
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockVatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.controllers.principal.error.{routes => errorRoutes}
import uk.gov.hmrc.vatsignupfrontend.forms.MultipleVatCheckForm._
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.models.MigratableDates
import uk.gov.hmrc.vatsignupfrontend.services.StoreVatNumberOrchestrationService._
import uk.gov.hmrc.vatsignupfrontend.services.mocks.MockStoreVatNumberOrchestrationService
import uk.gov.hmrc.vatsignupfrontend.utils.UnitSpec

import scala.concurrent.Future
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.multiple_vat_check

class MultipleVatCheckControllerSpec extends UnitSpec with MockVatControllerComponents with MockStoreVatNumberOrchestrationService {

  val view = app.injector.instanceOf[multiple_vat_check]

  object TestMultipleVatCheckController extends MultipleVatCheckController(mockStoreVatNumberOrchestrationService, view)

  val testGetRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/more-than-one-vat-business")

  def testPostRequest(entityTypeVal: String): FakeRequest[AnyContentAsFormUrlEncoded] =
    FakeRequest("POST", "/more-than-one-vat-business").withFormUrlEncodedBody(yesNo -> entityTypeVal)

  "Calling the show action of the Multiple Vat Check controller" should {
    "go to the Multiple Vat Check page" in {
      mockAuthAdminRole()

      val result = TestMultipleVatCheckController.show(testGetRequest)
      status(result) shouldBe Status.OK
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
    }
  }

  "Calling the submit action of the Multiple Vat Check controller" when {
    "form successfully submitted" should {
      "the choice is YES" should {
        "go to vat number" in {
          mockAuthRetrieveVatDecEnrolment()

          val result = TestMultipleVatCheckController.submit(testPostRequest(entityTypeVal = "yes"))
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.CaptureVatNumberController.show().url)
        }
      }

      "the choice is NO" when {
        "there are no enrolments" should {
          "go to resolve vat number" in {
            mockPrincipalAuthSuccess(enrolments = Enrolments(Set()))

            val result = TestMultipleVatCheckController.submit(testPostRequest(entityTypeVal = "no"))
            status(result) shouldBe Status.SEE_OTHER
            redirectLocation(result) shouldBe Some(routes.ResolveVatNumberController.resolve().url)
          }
        }
        "the non migrated VAT number is stored successfully" should {
          "go to business-type" in {
            mockAuthRetrieveVatDecEnrolment()
            mockOrchestrate(
              enrolments = Enrolments(Set(testVatDecEnrolment)),
              vatNumber = testVatNumber
            )(Future.successful(VatNumberStored(isOverseas = false, isDirectDebit = false, isMigrated = false)))

            val result = TestMultipleVatCheckController.submit(testPostRequest(entityTypeVal = "no"))
            status(result) shouldBe Status.SEE_OTHER
            redirectLocation(result) shouldBe Some(routes.CaptureBusinessEntityController.show().url)
            session(result) get SessionKeys.vatNumberKey should contain(testVatNumber)
            session(result) get SessionKeys.hasDirectDebitKey should contain("false")
            session(result) get SessionKeys.isMigratedKey should contain("false")
          }
        }
        "the migrated VAT number is stored successfully and the Business Entity is not overseas" should {
          "go to business-type" in {
            mockAuthRetrieveVatDecEnrolment()
            mockOrchestrate(
              enrolments = Enrolments(Set(testVatDecEnrolment)),
              vatNumber = testVatNumber
            )(Future.successful(VatNumberStored(isOverseas = false, isDirectDebit = false, isMigrated = true)))

            val result = TestMultipleVatCheckController.submit(testPostRequest(entityTypeVal = "no"))
            status(result) shouldBe Status.SEE_OTHER
            redirectLocation(result) shouldBe Some(routes.CaptureBusinessEntityController.show().url)
            session(result) get SessionKeys.vatNumberKey should contain(testVatNumber)
            session(result) get SessionKeys.hasDirectDebitKey should contain("false")
            session(result) get SessionKeys.isMigratedKey should contain("true")
            session(result) get SessionKeys.businessEntityKey shouldNot contain("overseas")
          }
        }
        "the non migrated VAT number is stored successfully and the Business Entity is Overseas" should {
          "go to business-type" in {
            mockAuthRetrieveVatDecEnrolment()
            mockOrchestrate(
              enrolments = Enrolments(Set(testVatDecEnrolment)),
              vatNumber = testVatNumber
            )(Future.successful(VatNumberStored(isOverseas = true, isDirectDebit = false, isMigrated = false)))

            val result = TestMultipleVatCheckController.submit(testPostRequest(entityTypeVal = "no"))
            status(result) shouldBe Status.SEE_OTHER
            redirectLocation(result) shouldBe Some(routes.CaptureBusinessEntityController.show().url)
            session(result) get SessionKeys.vatNumberKey should contain(testVatNumber)
            session(result) get SessionKeys.hasDirectDebitKey should contain("false")
            session(result) get SessionKeys.isMigratedKey should contain("false")
            session(result) get SessionKeys.businessEntityKey should contain("overseas")
          }
        }
        "the non migrated VAT number is stored successfully and the user has Direct Debits" should {
          "go to business-type with the direct debit flag in the session" in {
            mockAuthRetrieveVatDecEnrolment()
            mockOrchestrate(
              enrolments = Enrolments(Set(testVatDecEnrolment)),
              vatNumber = testVatNumber
            )(Future.successful(VatNumberStored(isOverseas = false, isDirectDebit = true, isMigrated = false)))

            val result = TestMultipleVatCheckController.submit(testPostRequest(entityTypeVal = "no"))
            status(result) shouldBe Status.SEE_OTHER
            redirectLocation(result) shouldBe Some(routes.CaptureBusinessEntityController.show().url)
            session(result) get SessionKeys.vatNumberKey should contain(testVatNumber)
            session(result) get SessionKeys.hasDirectDebitKey should contain("true")
            session(result) get SessionKeys.isMigratedKey should contain("false")

          }
        }
        "the VAT subscription has been claimed" should {
          "go to sign-up-complete-client" in {
            mockAuthRetrieveVatDecEnrolment()
            mockOrchestrate(
              enrolments = Enrolments(Set(testVatDecEnrolment)),
              vatNumber = testVatNumber
            )(Future.successful(SubscriptionClaimed))

            val result = TestMultipleVatCheckController.submit(testPostRequest(entityTypeVal = "no"))
            status(result) shouldBe Status.SEE_OTHER
            redirectLocation(result) shouldBe Some(routes.SignUpCompleteClientController.show().url)
          }
        }

        "the vat number is deregistered" should {
          "redirect to the deregistered VAT number page" in {
            mockAuthRetrieveVatDecEnrolment()
            mockOrchestrate(
              enrolments = Enrolments(Set(testVatDecEnrolment)),
              vatNumber = testVatNumber
            )(Future.successful(Deregistered))

            val result = TestMultipleVatCheckController.submit(testPostRequest(entityTypeVal = "no"))
            status(result) shouldBe Status.SEE_OTHER
            redirectLocation(result) shouldBe Some(errorRoutes.DeregisteredVatNumberController.show().url)
          }
        }

        "the vat number is ineligible" should {
          "redirect to the cannot use service page" in {
            mockAuthRetrieveVatDecEnrolment()
            mockOrchestrate(
              enrolments = Enrolments(Set(testVatDecEnrolment)),
              vatNumber = testVatNumber
            )(Future.successful(Ineligible))

            val result = TestMultipleVatCheckController.submit(testPostRequest(entityTypeVal = "no"))
            status(result) shouldBe Status.SEE_OTHER
            redirectLocation(result) shouldBe Some(errorRoutes.CannotUseServiceController.show().url)
          }

          "redirect to the migratable dates page" in {
            mockAuthRetrieveVatDecEnrolment()
            mockOrchestrate(
              enrolments = Enrolments(Set(testVatDecEnrolment)),
              vatNumber = testVatNumber
            )(Future.successful(Inhibited(MigratableDates(Some(testStartDate), Some(testEndDate)))))

            val result = TestMultipleVatCheckController.submit(testPostRequest(entityTypeVal = "no"))
            status(result) shouldBe Status.SEE_OTHER
            redirectLocation(result) shouldBe Some(errorRoutes.MigratableDatesController.show().url)
          }
        }
        "the vat number has already been signed up and migration is progress" should {
          "redirect to the migration in progress error page" in {
            mockAuthRetrieveVatDecEnrolment()
            mockOrchestrate(
              enrolments = Enrolments(Set(testVatDecEnrolment)),
              vatNumber = testVatNumber
            )(Future.successful(MigrationInProgress))

            val result = TestMultipleVatCheckController.submit(testPostRequest(entityTypeVal = "no"))
            status(result) shouldBe Status.SEE_OTHER
            redirectLocation(result) shouldBe Some(errorRoutes.MigrationInProgressErrorController.show().url)
          }
        }
        "the vat number has already been enrolled to another cred" should {
          "redirect to the business already signed up error page" in {
            mockAuthRetrieveVatDecEnrolment()
            mockOrchestrate(
              enrolments = Enrolments(Set(testVatDecEnrolment)),
              vatNumber = testVatNumber
            )(Future.successful(AlreadyEnrolledOnDifferentCredential))

            val result = TestMultipleVatCheckController.submit(testPostRequest(entityTypeVal = "no"))
            status(result) shouldBe Status.SEE_OTHER
            redirectLocation(result) shouldBe Some(errorRoutes.BusinessAlreadySignedUpController.show().url)
          }
        }
        "the vat number was registered less than a week ago" should {
          "redirect to the recently registered error page" in {
            mockAuthRetrieveVatDecEnrolment()
            mockOrchestrate(
              enrolments = Enrolments(Set(testVatDecEnrolment)),
              vatNumber = testVatNumber
            )(Future.successful(RecentlyRegistered))

            val result = TestMultipleVatCheckController.submit(testPostRequest(entityTypeVal = "no"))
            status(result) shouldBe Status.SEE_OTHER
            redirectLocation(result) shouldBe Some(errorRoutes.RecentlyRegisteredVatNumberController.show().url)
          }
        }
        "the user has an MTD-VAT and VAT-DEC enrolment" should {
          "redirect to already signed up error page" when {
            "the vat numbers from both enrolments match each other" in {
              mockAuthRetrieveAllVatEnrolments()
              mockOrchestrate(
                enrolments = Enrolments(Set(testVatDecEnrolment, testMtdVatEnrolment)),
                vatNumber = testVatNumber
              )(Future.successful(AlreadySubscribed(isOverseas = false)))

              val result = TestMultipleVatCheckController.submit(testPostRequest(entityTypeVal = "no"))
              status(result) shouldBe Status.SEE_OTHER
              redirectLocation(result) shouldBe Some(errorRoutes.AlreadySignedUpController.show().url)
            }
          }
        }
      }

      "form unsuccessfully submitted" should {
        "reload the page with errors" in {
          mockAuthRetrieveVatDecEnrolment()

          val result = TestMultipleVatCheckController.submit(testPostRequest("invalid"))
          status(result) shouldBe Status.BAD_REQUEST
          contentType(result) shouldBe Some("text/html")
          charset(result) shouldBe Some("utf-8")
        }
      }
    }
  }
}
