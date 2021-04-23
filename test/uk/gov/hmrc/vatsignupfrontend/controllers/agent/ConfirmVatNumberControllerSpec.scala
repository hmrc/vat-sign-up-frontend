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

package uk.gov.hmrc.vatsignupfrontend.controllers.agent

import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.libs.json.Json
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.Enrolments
import uk.gov.hmrc.http.InternalServerException
import uk.gov.hmrc.vatsignupfrontend.utils.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockVatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.controllers.agent.error.{routes => errorRoutes}
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.models.MigratableDates
import uk.gov.hmrc.vatsignupfrontend.services.StoreVatNumberOrchestrationService._
import uk.gov.hmrc.vatsignupfrontend.services.mocks.MockStoreVatNumberOrchestrationService

import scala.concurrent.Future

class ConfirmVatNumberControllerSpec extends UnitSpec
  with GuiceOneAppPerSuite
  with MockVatControllerComponents
  with MockStoreVatNumberOrchestrationService {

  object TestConfirmVatNumberController extends ConfirmVatNumberController(mockStoreVatNumberOrchestrationService)

  lazy val testGetRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/confirm-vat-number")

  lazy val testPostRequest: FakeRequest[AnyContentAsEmpty.type] =
    FakeRequest("POST", "/confirm-vat-number")

  "Calling the show action of the Confirm Vat Number controller" when {
    "there is a vrn in the session" should {
      "go to the Confirm Vat number page" in {
        mockAuthRetrieveAgentEnrolment()
        val request = testGetRequest.withSession(SessionKeys.vatNumberKey -> testVatNumber)

        val result = TestConfirmVatNumberController.show(request)
        status(result) shouldBe Status.OK
        contentType(result) shouldBe Some("text/html")
        charset(result) shouldBe Some("utf-8")
      }
    }

    "there isn't a vrn in the session" should {
      "redirect to Capture Vat number page" in {
        mockAuthRetrieveAgentEnrolment()

        val result = TestConfirmVatNumberController.show(testGetRequest)
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.CaptureVatNumberController.show().url)
      }
    }
  }

  "Calling the submit action of the Confirm Vat Number controller" when {

    "vat number is in session but it is invalid" should {
      "go to invalid vat number page" in {
        mockAgentAuthSuccess(Enrolments(Set(testAgentEnrolment)))

        val request = testPostRequest.withSession(SessionKeys.vatNumberKey -> testInvalidVatNumber)
        val result = TestConfirmVatNumberController.submit(request)

        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) should contain(errorRoutes.CouldNotConfirmVatNumberController.show().url)

        session(result).get(SessionKeys.vatNumberKey) shouldBe None
      }
    }

    "vat number is in session and store vat is successful" should {
      "go to the business entity type page" in {
        mockAgentAuthSuccess(Enrolments(Set(testAgentEnrolment)))
        mockOrchestrate(enrolments = Enrolments(Set(testAgentEnrolment)), vatNumber = testVatNumber)(
          Future.successful(VatNumberStored(isOverseas = false, isDirectDebit = false, isMigrated = false))
        )

        val result = TestConfirmVatNumberController.submit(testPostRequest.withSession(SessionKeys.vatNumberKey -> testVatNumber))
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) should contain(routes.CaptureBusinessEntityController.show().url)
        session(result) get SessionKeys.hasDirectDebitKey should contain("false")
        session(result) get SessionKeys.businessEntityKey shouldNot contain("overseas")
      }
    }

    "vat number is in session and store vat is successful for a migrated vat number" should {
      "go to the business entity type page" in {
        mockAgentAuthSuccess(Enrolments(Set(testAgentEnrolment)))
        mockOrchestrate(enrolments = Enrolments(Set(testAgentEnrolment)), vatNumber = testVatNumber)(
          Future.successful(VatNumberStored(isOverseas = false, isDirectDebit = false, isMigrated = true))
        )

        val result = TestConfirmVatNumberController.submit(testPostRequest.withSession(SessionKeys.vatNumberKey -> testVatNumber))
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) should contain(routes.CaptureBusinessEntityController.show().url)
        session(result) get SessionKeys.hasDirectDebitKey should contain("false")
        session(result) get SessionKeys.isMigratedKey should contain("true")
        session(result) get SessionKeys.businessEntityKey shouldNot contain("overseas")
      }
    }

    "overseas vat number is in session and store vat is successful" should {
      "go to the business entity type page" in {
        mockAgentAuthSuccess(Enrolments(Set(testAgentEnrolment)))
        mockOrchestrate(enrolments = Enrolments(Set(testAgentEnrolment)), vatNumber = testVatNumber)(
          Future.successful(VatNumberStored(isOverseas = true, isDirectDebit = true, isMigrated = false))
        )

        val result = TestConfirmVatNumberController.submit(testPostRequest.withSession(SessionKeys.vatNumberKey -> testVatNumber))
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) should contain(routes.CaptureBusinessEntityController.show().url)
        session(result) get SessionKeys.hasDirectDebitKey should contain("true")
        session(result) get SessionKeys.businessEntityKey should contain("overseas")
      }
    }

    "overseas vat number is in session and store vat is successful a migrated vat number" should {
      "go to the business entity type page" in {
        mockAgentAuthSuccess(Enrolments(Set(testAgentEnrolment)))
        mockOrchestrate(enrolments = Enrolments(Set(testAgentEnrolment)), vatNumber = testVatNumber)(
          Future.successful(VatNumberStored(isOverseas = true, isDirectDebit = false, isMigrated = true))
        )

        val result = TestConfirmVatNumberController.submit(testPostRequest.withSession(SessionKeys.vatNumberKey -> testVatNumber))
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) should contain(routes.CaptureBusinessEntityController.show().url)
        session(result) get SessionKeys.hasDirectDebitKey should contain("false")
        session(result) get SessionKeys.businessEntityKey should contain("overseas")
        session(result) get SessionKeys.isMigratedKey should contain("true")
      }
    }

    "vat number is in session but store vat is unsuccessful as no agent client relationship" should {
      "go to the no agent client relationship page" in {
        mockAgentAuthSuccess(Enrolments(Set(testAgentEnrolment)))
        mockOrchestrate(enrolments = Enrolments(Set(testAgentEnrolment)), vatNumber = testVatNumber)(Future.successful(NoAgentClientRelationship))

        val result = TestConfirmVatNumberController.submit(testPostRequest.withSession(SessionKeys.vatNumberKey -> testVatNumber))
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(errorRoutes.NoAgentClientRelationshipController.show().url)
      }
    }

    "vat number is in session but store vat is unsuccessful as client is ineligible" when {
      "go to the cannot use service page when no dates are given" in {
        mockAgentAuthSuccess(Enrolments(Set(testAgentEnrolment)))
        mockOrchestrate(enrolments = Enrolments(Set(testAgentEnrolment)), vatNumber = testVatNumber)(Future.successful(Ineligible))

        val result = TestConfirmVatNumberController.submit(testPostRequest.withSession(SessionKeys.vatNumberKey -> testVatNumber))
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(errorRoutes.CannotUseServiceController.show().url)
      }

      "redirect to sign up after this date page when one date is available" in {
        val testDates = MigratableDates(Some(testStartDate))

        mockAgentAuthSuccess(Enrolments(Set(testAgentEnrolment)))
        mockOrchestrate(enrolments = Enrolments(Set(testAgentEnrolment)), vatNumber = testVatNumber)(Future.successful(Inhibited(testDates)))

        val request = testPostRequest.withSession(SessionKeys.vatNumberKey -> testVatNumber)

        val result = TestConfirmVatNumberController.submit(request)

        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(errorRoutes.MigratableDatesController.show().url)

        session(result).get(SessionKeys.migratableDatesKey) shouldBe Some(Json.toJson(testDates).toString)
      }

      "redirect to sign up between these dates page when two dates are available" in {
        val testDates = MigratableDates(Some(testStartDate), Some(testEndDate))

        mockAgentAuthSuccess(Enrolments(Set(testAgentEnrolment)))
        mockOrchestrate(enrolments = Enrolments(Set(testAgentEnrolment)), vatNumber = testVatNumber)(Future.successful(Inhibited(testDates)))

        val request = testPostRequest.withSession(SessionKeys.vatNumberKey -> testVatNumber)

        val result = TestConfirmVatNumberController.submit(request)

        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(errorRoutes.MigratableDatesController.show().url)

        session(result).get(SessionKeys.migratableDatesKey) shouldBe Some(Json.toJson(testDates).toString)
      }

    }

    "vat number is in session but store vat is unsuccessful as client is deregistered" when {
      "go to the deregistered VAT number page" in {
        mockAgentAuthSuccess(Enrolments(Set(testAgentEnrolment)))
        mockOrchestrate(enrolments = Enrolments(Set(testAgentEnrolment)), vatNumber = testVatNumber)(Future.successful(Deregistered))

        val result = TestConfirmVatNumberController.submit(testPostRequest.withSession(SessionKeys.vatNumberKey -> testVatNumber))
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(errorRoutes.DeregisteredVatNumberController.show().url)
      }
    }

    "vat number is in session but store vat is unsuccessful as vrn was registered less than a week ago" when {
      "go to the deregistered VAT number page" in {
        mockAgentAuthSuccess(Enrolments(Set(testAgentEnrolment)))
        mockOrchestrate(enrolments = Enrolments(Set(testAgentEnrolment)), vatNumber = testVatNumber)(Future.successful(RecentlyRegistered))

        val result = TestConfirmVatNumberController.submit(testPostRequest.withSession(SessionKeys.vatNumberKey -> testVatNumber))
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(errorRoutes.RecentlyRegisteredVatNumberController.show().url)
      }
    }

    "vat number is in session but store vat is unsuccessful" should {
      "throw internal server exception" in {
        mockAgentAuthSuccess(Enrolments(Set(testAgentEnrolment)))
        mockOrchestrate(enrolments = Enrolments(Set(testAgentEnrolment)), vatNumber = testVatNumber)(Future.failed(new InternalServerException("")))

        intercept[InternalServerException] {
          TestConfirmVatNumberController.submit(testPostRequest.withSession(SessionKeys.vatNumberKey -> testVatNumber))
        }
      }
    }

    "store vat is unsuccessful" when {
      "vat number is already subscribed" should {
        "redirect to the already subscribed page" in {
          mockAgentAuthSuccess(Enrolments(Set(testAgentEnrolment)))
          mockOrchestrate(enrolments = Enrolments(Set(testAgentEnrolment)), vatNumber = testVatNumber)(Future.successful(AlreadySubscribed(isOverseas = false)))

          val result = TestConfirmVatNumberController.submit(testPostRequest.withSession(SessionKeys.vatNumberKey -> testVatNumber))
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(errorRoutes.AlreadySignedUpController.show().url)
        }
      }

      "vat number is already subscribed and is overseas" should {
        "redirect to the already subscribed page" in {
          mockAgentAuthSuccess(Enrolments(Set(testAgentEnrolment)))
          mockOrchestrate(enrolments = Enrolments(Set(testAgentEnrolment)), vatNumber = testVatNumber)(Future.successful(AlreadySubscribed(isOverseas = true)))

          val result = TestConfirmVatNumberController.submit(testPostRequest.withSession(SessionKeys.vatNumberKey -> testVatNumber))
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(errorRoutes.AlreadySignedUpController.show().url)
        }
      }

      "vat migration is in progress" should {
        "redirect to the migration in progress error page" in {
          mockAgentAuthSuccess(Enrolments(Set(testAgentEnrolment)))
          mockOrchestrate(enrolments = Enrolments(Set(testAgentEnrolment)), vatNumber = testVatNumber)(Future.successful(MigrationInProgress))

          val result = TestConfirmVatNumberController.submit(testPostRequest.withSession(SessionKeys.vatNumberKey -> testVatNumber))
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(errorRoutes.MigrationInProgressErrorController.show().url)
        }
      }
    }

    "vat number is not in session" should {
      "redirect to capture vat number" in {
        mockAgentAuthSuccess(Enrolments(Set(testAgentEnrolment)))

        val result = TestConfirmVatNumberController.submit(testPostRequest)
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.CaptureVatNumberController.show().url)
      }
    }
  }

}
