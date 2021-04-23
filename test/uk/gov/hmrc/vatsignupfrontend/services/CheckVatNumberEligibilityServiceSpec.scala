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

package uk.gov.hmrc.vatsignupfrontend.services

import org.scalatest.BeforeAndAfterEach
import uk.gov.hmrc.http.{HeaderCarrier, InternalServerException}
import uk.gov.hmrc.vatsignupfrontend.connectors.mocks.MockVatNumberEligibilityConnector
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants.{testEndDate, testStartDate, testVatNumber}
import uk.gov.hmrc.vatsignupfrontend.httpparsers.VatNumberEligibilityHttpParser._
import uk.gov.hmrc.vatsignupfrontend.models.MigratableDates
import uk.gov.hmrc.vatsignupfrontend.utils.UnitSpec

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class CheckVatNumberEligibilityServiceSpec extends UnitSpec with MockVatNumberEligibilityConnector with BeforeAndAfterEach {

  object TestService extends CheckVatNumberEligibilityService(
    mockVatNumberEligibilityConnector
  )

  implicit val hc: HeaderCarrier = HeaderCarrier()

  val testMigratableDates: MigratableDates = MigratableDates(Some(testStartDate), Some(testEndDate))

  "checkVatNumberEligibility" when {
    "the connector returns Eligible" should {
      "return Eligible and isMigrated" in {
        mockVatNumberEligibility(testVatNumber)(Future.successful(Right(Eligible(isOverseas = false, isMigrated = true, isNew = false))))

        val result = await(TestService.checkEligibility(testVatNumber))
        result shouldBe StoreVatNumberOrchestrationService.Eligible(isOverseas = false, isMigrated = true, isNew = false)
      }
    }

    "the connector returns MigrationInProgress" should {
      "return MigrationInProgress" in {
        mockVatNumberEligibility(testVatNumber)(Future.successful(Right(MigrationInProgress)))

        val result = await(TestService.checkEligibility(testVatNumber))
        result shouldBe StoreVatNumberOrchestrationService.MigrationInProgress
      }
    }

    "the connector returns AlreadySubscribed" should {
      "return AlreadySubscribed" in {
        mockVatNumberEligibility(testVatNumber)(Future.successful(Right(AlreadySubscribed(isOverseas = false))))

        val result = await(TestService.checkEligibility(testVatNumber))
        result shouldBe StoreVatNumberOrchestrationService.AlreadySubscribed(isOverseas = false)
      }
    }

    "the connector returns AlreadySubscribed (overseas)" should {
      "return AlreadySubscribed" in {
        mockVatNumberEligibility(testVatNumber)(Future.successful(Right(AlreadySubscribed(isOverseas = true))))

        val result = await(TestService.checkEligibility(testVatNumber))
        result shouldBe StoreVatNumberOrchestrationService.AlreadySubscribed(isOverseas = true)
      }
    }

    "the connector returns Ineligible" should {
      "return Ineligible" in {
        mockVatNumberEligibility(testVatNumber)(Future.successful(Right(Ineligible)))

        val result = await(TestService.checkEligibility(testVatNumber))
        result shouldBe StoreVatNumberOrchestrationService.Ineligible
      }
    }

    "the connector returns Deregistered" should {
      "return Deregistered" in {
        mockVatNumberEligibility(testVatNumber)(Future.successful(Right(Deregistered)))

        val result = await(TestService.checkEligibility(testVatNumber))
        result shouldBe StoreVatNumberOrchestrationService.Deregistered
      }
    }

    "the connector returns Inhibited with dates" should {
      "return Inhibited" in {
        mockVatNumberEligibility(testVatNumber)(Future.successful(Right(Inhibited(testMigratableDates))))

        val result = await(TestService.checkEligibility(testVatNumber))
        result shouldBe StoreVatNumberOrchestrationService.Inhibited(testMigratableDates)
      }
    }

    "the connector returns VatNumberNotFound" should {
      "return VatNumberNotFound" in {
        mockVatNumberEligibility(testVatNumber)(Future.successful(Left(VatNumberNotFound)))

        val result = await(TestService.checkEligibility(testVatNumber))

        result shouldBe StoreVatNumberOrchestrationService.InvalidVatNumber
      }
    }
  }

  "isOverseas" when {
    "the connector returns Eligible(isOverseas, _)" should {
      "return isOverseas" in {
        mockVatNumberEligibility(testVatNumber)(Future.successful(Right(Eligible(isOverseas = true, isMigrated = true, isNew = false))))

        val result = await(TestService.isOverseas(testVatNumber))

        result shouldBe true
      }
    }

    "the connector returns AlreadySubscribed(isOverseas)" should {
      "return isOverseas" in {
        mockVatNumberEligibility(testVatNumber)(Future.successful(Right(AlreadySubscribed(isOverseas = false))))

        val result = await(TestService.isOverseas(testVatNumber))

        result shouldBe false
      }
    }

    "the connector returns anything else" should {
      "fail" in {
        mockVatNumberEligibility(testVatNumber)(Future.successful(Right(MigrationInProgress)))

        intercept[InternalServerException](await(TestService.isOverseas(testVatNumber)))
      }
    }
  }
}
