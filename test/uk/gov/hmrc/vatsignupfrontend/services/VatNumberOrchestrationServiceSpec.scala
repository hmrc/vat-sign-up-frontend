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

package uk.gov.hmrc.vatsignupfrontend.services

import org.scalatest.BeforeAndAfterEach
import play.api.http.Status._
import uk.gov.hmrc.auth.core.Enrolments
import uk.gov.hmrc.http.{HeaderCarrier, InternalServerException}
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.httpparsers.{ClaimSubscriptionHttpParser, StoreMigratedVatNumberHttpParser}
import uk.gov.hmrc.vatsignupfrontend.models.MigratableDates
import uk.gov.hmrc.vatsignupfrontend.services.StoreVatNumberOrchestrationService._
import uk.gov.hmrc.vatsignupfrontend.services.mocks._
import uk.gov.hmrc.vatsignupfrontend.utils.UnitSpec

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class VatNumberOrchestrationServiceSpec extends UnitSpec
  with MockCheckVatNumberEligibilityService
  with MockStoreMigratedVatNumberService
  with MockStoreVatNumberService
  with MockClaimSubscriptionService
  with BeforeAndAfterEach {

  implicit val hc: HeaderCarrier = HeaderCarrier()

  object TestService extends StoreVatNumberOrchestrationService(
    mockCheckVatNumberEligibilityService,
    mockStoreMigratedVatNumberService,
    mockStoreVatNumberService,
    mockClaimSubscriptionService
  )

  val testMigratableDates = MigratableDates(Some(testStartDate), Some(testEndDate))
  val testVatEnrolments: Enrolments = Enrolments(Set(testMtdVatEnrolment, testVatDecEnrolment))
  val testNoEnrolments: Enrolments = Enrolments(Set())

  "orchestrate" when {
    "enrolments are not provided" when {
      "the vat number is eligible" should {
        "return Eligible" in {
          mockCheckVatNumberEligibility(testVatNumber)(Future.successful(Eligible(isOverseas = true, isMigrated = false)))

          val res = TestService.orchestrate(testNoEnrolments, testVatNumber)

          await(res) shouldBe StoreVatNumberOrchestrationService.Eligible(isOverseas = true, isMigrated = false)
        }
      }

      "the vat number is ineligible" should {
        "return Ineligible" in {
          mockCheckVatNumberEligibility(testVatNumber)(Future.successful(Ineligible))

          val res = TestService.orchestrate(testNoEnrolments, testVatNumber)

          await(res) shouldBe StoreVatNumberOrchestrationService.Ineligible
        }
      }

      "the vat number is deregistered" should {
        "return Deregistered" in {
          mockCheckVatNumberEligibility(testVatNumber)(Future.successful(Deregistered))

          val res = TestService.orchestrate(testNoEnrolments, testVatNumber)

          await(res) shouldBe StoreVatNumberOrchestrationService.Deregistered
        }
      }

      "the vat number is inhibited" should {
        "return Inhibited" in {
          mockCheckVatNumberEligibility(testVatNumber)(Future.successful(Inhibited(testMigratableDates)))

          val res = TestService.orchestrate(testNoEnrolments, testVatNumber)

          await(res) shouldBe StoreVatNumberOrchestrationService.Inhibited(testMigratableDates)
        }
      }

      "the vat number is being migrated" should {
        "return MigrationInProgress" in {
          mockCheckVatNumberEligibility(testVatNumber)(Future.successful(MigrationInProgress))

          val res = TestService.orchestrate(testNoEnrolments, testVatNumber)

          await(res) shouldBe StoreVatNumberOrchestrationService.MigrationInProgress
        }
      }

      "the vat number is already subscribed" should {
        "return AlreadySubscribed" in {
          mockCheckVatNumberEligibility(testVatNumber)(Future.successful(StoreVatNumberOrchestrationService.AlreadySubscribed(isOverseas = false)))

          val res = TestService.orchestrate(testNoEnrolments, testVatNumber)

          await(res) shouldBe StoreVatNumberOrchestrationService.AlreadySubscribed(isOverseas = false)
        }
      }

      "check vat number eligibility fails" should {
        "throw an internal server exception" in {
          mockCheckVatNumberEligibility(testVatNumber)(Future.failed(new InternalServerException("")))


          intercept[InternalServerException](await(TestService.orchestrate(testNoEnrolments, testVatNumber)))
        }
      }
    }

    "enrolments are provided" when {
      "an eligible vat number has already been migrated to ETMP" when {
        "the user is not overseas" should {
          "return VatNumberStored" in {
            mockCheckVatNumberEligibility(testVatNumber)(Future.successful(Eligible(isOverseas = false, isMigrated = true)))
            mockStoreMigratedVatNumber(testVatNumber, None, None)(
              Future.successful(Right(StoreMigratedVatNumberHttpParser.StoreMigratedVatNumberSuccess))
            )

            val res = TestService.orchestrate(Enrolments(Set(testVatDecEnrolment)), testVatNumber)

            await(res) shouldBe StoreVatNumberOrchestrationService.VatNumberStored(isOverseas = false, isDirectDebit = false, isMigrated = true)
          }
        }
        "the user is overseas" should {
          "return VatNumberStored" in {
            mockCheckVatNumberEligibility(testVatNumber)(Future.successful(Eligible(isOverseas = true, isMigrated = true)))
            mockStoreMigratedVatNumber(testVatNumber, None, None)(
              Future.successful(Right(StoreMigratedVatNumberHttpParser.StoreMigratedVatNumberSuccess))
            )

            val res = TestService.orchestrate(Enrolments(Set(testVatDecEnrolment)), testVatNumber)

            await(res) shouldBe StoreVatNumberOrchestrationService.VatNumberStored(isOverseas = true, isDirectDebit = false, isMigrated = true)
          }
        }
      }

      "the vat number has not been migrated to ETMP yet" when {
        "the user already has both enrolments" should {
          "return AlreadySubscribed" in {
            mockCheckVatNumberEligibility(testVatNumber)(Future.successful(Eligible(isOverseas = false, isMigrated = false)))
            mockStoreVatNumber(testVatNumber, isFromBta = false)(Future.successful(Right(StoreVatNumberService.VatNumberStored(isDirectDebit = false))))

            val res = TestService.orchestrate(Enrolments(Set(testMtdVatEnrolment, testVatDecEnrolment)), testVatNumber)

            await(res) shouldBe StoreVatNumberOrchestrationService.AlreadySubscribed(isOverseas = false)
          }
        }

        "the user already has an MTD-VAT enrolment" should {
          "return AlreadySubscribed" in {
            mockCheckVatNumberEligibility(testVatNumber)(Future.successful(Eligible(isOverseas = false, isMigrated = false)))
            mockStoreVatNumber(testVatNumber, isFromBta = false)(Future.successful(Right(StoreVatNumberService.VatNumberStored(isDirectDebit = false))))

            val res = TestService.orchestrate(Enrolments(Set(testMtdVatEnrolment)), testVatNumber)

            await(res) shouldBe StoreVatNumberOrchestrationService.AlreadySubscribed(isOverseas = false)
          }
        }

        "the user only has the legacy VAT enrolment" should {
          "return VatNumberStored" in {
            mockCheckVatNumberEligibility(testVatNumber)(Future.successful(Eligible(isOverseas = false, isMigrated = false)))
            mockStoreVatNumber(testVatNumber, isFromBta = false)(Future.successful(Right(StoreVatNumberService.VatNumberStored(isDirectDebit = false))))

            val res = TestService.orchestrate(Enrolments(Set(testVatDecEnrolment)), testVatNumber)

            await(res) shouldBe StoreVatNumberOrchestrationService.VatNumberStored(isOverseas = false, isDirectDebit = false, isMigrated = false)
          }
        }
      }

      "the vat number is ineligible" should {
        "return Ineligible" in {
          mockCheckVatNumberEligibility(testVatNumber)(Future.successful(Ineligible))

          val res = TestService.orchestrate(Enrolments(Set(testVatDecEnrolment)), testVatNumber)

          await(res) shouldBe StoreVatNumberOrchestrationService.Ineligible
        }
      }

      "the vat number is deregistered" should {
        "return Deregistered" in {
          mockCheckVatNumberEligibility(testVatNumber)(Future.successful(Deregistered))

          val res = TestService.orchestrate(Enrolments(Set(testVatDecEnrolment)), testVatNumber)

          await(res) shouldBe StoreVatNumberOrchestrationService.Deregistered
        }
      }

      "the vat number is inhibited" should {
        "return Inhibited" in {
          mockCheckVatNumberEligibility(testVatNumber)(Future.successful(Inhibited(testMigratableDates)))

          val res = TestService.orchestrate(Enrolments(Set(testVatDecEnrolment)), testVatNumber)

          await(res) shouldBe StoreVatNumberOrchestrationService.Inhibited(testMigratableDates)
        }
      }

      "the vat number is currently being migrated to ETMP" should {
        "return MigrationInProgress" in {
          mockCheckVatNumberEligibility(testVatNumber)(Future.successful(MigrationInProgress))

          val res = TestService.orchestrate(Enrolments(Set(testVatDecEnrolment)), testVatNumber)

          await(res) shouldBe StoreVatNumberOrchestrationService.MigrationInProgress
        }
      }

      "the vat number is already subscribed" when {
        "subscription is claimed successfully" should {
          "return SubscriptionClaimed" in {
            mockCheckVatNumberEligibility(testVatNumber)(Future.successful(StoreVatNumberOrchestrationService.AlreadySubscribed(isOverseas = false)))
            mockClaimSubscription(testVatNumber, isFromBta = false)(
              Future.successful(Right(ClaimSubscriptionHttpParser.SubscriptionClaimed))
            )

            val res = TestService.orchestrate(Enrolments(Set(testVatDecEnrolment)), testVatNumber)

            await(res) shouldBe StoreVatNumberOrchestrationService.SubscriptionClaimed
          }
        }

        "the vat number is already enrolled on a different cred" should {
          "return AlreadyEnrolledOnDifferentCredential" in {
            mockCheckVatNumberEligibility(testVatNumber)(Future.successful(StoreVatNumberOrchestrationService.AlreadySubscribed(isOverseas = false)))
            mockClaimSubscription(testVatNumber, isFromBta = false)(
              Future.successful(Left(ClaimSubscriptionHttpParser.AlreadyEnrolledOnDifferentCredential))
            )

            val res = TestService.orchestrate(Enrolments(Set(testVatDecEnrolment)), testVatNumber)

            await(res) shouldBe StoreVatNumberOrchestrationService.AlreadyEnrolledOnDifferentCredential
          }
        }

        "subscription claim fails" should {
          "throw and internal server exception" in {
            mockCheckVatNumberEligibility(testVatNumber)(Future.successful(StoreVatNumberOrchestrationService.AlreadySubscribed(isOverseas = false)))
            mockClaimSubscription(testVatNumber, isFromBta = false)(
              Future.successful(Left(ClaimSubscriptionHttpParser.ClaimSubscriptionFailureResponse(BAD_REQUEST, "error message")))
            )

            val res = TestService.orchestrate(Enrolments(Set(testVatDecEnrolment)), testVatNumber)

            intercept[InternalServerException](await(res))
          }
        }
      }

      "the vat number eligibility check fails" should {
        "throw internal server exception" in {
          mockCheckVatNumberEligibility(testVatNumber)(Future.failed(new InternalServerException("")))

          val res = TestService.orchestrate(Enrolments(Set(testVatDecEnrolment)), testVatNumber)

          intercept[InternalServerException](await(res))
        }
      }

      "the user has MTD VAT enrolment and the vat number is already signed up" should {
        "return AlreadySubscribed" in {
          mockCheckVatNumberEligibility(testVatNumber)(Future.successful(StoreVatNumberOrchestrationService.AlreadySubscribed(isOverseas = false)))

          val res = TestService.orchestrate(Enrolments(Set(testMtdVatEnrolment)), testVatNumber)

          await(res) shouldBe StoreVatNumberOrchestrationService.AlreadySubscribed(isOverseas = false)
        }
      }

      "the user has multiple enrolments and the vat number is already signed up" should {
        "return AlreadySubscribed" in {
          mockCheckVatNumberEligibility(testVatNumber)(Future.successful(StoreVatNumberOrchestrationService.AlreadySubscribed(isOverseas = false)))

          val res = TestService.orchestrate(Enrolments(Set(testVatDecEnrolment, testMtdVatEnrolment)), testVatNumber)

          await(res) shouldBe StoreVatNumberOrchestrationService.AlreadySubscribed(isOverseas = false)
        }

      }
    }
  }
  "the user has an agent enrolment" should {
    "an eligible vat number has already been migrated to ETMP" when {
      "the user is not overseas" should {
        "return VatNumberStored" in {
          mockCheckVatNumberEligibility(testVatNumber)(Future.successful(Eligible(isOverseas = false, isMigrated = true)))
          mockStoreMigratedVatNumber(testVatNumber, None, None)(
            Future.successful(Right(StoreMigratedVatNumberHttpParser.StoreMigratedVatNumberSuccess))
          )

          val res = TestService.orchestrate(Enrolments(Set(testAgentEnrolment)), testVatNumber)

          await(res) shouldBe StoreVatNumberOrchestrationService.VatNumberStored(isOverseas = false, isDirectDebit = false, isMigrated = true)
        }
      }

      "the user is overseas" should {
        "return VatNumberStored" in {
          mockCheckVatNumberEligibility(testVatNumber)(Future.successful(Eligible(isOverseas = true, isMigrated = true)))
          mockStoreMigratedVatNumber(testVatNumber, None, None)(
            Future.successful(Right(StoreMigratedVatNumberHttpParser.StoreMigratedVatNumberSuccess))
          )

          val res = TestService.orchestrate(Enrolments(Set(testAgentEnrolment)), testVatNumber)

          await(res) shouldBe StoreVatNumberOrchestrationService.VatNumberStored(isOverseas = true, isDirectDebit = false, isMigrated = true)
        }
      }
    }

    "the vat number has not been migrated to ETMP yet" should {
      "return VatNumberStored" in {
        mockCheckVatNumberEligibility(testVatNumber)(Future.successful(Eligible(isOverseas = false, isMigrated = false)))
        mockStoreVatNumberDelegated(testVatNumber)(Future.successful(Right(StoreVatNumberService.VatNumberStored(isDirectDebit = false))))

        val res = TestService.orchestrate(Enrolments(Set(testAgentEnrolment)), testVatNumber)

        await(res) shouldBe StoreVatNumberOrchestrationService.VatNumberStored(isOverseas = false, isDirectDebit = false, isMigrated = false)
      }
    }

    "the vat number is ineligible" should {
      "return Ineligible" in {
        mockCheckVatNumberEligibility(testVatNumber)(Future.successful(Ineligible))

        val res = TestService.orchestrate(Enrolments(Set(testAgentEnrolment)), testVatNumber)

        await(res) shouldBe StoreVatNumberOrchestrationService.Ineligible
      }
    }

    "the vat number is deregistered" should {
      "return Deregistered" in {
        mockCheckVatNumberEligibility(testVatNumber)(Future.successful(Deregistered))

        val res = TestService.orchestrate(Enrolments(Set(testAgentEnrolment)), testVatNumber)

        await(res) shouldBe StoreVatNumberOrchestrationService.Deregistered
      }
    }

    "the vat number is inhibited" should {
      "return Inhibited" in {
        mockCheckVatNumberEligibility(testVatNumber)(Future.successful(Inhibited(testMigratableDates)))

        val res = TestService.orchestrate(Enrolments(Set(testAgentEnrolment)), testVatNumber)

        await(res) shouldBe StoreVatNumberOrchestrationService.Inhibited(testMigratableDates)
      }
    }

    "the vat number is currently being migrated to ETMP" should {
      "return MigrationInProgress" in {
        mockCheckVatNumberEligibility(testVatNumber)(Future.successful(MigrationInProgress))

        val res = TestService.orchestrate(Enrolments(Set(testAgentEnrolment)), testVatNumber)

        await(res) shouldBe StoreVatNumberOrchestrationService.MigrationInProgress
      }
    }

    "the vat number eligibility check fails" should {
      "throw internal server exception" in {
        mockCheckVatNumberEligibility(testVatNumber)(Future.failed(new InternalServerException("")))

        val res = TestService.orchestrate(Enrolments(Set(testAgentEnrolment)), testVatNumber)

        intercept[InternalServerException](await(res))
      }
    }

    "the vat number is already signed up" should {
      "return AlreadySubscribed" in {
        mockCheckVatNumberEligibility(testVatNumber)(Future.successful(StoreVatNumberOrchestrationService.AlreadySubscribed(isOverseas = false)))

        val res = TestService.orchestrate(Enrolments(Set(testAgentEnrolment)), testVatNumber)

        await(res) shouldBe StoreVatNumberOrchestrationService.AlreadySubscribed(isOverseas = false)
      }
    }

    "the vat number has No Agent Client Relationship" should {
      "return NoAgentClientRelationship" in {
        mockCheckVatNumberEligibility(testVatNumber)(Future.successful(StoreVatNumberOrchestrationService.NoAgentClientRelationship))

        val res = TestService.orchestrate(Enrolments(Set(testAgentEnrolment)), testVatNumber)

        await(res) shouldBe StoreVatNumberOrchestrationService.NoAgentClientRelationship
      }
    }

    "the vat number is Invalid" should {
      "return InvalidVatNumber" in {
        mockCheckVatNumberEligibility(testVatNumber)(Future.successful(StoreVatNumberOrchestrationService.InvalidVatNumber))

        val res = TestService.orchestrate(Enrolments(Set(testAgentEnrolment)), testVatNumber)

        await(res) shouldBe StoreVatNumberOrchestrationService.InvalidVatNumber
      }
    }
  }
}
