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

import play.api.http.Status.INTERNAL_SERVER_ERROR
import uk.gov.hmrc.http.{HeaderCarrier, InternalServerException}
import uk.gov.hmrc.vatsignupfrontend.utils.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.connectors.mocks.MockStoreVatNumberConnector
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.httpparsers.{ClaimSubscriptionHttpParser, StoreVatNumberHttpParser}
import uk.gov.hmrc.vatsignupfrontend.models.MigratableDates
import uk.gov.hmrc.vatsignupfrontend.services.StoreVatNumberService._
import uk.gov.hmrc.vatsignupfrontend.services.mocks.MockClaimSubscriptionService

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class StoreVatNumberServiceSpec extends UnitSpec with MockStoreVatNumberConnector with MockClaimSubscriptionService {


  object TestStoreVatNumberService extends StoreVatNumberService(mockStoreVatNumberConnector, mockClaimSubscriptionService)

  implicit val hc = HeaderCarrier()

  "storeVatNumberDelegated" when {
    "the connector returns VatNumberStored" should {
      "return VatNumberStored(false) when the company is not overseas" in {
        mockStoreVatNumber(testVatNumber, isFromBta = false)(Future.successful(Right(StoreVatNumberHttpParser.VatNumberStored(isOverseas = false, isDirectDebit = false))))

        val res = await(TestStoreVatNumberService.storeVatNumberDelegated(testVatNumber))
        res shouldBe Right(VatNumberStored(isOverseas = false, isDirectDebit = false))
      }
      "return VatNumberStored(true) when the company is overseas" in {
        mockStoreVatNumber(testVatNumber, isFromBta = false)(Future.successful(Right(StoreVatNumberHttpParser.VatNumberStored(isOverseas = true, isDirectDebit = false))))

        val res = await(TestStoreVatNumberService.storeVatNumberDelegated(testVatNumber))
        res shouldBe Right(VatNumberStored(isOverseas = true, isDirectDebit = false))
      }
    }
    "the connector returns AlreadySubscribed" should {
      "return AlreadySubscribed" in {
        mockStoreVatNumber(testVatNumber, isFromBta = false)(Future.successful(Left(StoreVatNumberHttpParser.AlreadySubscribed)))

        val res = await(TestStoreVatNumberService.storeVatNumberDelegated(testVatNumber))
        res shouldBe Left(AlreadySubscribed)
      }
    }
    "the connector returns NoAgentClientRelationship" should {
      "return NoAgentClientRelationship" in {
        mockStoreVatNumber(testVatNumber, isFromBta = false)(Future.successful(Left(StoreVatNumberHttpParser.NoAgentClientRelationship)))

        val res = await(TestStoreVatNumberService.storeVatNumberDelegated(testVatNumber))
        res shouldBe Left(NoAgentClientRelationship)
      }
    }
    "the connector returns InvalidVatNumber" should {
      "return InvalidVatNumber" in {
        mockStoreVatNumber(testVatNumber, isFromBta = false)(Future.successful(Left(StoreVatNumberHttpParser.InvalidVatNumber)))

        val res = await(TestStoreVatNumberService.storeVatNumberDelegated(testVatNumber))
        res shouldBe Left(InvalidVatNumber)
      }
    }
    "the connector returns IneligibleVatNumber" should {
      "return IneligibleVatNumber and pass through the migratable dates" in {
        val testMigratableDates = MigratableDates(Some(testStartDate), Some(testEndDate))

        mockStoreVatNumber(testVatNumber, isFromBta = false)(Future.successful(Left(StoreVatNumberHttpParser.IneligibleVatNumber(testMigratableDates))))

        val res = await(TestStoreVatNumberService.storeVatNumberDelegated(testVatNumber))
        res shouldBe Left(IneligibleVatNumber(testMigratableDates))
      }
    }
    "the connector returns VatMigrationInProgress" should {
      "return VatMigrationInProgress" in {
        mockStoreVatNumber(testVatNumber, isFromBta = false)(
          Future.successful(Left(StoreVatNumberHttpParser.VatMigrationInProgress))
        )

        val res = await(TestStoreVatNumberService.storeVatNumberDelegated(testVatNumber))
        res shouldBe Left(VatMigrationInProgress)
      }
    }
    "the connector returns anything else" should {
      "throw an InternalServerException" in {
        mockStoreVatNumber(testVatNumber, isFromBta = false)(
          Future.successful(Left(StoreVatNumberHttpParser.StoreVatNumberFailureResponse(INTERNAL_SERVER_ERROR)))
        )

        intercept[InternalServerException](await(TestStoreVatNumberService.storeVatNumberDelegated(testVatNumber)))
      }
    }
  }
  "storeVatNumber with an enrolment" when {
    "the connector returns VatNumberStored" should {
      "return VatNumberStored(false) when the company is not overseas" in {
        mockStoreVatNumber(testVatNumber, isFromBta = false)(Future.successful(Right(StoreVatNumberHttpParser.VatNumberStored(isOverseas = false, isDirectDebit = false))))

        val res = await(TestStoreVatNumberService.storeVatNumber(testVatNumber, isFromBta = false))
        res shouldBe Right(VatNumberStored(isOverseas = false, isDirectDebit = false))
      }
      "return VatNumberStored(true) when the company is overseas" in {
        mockStoreVatNumber(testVatNumber, isFromBta = false)(Future.successful(Right(StoreVatNumberHttpParser.VatNumberStored(isOverseas = true, isDirectDebit = false))))

        val res = await(TestStoreVatNumberService.storeVatNumber(testVatNumber, isFromBta = false))
        res shouldBe Right(VatNumberStored(isOverseas = true, isDirectDebit = false))
      }
    }
    "the store vat number connector returns AlreadySubscribed" when {
      "the claim subscription connector returns SubscriptionClaimed" should {
        "return SubscriptionClaimed" in {
          mockStoreVatNumber(testVatNumber, isFromBta = false)(Future.successful(Left(StoreVatNumberHttpParser.AlreadySubscribed)))
          mockClaimSubscription(testVatNumber, isFromBta = false)(Future.successful(Right(ClaimSubscriptionHttpParser.SubscriptionClaimed)))

          val res = await(TestStoreVatNumberService.storeVatNumber(testVatNumber, isFromBta = false))
          res shouldBe Right(SubscriptionClaimed)
        }
      }
      "the claim subscription connector returns AlreadyEnrolledOnDifferentCredential" should {
        "return VatNumberAlreadyEnrolled" in {
          mockStoreVatNumber(testVatNumber, isFromBta = false)(
            Future.successful(Left(StoreVatNumberHttpParser.AlreadySubscribed))
          )
          mockClaimSubscription(testVatNumber, isFromBta = false)(
            Future.successful(Left(ClaimSubscriptionHttpParser.AlreadyEnrolledOnDifferentCredential))
          )

          val res = await(TestStoreVatNumberService.storeVatNumber(testVatNumber, isFromBta = false))
          res shouldBe Left(VatNumberAlreadyEnrolled)
        }
      }
      "the claim subscription connector returns anything else" should {
        "throw an InternalServerException" in {
          mockStoreVatNumber(testVatNumber, isFromBta = false)(Future.successful(Left(StoreVatNumberHttpParser.AlreadySubscribed)))
          mockClaimSubscription(testVatNumber, isFromBta = false)(
            Future.successful(Left(ClaimSubscriptionHttpParser.ClaimSubscriptionFailureResponse(INTERNAL_SERVER_ERROR, "error message")))
          )

          intercept[InternalServerException](await(TestStoreVatNumberService.storeVatNumber(testVatNumber, isFromBta = false)))
        }
      }
    }
    "the connector returns IneligibleVatNumber" should {
      "return IneligibleVatNumber and pass through the migratable dates" in {
        val testMigratableDates = MigratableDates(Some(testStartDate), Some(testEndDate))

        mockStoreVatNumber(testVatNumber, isFromBta = false)(Future.successful(Left(StoreVatNumberHttpParser.IneligibleVatNumber(testMigratableDates))))

        val res = await(TestStoreVatNumberService.storeVatNumber(testVatNumber, isFromBta = false))
        res shouldBe Left(IneligibleVatNumber(testMigratableDates))
      }
    }
    "the connector returns VatMigrationInProgress" should {
      "return VatMigrationInProgress" in {
        mockStoreVatNumber(testVatNumber, isFromBta = false)(
          Future.successful(Left(StoreVatNumberHttpParser.VatMigrationInProgress))
        )

        val res = await(TestStoreVatNumberService.storeVatNumber(testVatNumber, isFromBta = false))
        res shouldBe Left(VatMigrationInProgress)
      }
    }
    "the connector returns anything else" should {
      "throw an InternalServerException" in {
        mockStoreVatNumber(testVatNumber, isFromBta = false)(
          Future.successful(Left(StoreVatNumberHttpParser.StoreVatNumberFailureResponse(INTERNAL_SERVER_ERROR)))
        )

        intercept[InternalServerException](await(TestStoreVatNumberService.storeVatNumber(testVatNumber, isFromBta = false)))
      }
    }
  }
  "storeVatNumber with supplied known facts" when {
    "the connector returns VatNumberStored" should {
      "return VatNumberStored(false) when the company is not overseas" in {
        mockStoreVatNumber(
          vatNumber = testVatNumber,
          optPostCode = Some(testBusinessPostcode),
          registrationDate = testDateModel.toLocalDate.toString,
          optBox5Figure = Some(testBox5Figure),
          optLastReturnMonth = Some(testLastReturnMonthPeriod),
          isFromBta = false
        )(Future.successful(Right(StoreVatNumberHttpParser.VatNumberStored(isOverseas = false, isDirectDebit = false))))

        val res = await(TestStoreVatNumberService.storeVatNumber(
          vatNumber = testVatNumber,
          optPostCode = Some(testBusinessPostcode),
          registrationDate = testDateModel,
          optBox5Figure = Some(testBox5Figure),
          optLastReturnMonth = Some(testLastReturnMonthPeriod),
          isFromBta = false
        ))

        res shouldBe Right(VatNumberStored(isOverseas = false, isDirectDebit = false))
      }
    }
    "the store vat number connector returns AlreadySubscribed" when {
      "the claim subscription connector returns SubscriptionClaimed" should {
        "return SubscriptionClaimed" in {
          mockStoreVatNumber(
            vatNumber = testVatNumber,
            optPostCode = Some(testBusinessPostcode),
            registrationDate = testDateModel.toLocalDate.toString,
            optBox5Figure = Some(testBox5Figure),
            optLastReturnMonth = Some(testLastReturnMonthPeriod),
            isFromBta = false
          )(Future.successful(Left(StoreVatNumberHttpParser.AlreadySubscribed)))

          mockClaimSubscription(
            vatNumber = testVatNumber,
            optPostCode = Some(testBusinessPostcode),
            registrationDate = testDateModel,
            isFromBta = false
          )(Future.successful(Right(ClaimSubscriptionHttpParser.SubscriptionClaimed)))

          val res = await(TestStoreVatNumberService.storeVatNumber(
            vatNumber = testVatNumber,
            optPostCode = Some(testBusinessPostcode),
            registrationDate = testDateModel,
            optBox5Figure = Some(testBox5Figure),
            optLastReturnMonth = Some(testLastReturnMonthPeriod),
            isFromBta = false
          ))

          res shouldBe Right(SubscriptionClaimed)
        }
      }
      "the claim subscription connector returns AlreadyEnrolledOnDifferentCredential" should {
        "return VatNumberAlreadyEnrolled" in {
          mockStoreVatNumber(
            vatNumber = testVatNumber,
            optPostCode = Some(testBusinessPostcode),
            registrationDate = testDateModel.toLocalDate.toString,
            optBox5Figure = Some(testBox5Figure),
            optLastReturnMonth = Some(testLastReturnMonthPeriod),
            isFromBta = false
          )(Future.successful(Left(StoreVatNumberHttpParser.AlreadySubscribed)))

          mockClaimSubscription(
            vatNumber = testVatNumber,
            optPostCode = Some(testBusinessPostcode),
            registrationDate = testDateModel,
            isFromBta = false
          )(Future.successful(Left(ClaimSubscriptionHttpParser.AlreadyEnrolledOnDifferentCredential)))

          val res = await(TestStoreVatNumberService.storeVatNumber(
            vatNumber = testVatNumber,
            optPostCode = Some(testBusinessPostcode),
            registrationDate = testDateModel,
            optBox5Figure = Some(testBox5Figure),
            optLastReturnMonth = Some(testLastReturnMonthPeriod),
            isFromBta = false
          ))

          res shouldBe Left(VatNumberAlreadyEnrolled)
        }
      }
      "the claim subscription connector returns KnownFactsMismatch" should {
        "return KnownFactsMismatch" in {
          mockStoreVatNumber(
            vatNumber = testVatNumber,
            optPostCode = Some(testBusinessPostcode),
            registrationDate = testDateModel.toLocalDate.toString,
            optBox5Figure = Some(testBox5Figure),
            optLastReturnMonth = Some(testLastReturnMonthPeriod),
            isFromBta = false
          )(Future.successful(Left(StoreVatNumberHttpParser.AlreadySubscribed)))

          mockClaimSubscription(
            vatNumber = testVatNumber,
            optPostCode = Some(testBusinessPostcode),
            registrationDate = testDateModel,
            isFromBta = false
          )(Future.successful(Left(ClaimSubscriptionHttpParser.KnownFactsMismatch)))

          val res = await(TestStoreVatNumberService.storeVatNumber(
            vatNumber = testVatNumber,
            optPostCode = Some(testBusinessPostcode),
            registrationDate = testDateModel,
            optBox5Figure = Some(testBox5Figure),
            optLastReturnMonth = Some(testLastReturnMonthPeriod),
            isFromBta = false
          ))

          res shouldBe Left(KnownFactsMismatch)
        }
      }
      "the claim subscription connector returns anything else" should {
        "throw an InternalServerException" in {
          mockStoreVatNumber(
            vatNumber = testVatNumber,
            optPostCode = Some(testBusinessPostcode),
            registrationDate = testDateModel.toLocalDate.toString,
            optBox5Figure = Some(testBox5Figure),
            optLastReturnMonth = Some(testLastReturnMonthPeriod),
            isFromBta = false
          )(Future.successful(Left(StoreVatNumberHttpParser.AlreadySubscribed)))

          mockClaimSubscription(
            vatNumber = testVatNumber,
            optPostCode = Some(testBusinessPostcode),
            registrationDate = testDateModel,
            isFromBta = false
          )(Future.successful(Left(ClaimSubscriptionHttpParser.ClaimSubscriptionFailureResponse(INTERNAL_SERVER_ERROR, "error message"))))

          intercept[InternalServerException](await(TestStoreVatNumberService.storeVatNumber(
            vatNumber = testVatNumber,
            optPostCode = Some(testBusinessPostcode),
            registrationDate = testDateModel,
            optBox5Figure = Some(testBox5Figure),
            optLastReturnMonth = Some(testLastReturnMonthPeriod),
            isFromBta = false
          )))
        }
      }
    }
    "the connector returns IneligibleVatNumber" should {
      "return IneligibleVatNumber and pass through the migratable dates" in {
        val testMigratableDates = MigratableDates(Some(testStartDate), Some(testEndDate))

        mockStoreVatNumber(
          vatNumber = testVatNumber,
          optPostCode = Some(testBusinessPostcode),
          registrationDate = testDateModel.toLocalDate.toString,
          optBox5Figure = Some(testBox5Figure),
          optLastReturnMonth = Some(testLastReturnMonthPeriod),
          isFromBta = false
        )(Future.successful(Left(StoreVatNumberHttpParser.IneligibleVatNumber(testMigratableDates))))

        val res = await(TestStoreVatNumberService.storeVatNumber(
          vatNumber = testVatNumber,
          optPostCode = Some(testBusinessPostcode),
          registrationDate = testDateModel,
          optBox5Figure = Some(testBox5Figure),
          optLastReturnMonth = Some(testLastReturnMonthPeriod),
          isFromBta = false
        ))

        res shouldBe Left(IneligibleVatNumber(testMigratableDates))
      }
    }
    "the connector returns KnownFactsMismatch" should {
      "return KnownFactsMismatch and pass through the migratable dates" in {
        val testMigratableDates = MigratableDates(Some(testStartDate), Some(testEndDate))

        mockStoreVatNumber(
          vatNumber = testVatNumber,
          optPostCode = Some(testBusinessPostcode),
          registrationDate = testDateModel.toLocalDate.toString,
          optBox5Figure = Some(testBox5Figure),
          optLastReturnMonth = Some(testLastReturnMonthPeriod),
          isFromBta = false
        )(Future.successful(Left(StoreVatNumberHttpParser.KnownFactsMismatch)))

        val res = await(TestStoreVatNumberService.storeVatNumber(
          vatNumber = testVatNumber,
          optPostCode = Some(testBusinessPostcode),
          registrationDate = testDateModel,
          optBox5Figure = Some(testBox5Figure),
          optLastReturnMonth = Some(testLastReturnMonthPeriod),
          isFromBta = false
        ))

        res shouldBe Left(KnownFactsMismatch)
      }
    }
    "the connector returns VatMigrationInProgress" should {
      "return VatMigrationInProgress" in {
        mockStoreVatNumber(
          vatNumber = testVatNumber,
          optPostCode = Some(testBusinessPostcode),
          registrationDate = testDateModel.toLocalDate.toString,
          optBox5Figure = Some(testBox5Figure),
          optLastReturnMonth = Some(testLastReturnMonthPeriod),
          isFromBta = false
        )(Future.successful(Left(StoreVatNumberHttpParser.VatMigrationInProgress)))

        val res = await(TestStoreVatNumberService.storeVatNumber(
          vatNumber = testVatNumber,
          optPostCode = Some(testBusinessPostcode),
          registrationDate = testDateModel,
          optBox5Figure = Some(testBox5Figure),
          optLastReturnMonth = Some(testLastReturnMonthPeriod),
          isFromBta = false
        ))

        res shouldBe Left(VatMigrationInProgress)
      }
    }
    "the connector returns anything else" should {
      "throw an InternalServerException" in {
        mockStoreVatNumber(
          vatNumber = testVatNumber,
          optPostCode = Some(testBusinessPostcode),
          registrationDate = testDateModel.toLocalDate.toString,
          optBox5Figure = Some(testBox5Figure),
          optLastReturnMonth = Some(testLastReturnMonthPeriod),
          isFromBta = false
        )(Future.successful(Left(StoreVatNumberHttpParser.StoreVatNumberFailureResponse(INTERNAL_SERVER_ERROR))))

        intercept[InternalServerException](await(TestStoreVatNumberService.storeVatNumber(
          vatNumber = testVatNumber,
          optPostCode = Some(testBusinessPostcode),
          registrationDate = testDateModel,
          optBox5Figure = Some(testBox5Figure),
          optLastReturnMonth = Some(testLastReturnMonthPeriod),
          isFromBta = false
        )))
      }
    }
  }
}
