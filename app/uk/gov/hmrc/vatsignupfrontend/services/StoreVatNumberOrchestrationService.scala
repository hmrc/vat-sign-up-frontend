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

import javax.inject.{Inject, Singleton}
import uk.gov.hmrc.auth.core.Enrolments
import uk.gov.hmrc.http.{HeaderCarrier, InternalServerException}
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch.FeatureSwitching
import uk.gov.hmrc.vatsignupfrontend.httpparsers.{ClaimSubscriptionHttpParser, StoreMigratedVatNumberHttpParser}
import uk.gov.hmrc.vatsignupfrontend.models.MigratableDates
import uk.gov.hmrc.vatsignupfrontend.services.StoreVatNumberOrchestrationService._
import uk.gov.hmrc.vatsignupfrontend.services.StoreVatNumberService.{StoreVatNumberFailure, StoreVatNumberSuccess}
import uk.gov.hmrc.vatsignupfrontend.utils.EnrolmentUtils._

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class StoreVatNumberOrchestrationService @Inject()(checkVatNumberEligibilityService: CheckVatNumberEligibilityService,
                                                   storeMigratedVatNumberService: StoreMigratedVatNumberService,
                                                   storeVatNumberService: StoreVatNumberService,
                                                   claimSubscriptionService: ClaimSubscriptionService
                                                  )(implicit ec: ExecutionContext) extends FeatureSwitching {

  // scalastyle:off
  def orchestrate(enrolments: Enrolments, vatNumber: String)(implicit headerCarrier: HeaderCarrier): Future[StoreVatNumberOrchestrationServiceResponse] = {
    val isEnrolled = enrolments.getAnyVatNumber.contains(vatNumber)
    val isAgent = enrolments.agentReferenceNumber.isDefined

    checkVatNumberEligibilityService.checkEligibility(vatNumber).flatMap {
      case Eligible(isOverseas, isMigrated, false) if isMigrated && (isEnrolled || isAgent) =>
        storeMigratedVatNumberService.storeVatNumber(vatNumber, None, None) map {
          case Right(StoreMigratedVatNumberHttpParser.StoreMigratedVatNumberSuccess) =>
            VatNumberStored(isOverseas, isDirectDebit = false, isMigrated)
          case Left(StoreMigratedVatNumberHttpParser.KnownFactsMismatch) =>
            KnownFactsMismatch
          case Left(StoreMigratedVatNumberHttpParser.NoAgentClientRelationship) =>
            NoAgentClientRelationship
        }
      case Eligible(isOverseas, _, _) if enrolments.mtdVatNumber.isDefined =>
        // If user already has MTD-VAT enrolment, do not call legacy store VRN code as it will attempt to claim enrolment and fail
        Future.successful(AlreadySubscribed(isOverseas))
      case Eligible(_, _, true) =>
        Future.successful(RecentlyRegistered)
      case Eligible(isOverseas, _, _) if isEnrolled =>
        storeVatNumberService.storeVatNumber(vatNumber, isFromBta = false)
          .map(handleLegacyStoreVatNumberResponse(vatNumber, isOverseas))
      case Eligible(isOverseas, _, _) if isAgent =>
        storeVatNumberService.storeVatNumberDelegated(vatNumber)
          .map(handleLegacyStoreVatNumberResponse(vatNumber, isOverseas))
      case StoreVatNumberOrchestrationService.AlreadySubscribed(_) if isEnrolled && enrolments.mtdVatNumber.isEmpty =>
        claimSubscriptionService.claimSubscription(vatNumber, isFromBta = false).map {
          case Right(ClaimSubscriptionHttpParser.SubscriptionClaimed) =>
            StoreVatNumberOrchestrationService.SubscriptionClaimed
          case Left(ClaimSubscriptionHttpParser.AlreadyEnrolledOnDifferentCredential) =>
            StoreVatNumberOrchestrationService.AlreadyEnrolledOnDifferentCredential
          case Left(unexpectedError) =>
            throw new InternalServerException(s"Unexpected error in claim subscription for user with enrolment - $unexpectedError")
        }
      case eligibilityResponse =>
        Future.successful(eligibilityResponse)
    }
  }

  private def handleLegacyStoreVatNumberResponse(vatNumber: String, isOverseas: Boolean)(response: Either[StoreVatNumberFailure, StoreVatNumberSuccess]): StoreVatNumberOrchestrationServiceResponse =
    response match {
      case Right(StoreVatNumberService.VatNumberStored(isOverseas, isDirectDebit)) =>
        StoreVatNumberOrchestrationService.VatNumberStored(isOverseas, isDirectDebit, isMigrated = false)
      case Right(StoreVatNumberService.SubscriptionClaimed) =>
        StoreVatNumberOrchestrationService.SubscriptionClaimed
      case Left(StoreVatNumberService.IneligibleVatNumber(migratableDates)) if migratableDates.isEmpty =>
        StoreVatNumberOrchestrationService.Ineligible
      case Left(StoreVatNumberService.IneligibleVatNumber(migratableDates)) =>
        StoreVatNumberOrchestrationService.Inhibited(migratableDates)
      case Left(StoreVatNumberService.VatMigrationInProgress) =>
        StoreVatNumberOrchestrationService.MigrationInProgress
      case Left(StoreVatNumberService.VatNumberAlreadyEnrolled) =>
        StoreVatNumberOrchestrationService.AlreadyEnrolledOnDifferentCredential
      case Left(StoreVatNumberService.NoAgentClientRelationship) =>
        StoreVatNumberOrchestrationService.NoAgentClientRelationship
      case Left(StoreVatNumberService.InvalidVatNumber) =>
        StoreVatNumberOrchestrationService.InvalidVatNumber
      case Left(StoreVatNumberService.AlreadySubscribed) =>
        StoreVatNumberOrchestrationService.AlreadySubscribed(isOverseas)
    }
}

object StoreVatNumberOrchestrationService {

  sealed trait StoreVatNumberOrchestrationServiceResponse

  case class VatNumberStored(isOverseas: Boolean, isDirectDebit: Boolean, isMigrated: Boolean) extends StoreVatNumberOrchestrationServiceResponse

  case object Ineligible extends StoreVatNumberOrchestrationServiceResponse

  case object Deregistered extends StoreVatNumberOrchestrationServiceResponse

  case object SubscriptionClaimed extends StoreVatNumberOrchestrationServiceResponse

  case object MigrationInProgress extends StoreVatNumberOrchestrationServiceResponse

  case object KnownFactsMismatch extends StoreVatNumberOrchestrationServiceResponse

  case object NoAgentClientRelationship extends StoreVatNumberOrchestrationServiceResponse

  case object AlreadyEnrolledOnDifferentCredential extends StoreVatNumberOrchestrationServiceResponse

  case object InvalidVatNumber extends StoreVatNumberOrchestrationServiceResponse

  case class Inhibited(migratableDates: MigratableDates) extends StoreVatNumberOrchestrationServiceResponse

  case class Eligible(isOverseas: Boolean, isMigrated: Boolean, isNew: Boolean) extends StoreVatNumberOrchestrationServiceResponse

  case class AlreadySubscribed(isOverseas: Boolean) extends StoreVatNumberOrchestrationServiceResponse

  case object RecentlyRegistered extends StoreVatNumberOrchestrationServiceResponse

}

