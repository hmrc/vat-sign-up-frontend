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
import uk.gov.hmrc.http.{HeaderCarrier, InternalServerException}
import uk.gov.hmrc.vatsignupfrontend.connectors.VatNumberEligibilityConnector
import uk.gov.hmrc.vatsignupfrontend.httpparsers.VatNumberEligibilityHttpParser._
import uk.gov.hmrc.vatsignupfrontend.services.StoreVatNumberOrchestrationService.StoreVatNumberOrchestrationServiceResponse

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class CheckVatNumberEligibilityService @Inject()(vatNumberEligibilityConnector: VatNumberEligibilityConnector
                                                )(implicit ec: ExecutionContext) {

  def checkEligibility(vatNumber: String)(implicit headerCarrier: HeaderCarrier): Future[StoreVatNumberOrchestrationServiceResponse] = {
    vatNumberEligibilityConnector.checkVatNumberEligibility(vatNumber).map {
      case Right(MigrationInProgress) =>
        StoreVatNumberOrchestrationService.MigrationInProgress
      case Right(AlreadySubscribed(isOverseas)) =>
        StoreVatNumberOrchestrationService.AlreadySubscribed(isOverseas)
      case Right(Ineligible) =>
        StoreVatNumberOrchestrationService.Ineligible
      case Right(Deregistered) =>
        StoreVatNumberOrchestrationService.Deregistered
      case Right(Inhibited(inhibitedDates)) =>
        StoreVatNumberOrchestrationService.Inhibited(inhibitedDates)
      case Right(Eligible(isOverseas, isMigrated, isNew)) =>
        StoreVatNumberOrchestrationService.Eligible(isOverseas, isMigrated, isNew)
      case Left(VatNumberNotFound) =>
        StoreVatNumberOrchestrationService.InvalidVatNumber
      case _ =>
        throw new InternalServerException("Unexpected response from new VAT number eligibility backend")
    }
  }

  def isOverseas(vatNumber: String)(implicit headerCarrier: HeaderCarrier): Future[Boolean] = {
    vatNumberEligibilityConnector.checkVatNumberEligibility(vatNumber).map {
      case Right(Eligible(isOverseas, _, _)) => isOverseas
      case Right(AlreadySubscribed(isOverseas)) => isOverseas
      case eligibilityStatus =>
        throw new InternalServerException(
          s"$vatNumber is ineligible to claim a subscription due to the following eligibility status: $eligibilityStatus."
        )
    }
  }
}


