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

import javax.inject.{Inject, Singleton}
import play.api.mvc.{Action, AnyContent}
import uk.gov.hmrc.auth.core.retrieve.Retrievals
import uk.gov.hmrc.http.InternalServerException
import uk.gov.hmrc.vatsignupfrontend.SessionKeys._
import uk.gov.hmrc.vatsignupfrontend.config.VatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.config.auth.AgentEnrolmentPredicate
import uk.gov.hmrc.vatsignupfrontend.controllers.AuthenticatedController
import uk.gov.hmrc.vatsignupfrontend.controllers.agent.error.{routes => errorRoutes}
import uk.gov.hmrc.vatsignupfrontend.models.Overseas
import uk.gov.hmrc.vatsignupfrontend.services.StoreVatNumberOrchestrationService
import uk.gov.hmrc.vatsignupfrontend.services.StoreVatNumberOrchestrationService._
import uk.gov.hmrc.vatsignupfrontend.utils.SessionUtils.ResultUtils
import uk.gov.hmrc.vatsignupfrontend.utils.VatNumberChecksumValidation
import uk.gov.hmrc.vatsignupfrontend.views.html.agent.confirm_vat_number

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class ConfirmVatNumberController @Inject()(storeVatNumberOrchestrationService: StoreVatNumberOrchestrationService,
                                           view: confirm_vat_number)
                                          (implicit ec: ExecutionContext,
                                           vcc: VatControllerComponents)
  extends AuthenticatedController(AgentEnrolmentPredicate) {

  val show: Action[AnyContent] = Action.async {
    implicit request =>
      authorised() {
        request.session.get(vatNumberKey) match {
          case Some(vatNumber) if vatNumber.nonEmpty =>
            Future.successful(
              Ok(view(vatNumber, routes.ConfirmVatNumberController.submit()))
            )
          case _ =>
            Future.successful(
              Redirect(routes.CaptureVatNumberController.show())
            )
        }
      }
  }

  val submit: Action[AnyContent] = Action.async {
    implicit request =>
      authorised()(Retrievals.allEnrolments) {
        enrolments =>
          request.session.get(vatNumberKey) match {
            case Some(vatNumber) if vatNumber.nonEmpty =>
              if (VatNumberChecksumValidation.isValidChecksum(vatNumber))
                storeVatNumberOrchestrationService.orchestrate(enrolments, vatNumber).map {
                  case VatNumberStored(isOverseas, isDirectDebit, isMigrated) =>
                    Redirect(routes.CaptureBusinessEntityController.show())
                      .addingToSession(hasDirectDebitKey, isDirectDebit)
                      .addingToSession(isMigratedKey, isMigrated)
                      .conditionallyAddingToSession(businessEntityKey, Overseas.toString, isOverseas)
                  case NoAgentClientRelationship =>
                    Redirect(errorRoutes.NoAgentClientRelationshipController.show())
                  case AlreadySubscribed(_) =>
                    Redirect(errorRoutes.AlreadySignedUpController.show())
                  case Ineligible =>
                    Redirect(errorRoutes.CannotUseServiceController.show())
                  case Deregistered =>
                    Redirect(errorRoutes.DeregisteredVatNumberController.show())
                  case Inhibited(migratableDates) =>
                    Redirect(errorRoutes.MigratableDatesController.show())
                      .addingToSession(migratableDatesKey, migratableDates)
                  case MigrationInProgress =>
                    Redirect(errorRoutes.MigrationInProgressErrorController.show())
                  case InvalidVatNumber =>
                    Redirect(errorRoutes.CouldNotConfirmVatNumberController.show())
                  case RecentlyRegistered =>
                    Redirect(errorRoutes.RecentlyRegisteredVatNumberController.show())
                  case errorResponse =>
                    throw new InternalServerException(s"storeVatNumberOrchestration failed due to $errorResponse")
                }
              else Future.successful(
                Redirect(errorRoutes.CouldNotConfirmVatNumberController.show())
                  .removingFromSession(vatNumberKey)
              )
            case _ =>
              Future.successful(
                Redirect(routes.CaptureVatNumberController.show())
              )
          }
      }
  }

}
