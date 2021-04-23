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

import javax.inject.{Inject, Singleton}
import play.api.mvc.{Action, AnyContent}
import uk.gov.hmrc.auth.core.retrieve.Retrievals
import uk.gov.hmrc.http.InternalServerException
import uk.gov.hmrc.vatsignupfrontend.SessionKeys._
import uk.gov.hmrc.vatsignupfrontend.config.VatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.config.auth.AdministratorRolePredicate
import uk.gov.hmrc.vatsignupfrontend.controllers.AuthenticatedController
import uk.gov.hmrc.vatsignupfrontend.controllers.principal.error.{routes => errorRoutes}
import uk.gov.hmrc.vatsignupfrontend.forms.MultipleVatCheckForm._
import uk.gov.hmrc.vatsignupfrontend.models.{No, Overseas, Yes}
import uk.gov.hmrc.vatsignupfrontend.services.StoreVatNumberOrchestrationService
import uk.gov.hmrc.vatsignupfrontend.services.StoreVatNumberOrchestrationService._
import uk.gov.hmrc.vatsignupfrontend.utils.EnrolmentUtils._
import uk.gov.hmrc.vatsignupfrontend.utils.SessionUtils.ResultUtils
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.multiple_vat_check

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class MultipleVatCheckController @Inject()(storeVatNumberOrchestrationService: StoreVatNumberOrchestrationService)
                                          (implicit ec: ExecutionContext,
                                           vcc: VatControllerComponents)
  extends AuthenticatedController(AdministratorRolePredicate) {

  val show: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      Future.successful(Ok(multiple_vat_check(multipleVatCheckForm, routes.MultipleVatCheckController.submit())))
    }
  }

  val submit: Action[AnyContent] = Action.async { implicit request =>
    authorised()(Retrievals.allEnrolments) { enrolments =>

      multipleVatCheckForm.bindFromRequest.fold(
        formWithErrors =>
          Future.successful(
            BadRequest(multiple_vat_check(formWithErrors, routes.MultipleVatCheckController.submit()))
          ), {
          case Yes =>
            Future.successful(Redirect(routes.CaptureVatNumberController.show()))
          case No =>
            enrolments.getAnyVatNumber match {
              case Some(vatNumber) =>
                storeVatNumberOrchestrationService.orchestrate(enrolments, vatNumber).map {
                  case VatNumberStored(isOverseas, isDirectDebit, isMigrated) =>
                    Redirect(routes.CaptureBusinessEntityController.show())
                      .addingToSession(vatNumberKey -> vatNumber)
                      .addingToSession(isMigratedKey, isMigrated)
                      .addingToSession(hasDirectDebitKey, isDirectDebit)
                      .conditionallyAddingToSession(businessEntityKey, Overseas.toString, isOverseas)
                  case SubscriptionClaimed =>
                    Redirect(routes.SignUpCompleteClientController.show())
                  case Ineligible =>
                    Redirect(errorRoutes.CannotUseServiceController.show())
                  case Deregistered =>
                    Redirect(errorRoutes.DeregisteredVatNumberController.show())
                  case Inhibited(migratableDates) =>
                    Redirect(errorRoutes.MigratableDatesController.show())
                      .addingToSession(migratableDatesKey, migratableDates)
                  case MigrationInProgress =>
                    Redirect(errorRoutes.MigrationInProgressErrorController.show())
                  case AlreadyEnrolledOnDifferentCredential =>
                    Redirect(errorRoutes.BusinessAlreadySignedUpController.show())
                  case AlreadySubscribed(_) =>
                    Redirect(errorRoutes.AlreadySignedUpController.show())
                  case RecentlyRegistered =>
                    Redirect(errorRoutes.RecentlyRegisteredVatNumberController.show())
                  case _ =>
                    throw new InternalServerException("Unexpected response from vat number orchestration service")
                }
              case None =>
                Future.successful(Redirect(routes.ResolveVatNumberController.resolve()))
            }
        }
      )
    }
  }
}
