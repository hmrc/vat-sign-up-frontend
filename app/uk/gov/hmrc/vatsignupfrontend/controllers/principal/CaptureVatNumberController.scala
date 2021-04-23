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
import uk.gov.hmrc.vatsignupfrontend.forms.VatNumberForm.vatNumberForm
import uk.gov.hmrc.vatsignupfrontend.models.Overseas
import uk.gov.hmrc.vatsignupfrontend.services.StoreVatNumberOrchestrationService
import uk.gov.hmrc.vatsignupfrontend.services.StoreVatNumberOrchestrationService._
import uk.gov.hmrc.vatsignupfrontend.utils.EnrolmentUtils._
import uk.gov.hmrc.vatsignupfrontend.utils.SessionUtils._
import uk.gov.hmrc.vatsignupfrontend.utils.VatNumberChecksumValidation
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.capture_vat_number

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class CaptureVatNumberController @Inject()(storeVatNumberOrchestrationService: StoreVatNumberOrchestrationService)
                                          (implicit ec: ExecutionContext,
                                           vcc: VatControllerComponents)
  extends AuthenticatedController(AdministratorRolePredicate) {

  private val validateVatNumberForm = vatNumberForm(isAgent = false)

  def show: Action[AnyContent] = Action.async {
    implicit request =>
      authorised() {
        Future.successful(Ok(capture_vat_number(validateVatNumberForm.form, routes.CaptureVatNumberController.submit())))
      }
  }

  // scalastyle:off
  def submit: Action[AnyContent] = Action.async {
    implicit request =>
      authorised()(Retrievals.allEnrolments) {
        enrolments =>
          validateVatNumberForm.bindFromRequest.fold(
            formWithErrors =>
              Future.successful(
                BadRequest(capture_vat_number(formWithErrors, routes.CaptureVatNumberController.submit()))
              ),
            formVatNumber => {
              if (VatNumberChecksumValidation.isValidChecksum(formVatNumber)) {
                enrolments.getAnyVatNumber match {
                  case Some(vrn) if vrn != formVatNumber =>
                    Future.successful(Redirect(errorRoutes.IncorrectEnrolmentVatNumberController.show()))
                  case _ =>
                    storeVatNumberOrchestrationService.orchestrate(enrolments, formVatNumber).map {
                      case Eligible(isOverseas, isMigrated, _) =>
                        Redirect(routes.CaptureVatRegistrationDateController.show())
                          .addingToSession(vatNumberKey -> formVatNumber)
                          .conditionallyAddingToSession(businessEntityKey, Overseas.toString, isOverseas)
                          .removingFromSession(isAlreadySubscribedKey)
                          .addingToSession(isMigratedKey, isMigrated)
                      case VatNumberStored(isOverseas, isDirectDebit, isMigrated) =>
                        Redirect(routes.CaptureBusinessEntityController.show())
                          .addingToSession(vatNumberKey -> formVatNumber)
                          .conditionallyAddingToSession(businessEntityKey, Overseas.toString, isOverseas)
                          .addingToSession(hasDirectDebitKey, isDirectDebit)
                          .removingFromSession(isAlreadySubscribedKey)
                          .addingToSession(isMigratedKey, isMigrated)
                      case AlreadySubscribed(isOverseas) if enrolments.getAnyVatNumber.isEmpty =>
                        Redirect(routes.CaptureVatRegistrationDateController.show())
                          .addingToSession(vatNumberKey -> formVatNumber)
                          .conditionallyAddingToSession(businessEntityKey, Overseas.toString, isOverseas)
                          .addingToSession(isAlreadySubscribedKey, true)
                      case AlreadySubscribed(_) =>
                        Redirect(errorRoutes.AlreadySignedUpController.show())
                      case SubscriptionClaimed =>
                        Redirect(routes.SignUpCompleteClientController.show())
                      case Ineligible =>
                        Redirect(errorRoutes.CannotUseServiceController.show())
                          .removingFromSession(businessEntityKey)
                      case Deregistered =>
                        Redirect(errorRoutes.DeregisteredVatNumberController.show())
                          .removingFromSession(businessEntityKey)
                      case Inhibited(migratablDates) =>
                        Redirect(errorRoutes.MigratableDatesController.show())
                          .addingToSession(migratableDatesKey, migratablDates)
                          .removingFromSession(businessEntityKey)
                      case MigrationInProgress =>
                        Redirect(errorRoutes.MigrationInProgressErrorController.show())
                      case AlreadyEnrolledOnDifferentCredential =>
                        Redirect(errorRoutes.BusinessAlreadySignedUpController.show())
                      case InvalidVatNumber =>
                        Redirect(errorRoutes.InvalidVatNumberController.show())
                          .removingFromSession(businessEntityKey)
                      case RecentlyRegistered =>
                        Redirect(errorRoutes.RecentlyRegisteredVatNumberController.show())
                      case errorResponse =>
                        throw new InternalServerException(s"storeVatNumberOrchestration failed due to $errorResponse")
                    }.map {
                      _.removingFromSession(
                        vatRegistrationDateKey,
                        businessPostCodeKey,
                        previousVatReturnKey,
                        lastReturnMonthPeriodKey,
                        box5FigureKey
                      )
                    }
                }
              } else {
                Future.successful(Redirect(errorRoutes.InvalidVatNumberController.show()))
              }
            }
          )
      }
  }
}
