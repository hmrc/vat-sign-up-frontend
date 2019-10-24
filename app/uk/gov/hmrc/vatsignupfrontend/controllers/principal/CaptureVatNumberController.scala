/*
 * Copyright 2019 HM Revenue & Customs
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
import play.api.mvc.{Action, AnyContent, Request, Result}
import uk.gov.hmrc.auth.core.retrieve.Retrievals
import uk.gov.hmrc.http.{HeaderCarrier, InternalServerException}
import uk.gov.hmrc.vatsignupfrontend.SessionKeys._
import uk.gov.hmrc.vatsignupfrontend.config.ControllerComponents
import uk.gov.hmrc.vatsignupfrontend.config.auth.AdministratorRolePredicate
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch.ReSignUpJourney
import uk.gov.hmrc.vatsignupfrontend.controllers.AuthenticatedController
import uk.gov.hmrc.vatsignupfrontend.forms.VatNumberForm._
import uk.gov.hmrc.vatsignupfrontend.models.Overseas
import uk.gov.hmrc.vatsignupfrontend.services.VatNumberOrchestrationService
import uk.gov.hmrc.vatsignupfrontend.services.VatNumberOrchestrationService._
import uk.gov.hmrc.vatsignupfrontend.utils.EnrolmentUtils._
import uk.gov.hmrc.vatsignupfrontend.utils.SessionUtils._
import uk.gov.hmrc.vatsignupfrontend.utils.VatNumberChecksumValidation
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.capture_vat_number

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class CaptureVatNumberController @Inject()(val controllerComponents: ControllerComponents,
                                           vatNumberOrchestrationService: VatNumberOrchestrationService
                                          )(implicit ec: ExecutionContext) extends AuthenticatedController(AdministratorRolePredicate) {

  private val validateVatNumberForm = vatNumberForm(isAgent = false)

  def show: Action[AnyContent] = Action.async {
    implicit request =>
      authorised() {
        Future.successful(Ok(capture_vat_number(validateVatNumberForm.form, routes.CaptureVatNumberController.submit())))
      }
  }

  def submit: Action[AnyContent] = Action.async { implicit request =>
    authorised()(Retrievals.allEnrolments) { enrolments =>
      validateVatNumberForm.bindFromRequest.fold(
        formWithErrors =>
          Future.successful(
            BadRequest(capture_vat_number(formWithErrors, routes.CaptureVatNumberController.submit()))
          ),
        formVatNumber => {
          val isValidFormVatNumber = VatNumberChecksumValidation.isValidChecksum(formVatNumber)

          if (isValidFormVatNumber && isEnabled(ReSignUpJourney))
            enrolments.getAnyVatNumber match {
              case Some(vrn) if vrn == formVatNumber =>
                storeVatNumber(formVatNumber)
              case Some(_) =>
                Future.successful(Redirect(routes.IncorrectEnrolmentVatNumberController.show()))
              case None =>
                checkVatNumberEligibility(formVatNumber)
            }
          else if (isValidFormVatNumber)
            (enrolments.mtdVatNumber, enrolments.vatNumber) match {
              case (Some(mtdVrn), _) if mtdVrn == formVatNumber =>
                Future.successful(Redirect(routes.AlreadySignedUpController.show()))
              case (Some(_), _) =>
                Future.successful(Redirect(routes.CannotSignUpAnotherAccountController.show()))
              case (_, Some(legacyVrn)) if legacyVrn == formVatNumber =>
                storeVatNumber(legacyVrn)
              case (_, Some(_)) =>
                Future.successful(Redirect(routes.IncorrectEnrolmentVatNumberController.show()))
              case (None, None) =>
                checkVatNumberEligibility(formVatNumber)
            }
          else
            Future.successful(Redirect(routes.InvalidVatNumberController.show()))
        }
      )
    }
  }

  private def checkVatNumberEligibility(vatNumber: String)(implicit hc: HeaderCarrier, request: Request[_]): Future[Result] = {
    vatNumberOrchestrationService.checkVatNumberEligibility(vatNumber).map {
      case Eligible(isOverseas, isMigrated) if isMigrated =>
        Redirect(routes.CaptureVatRegistrationDateController.show())
          .addingToSession(vatNumberKey -> vatNumber)
          .addingToSession(isMigratedKey, isMigrated)
      case Eligible(isOverseas, isMigrated) if isOverseas =>
        Redirect(routes.CaptureVatRegistrationDateController.show())
          .addingToSession(vatNumberKey -> vatNumber)
          .addingToSession(businessEntityKey -> Overseas.toString)
      case Eligible(_, _) =>
        Redirect(routes.CaptureVatRegistrationDateController.show())
          .addingToSession(vatNumberKey -> vatNumber)
          .removingFromSession(businessEntityKey)
      case Ineligible =>
        Redirect(routes.CannotUseServiceController.show())
          .removingFromSession(businessEntityKey)
      case Inhibited(migratablDates) =>
        Redirect(routes.MigratableDatesController.show())
          .addingToSession(migratableDatesKey, migratablDates)
          .removingFromSession(businessEntityKey)
      case MigrationInProgress =>
        throw new InternalServerException("Migration In Progress should not be returned when the feature switch is enabled")
      case AlreadySubscribed =>
        throw new InternalServerException("Already Subscribed should not be returned when the feature switch is enabled")
      case InvalidVatNumber =>
        Redirect(routes.InvalidVatNumberController.show())
          .removingFromSession(businessEntityKey)
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

  private def storeVatNumber(vatNumber: String)(implicit request: Request[_]): Future[Result] = {
    vatNumberOrchestrationService.storeVatNumber(vatNumber, isFromBta = false).flatMap {
      case MigratedVatNumberStored =>
        Future.successful(
          Redirect(routes.CaptureBusinessEntityController.show())
            .addingToSession(vatNumberKey -> vatNumber)
            .addingToSession(isMigratedKey, true)
        )
      case NonMigratedVatNumberStored(isOverseas, hasDirectDebit) if isOverseas =>
        Future.successful(
          Redirect(routes.OverseasResolverController.resolve())
            .addingToSession(vatNumberKey -> vatNumber)
            .addingToSession(hasDirectDebitKey, hasDirectDebit)
        )
      case NonMigratedVatNumberStored(_, hasDirectDebit) =>
        Future.successful(
          Redirect(routes.CaptureBusinessEntityController.show())
            .addingToSession(vatNumberKey -> vatNumber)
            .addingToSession(hasDirectDebitKey, hasDirectDebit)
        )
      case ClaimedSubscription =>
        Future.successful(Redirect(routes.SignUpCompleteClientController.show()))
      case Ineligible =>
        Future.successful(Redirect(routes.CannotUseServiceController.show()))
      case Inhibited(migratableDates) =>
        Future.successful(
          Redirect(routes.MigratableDatesController.show())
            .addingToSession(migratableDatesKey, migratableDates)
        )
      case MigrationInProgress =>
        Future.successful(Redirect(routes.MigrationInProgressErrorController.show()))
      case AlreadyEnrolledOnDifferentCredential =>
        Future.successful(Redirect(bta.routes.BusinessAlreadySignedUpController.show()))
    }
  }
}
