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
import uk.gov.hmrc.http.InternalServerException
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.SessionKeys._
import uk.gov.hmrc.vatsignupfrontend.config.ControllerComponents
import uk.gov.hmrc.vatsignupfrontend.config.auth.AdministratorRolePredicate
import uk.gov.hmrc.vatsignupfrontend.controllers.AuthenticatedController
import uk.gov.hmrc.vatsignupfrontend.forms.VatNumberForm._
import uk.gov.hmrc.vatsignupfrontend.httpparsers.VatNumberEligibilityHttpParser
import uk.gov.hmrc.vatsignupfrontend.httpparsers.VatNumberEligibilityHttpParser._
import uk.gov.hmrc.vatsignupfrontend.models.{MigratableDates, Overseas}
import uk.gov.hmrc.vatsignupfrontend.services.StoreVatNumberService._
import uk.gov.hmrc.vatsignupfrontend.services.{AdministrativeDivisionLookupService, StoreVatNumberService, VatNumberEligibilityService}
import uk.gov.hmrc.vatsignupfrontend.utils.EnrolmentUtils._
import uk.gov.hmrc.vatsignupfrontend.utils.SessionUtils.ResultUtils
import uk.gov.hmrc.vatsignupfrontend.utils.VatNumberChecksumValidation
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.capture_vat_number

import scala.concurrent.Future

@Singleton
class CaptureVatNumberController @Inject()(val controllerComponents: ControllerComponents,
                                           vatNumberEligibilityService: VatNumberEligibilityService,
                                           storeVatNumberService: StoreVatNumberService,
                                           administrativeDivisionLookupService: AdministrativeDivisionLookupService)
  extends AuthenticatedController(AdministratorRolePredicate) {

  private val validateVatNumberForm = vatNumberForm(isAgent = false)

  def show: Action[AnyContent] = Action.async { implicit request =>
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
          )
        , formVatNumber =>
          if (VatNumberChecksumValidation.isValidChecksum(formVatNumber)) {
            enrolments.mtdVatNumber match {
              case Some(mtdVatNumber) =>
                if (mtdVatNumber == formVatNumber)
                  Future.successful(Redirect(routes.AlreadySignedUpController.show()))
                else
                  Future.successful(Redirect(routes.CannotSignUpAnotherAccountController.show()))
              case None =>
                enrolments.vatNumber match {
                  case Some(enrolmentVatNumber) =>
                    if (enrolmentVatNumber == formVatNumber)
                      storeVatNumber(formVatNumber)
                    else
                      Future.successful(Redirect(routes.IncorrectEnrolmentVatNumberController.show()))
                  case _ =>
                    checkVrnEligibility(formVatNumber) map {
                      _.removingFromSession(
                        vatRegistrationDateKey,
                        businessPostCodeKey,
                        previousVatReturnKey,
                        lastReturnMonthPeriodKey,
                        box5FigureKey
                      )
                    }

                }
            }
          } else Future.successful(Redirect(routes.InvalidVatNumberController.show()))
      )
    }
  }

  private def checkVrnEligibility(formVatNumber: String)(implicit request: Request[AnyContent]): Future[Result] = {
    vatNumberEligibilityService.checkVatNumberEligibility(formVatNumber) map {
      case Right(success) if success.isOverseas =>
        Redirect(routes.CaptureVatRegistrationDateController.show())
          .addingToSession(vatNumberKey -> formVatNumber)
          .addingToSession(businessEntityKey -> Overseas.toString)
      case Right(_) =>
        Redirect(routes.CaptureVatRegistrationDateController.show())
          .addingToSession(vatNumberKey -> formVatNumber)
          .removingFromSession(businessEntityKey)
      case Left(IneligibleForMtdVatNumber(MigratableDates(None, None))) =>
        Redirect(routes.CannotUseServiceController.show())
          .removingFromSession(businessEntityKey)
      case Left(IneligibleForMtdVatNumber(migratableDates)) =>
        Redirect(routes.MigratableDatesController.show())
          .addingToSession(SessionKeys.migratableDatesKey, migratableDates)
          .removingFromSession(businessEntityKey)
      case Left(VatNumberEligibilityHttpParser.InvalidVatNumber) =>
        Redirect(routes.InvalidVatNumberController.show())
          .removingFromSession(businessEntityKey)
      case Left(VatNumberEligibilityFailureResponse(status)) =>
        throw new InternalServerException(s"Failure retrieving eligibility of vat number: status=$status")
    }
  }

  private def checkVatDivision(formVatNumber: String,
                               isDirectDebit: Boolean)(implicit request: Request[AnyContent]): Result = {
    if(administrativeDivisionLookupService.isAdministrativeDivision(formVatNumber)) {
      Redirect(routes.DivisionResolverController.resolve())
        .addingToSession(SessionKeys.vatNumberKey -> formVatNumber)
        .addingToSession(SessionKeys.hasDirectDebitKey, isDirectDebit)
    } else {
      Redirect(routes.CaptureBusinessEntityController.show())
        .addingToSession(SessionKeys.vatNumberKey -> formVatNumber)
        .addingToSession(SessionKeys.hasDirectDebitKey, isDirectDebit)
    }
  }

  private def storeVatNumber(formVatNumber: String)(implicit request: Request[AnyContent]): Future[Result] = {
    storeVatNumberService.storeVatNumber(formVatNumber, isFromBta = false) map {
      case Right(VatNumberStored(isOverseas, isDirectDebit)) if isOverseas =>
        Redirect(routes.OverseasResolverController.resolve())
          .addingToSession(vatNumberKey -> formVatNumber)
          .addingToSession(SessionKeys.hasDirectDebitKey, isDirectDebit)
      case Right(VatNumberStored(_, isDirectDebit)) =>
          checkVatDivision(formVatNumber, isDirectDebit)
      case Right(SubscriptionClaimed) =>
        Redirect(routes.SignUpCompleteClientController.show())
      case Left(IneligibleVatNumber(MigratableDates(None, None))) =>
        Redirect(routes.CannotUseServiceController.show())
      case Left(IneligibleVatNumber(migratableDates)) =>
        Redirect(routes.MigratableDatesController.show())
          .addingToSession(SessionKeys.migratableDatesKey, migratableDates)
      case Left(VatMigrationInProgress) =>
        Redirect(routes.MigrationInProgressErrorController.show())
      case Left(VatNumberAlreadyEnrolled) =>
        Redirect(bta.routes.BusinessAlreadySignedUpController.show())
      case Left(_) =>
        throw new InternalServerException("storeVatNumber failed")
    }
  }
}
