/*
 * Copyright 2018 HM Revenue & Customs
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
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.SessionKeys._
import uk.gov.hmrc.vatsignupfrontend.config.ControllerComponents
import uk.gov.hmrc.vatsignupfrontend.config.auth.AdministratorRolePredicate
import uk.gov.hmrc.vatsignupfrontend.controllers.AuthenticatedController
import uk.gov.hmrc.vatsignupfrontend.forms.VatNumberForm._
import uk.gov.hmrc.vatsignupfrontend.httpparsers.StoreVatNumberHttpParser.{AlreadySubscribed, IneligibleVatNumber, SubscriptionClaimed, VatNumberStored}
import uk.gov.hmrc.vatsignupfrontend.httpparsers.VatNumberEligibilityHttpParser._
import uk.gov.hmrc.vatsignupfrontend.models.MigratableDates
import uk.gov.hmrc.vatsignupfrontend.services.{StoreVatNumberService, VatNumberEligibilityService}
import uk.gov.hmrc.vatsignupfrontend.utils.EnrolmentUtils._
import uk.gov.hmrc.vatsignupfrontend.utils.{SessionUtils, VatNumberChecksumValidation}
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.capture_vat_number
import uk.gov.hmrc.vatsignupfrontend.utils.SessionUtils.ResultUtils

import scala.concurrent.Future

@Singleton
class CaptureVatNumberController @Inject()(val controllerComponents: ControllerComponents,
                                           vatNumberEligibilityService: VatNumberEligibilityService,
                                           storeVatNumberService: StoreVatNumberService)
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
            enrolments.vatNumber match {
              case Some(enrolmentVatNumber) if enrolmentVatNumber == formVatNumber =>
                storeVatNumberService.storeVatNumber(formVatNumber, isFromBta = Some(false)) map {
                  case Right(VatNumberStored) =>
                    Redirect(routes.CaptureBusinessEntityController.show())
                      .addingToSession(SessionKeys.vatNumberKey -> formVatNumber)
                  case Right(SubscriptionClaimed) =>
                    Redirect(routes.SignUpCompleteClientController.show())
                  case Left(AlreadySubscribed) => Redirect(routes.AlreadySignedUpController.show())
                  case Left(IneligibleVatNumber(MigratableDates(None, None))) => Redirect(routes.CannotUseServiceController.show())
                  case Left(IneligibleVatNumber(migratableDates)) => Redirect(routes.MigratableDatesController.show())
                    .addingToSession(SessionKeys.migratableDatesKey, migratableDates)
                  case Left(_) =>
                    throw new InternalServerException("storeVatNumber failed")
                }
              case Some(_) =>
                Future.successful(Redirect(routes.IncorrectEnrolmentVatNumberController.show()))
              case None =>
                vatNumberEligibilityService.checkVatNumberEligibility(formVatNumber) map {
                  case Right(VatNumberEligible) => Redirect(routes.CaptureVatRegistrationDateController.show()).addingToSession(vatNumberKey -> formVatNumber)
                  case Left(IneligibleForMtdVatNumber(MigratableDates(None, None))) => Redirect(routes.CannotUseServiceController.show())
                  case Left(IneligibleForMtdVatNumber(migratableDates)) => Redirect(routes.MigratableDatesController.show())
                    .addingToSession(SessionKeys.migratableDatesKey, migratableDates)
                  case Left(InvalidVatNumber) => Redirect(routes.InvalidVatNumberController.show())
                  case Left(VatNumberAlreadySubscribed) => Redirect(routes.AlreadySignedUpController.show())
                  case Left(VatNumberEligibilityFailureResponse(status)) => {
                    throw new InternalServerException(s"Failure retrieving eligibility of vat number: status=$status")
                  }
                }
            }
          } else
            Future.successful(Redirect(routes.InvalidVatNumberController.show()))
      )
    }
  }

}
