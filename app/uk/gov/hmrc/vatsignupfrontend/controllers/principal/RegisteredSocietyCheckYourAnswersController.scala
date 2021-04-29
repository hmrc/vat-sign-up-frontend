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
import uk.gov.hmrc.http.InternalServerException
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.VatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.config.auth.AdministratorRolePredicate
import uk.gov.hmrc.vatsignupfrontend.controllers.AuthenticatedController
import uk.gov.hmrc.vatsignupfrontend.controllers.principal.error.{routes => errorRoutes}
import uk.gov.hmrc.vatsignupfrontend.httpparsers.StoreRegisteredSocietyHttpParser.{CtReferenceMismatch, StoreRegisteredSocietySuccess}
import uk.gov.hmrc.vatsignupfrontend.models._
import uk.gov.hmrc.vatsignupfrontend.services.StoreRegisteredSocietyService
import uk.gov.hmrc.vatsignupfrontend.utils.SessionUtils._
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.check_your_answers_registered_society

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class RegisteredSocietyCheckYourAnswersController @Inject()(val storeRegisteredSocietyService: StoreRegisteredSocietyService,
                                                            view: check_your_answers_registered_society)
                                                           (implicit ec: ExecutionContext,
                                                            vcc: VatControllerComponents)
  extends AuthenticatedController(AdministratorRolePredicate) {

  def show: Action[AnyContent] = Action.async { implicit request =>
    authorised() {

      val optBusinessEntity = request.session.getModel[BusinessEntity](SessionKeys.businessEntityKey)
      val optCompanyNumber = request.session.get(SessionKeys.registeredSocietyCompanyNumberKey).filter(_.nonEmpty)
      val optCompanyUtr = request.session.get(SessionKeys.registeredSocietyUtrKey).filter(_.nonEmpty)

      (optBusinessEntity, optCompanyNumber, optCompanyUtr) match {
        case (Some(entity), Some(companyNumber), Some(companyUtr)) =>
          Future.successful(
            Ok(view(
              companyNumber = companyNumber,
              ctReference = companyUtr,
              entityType = entity,
              routes.RegisteredSocietyCheckYourAnswersController.submit()
            ))
          )
        case (None, _, _) =>
          Future.successful(
            Redirect(routes.CaptureBusinessEntityController.show())
          )
        case (_, None, _) =>
          Future.successful(
            Redirect(routes.CaptureRegisteredSocietyCompanyNumberController.show())
          )
        case (_, _, None) =>
          Future.successful(
            Redirect(routes.CaptureRegisteredSocietyUtrController.show())
          )
      }
    }
  }

  def submit: Action[AnyContent] = Action.async { implicit request =>
    authorised() {

      val optVatNumber = request.session.get(SessionKeys.vatNumberKey).filter(_.nonEmpty)
      val optBusinessEntity = request.session.getModel[BusinessEntity](SessionKeys.businessEntityKey)
      val optCompanyNumber = request.session.get(SessionKeys.registeredSocietyCompanyNumberKey).filter(_.nonEmpty)
      val optCompanyUtr = request.session.get(SessionKeys.registeredSocietyUtrKey).filter(_.nonEmpty)

      (optVatNumber, optBusinessEntity, optCompanyNumber, optCompanyUtr) match {
        case (Some(vatNumber), Some(RegisteredSociety), Some(companyNumber), Some(companyUtr)) =>
          storeRegisteredSocietyService.storeRegisteredSociety(vatNumber, companyNumber, Some(companyUtr)) map {
            case Right(StoreRegisteredSocietySuccess) =>
              Redirect(routes.DirectDebitResolverController.show())
            case Left(CtReferenceMismatch) =>
              Redirect(errorRoutes.CouldNotConfirmBusinessController.show())
            case Left(failure) =>
              throw new InternalServerException("unexpected response on store company number " + failure.status)
          }
        case (None, _, _, _) =>
          Future.successful(
            Redirect(routes.CaptureVatNumberController.show())
          )
        case (_, None, _, _) =>
          Future.successful(
            Redirect(routes.CaptureBusinessEntityController.show())
          )
        case (_, _, None, _) =>
          Future.successful(
            Redirect(routes.CaptureRegisteredSocietyCompanyNumberController.show())
          )
        case (_, _, _, None) =>
          Future.successful(
            Redirect(routes.CaptureRegisteredSocietyUtrController.show())
          )
      }
    }
  }
}
