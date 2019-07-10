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
import play.api.mvc.{Action, AnyContent}
import uk.gov.hmrc.http.InternalServerException
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.ControllerComponents
import uk.gov.hmrc.vatsignupfrontend.config.auth.AdministratorRolePredicate
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch.{ContactPreferencesJourney, FinalCheckYourAnswer}
import uk.gov.hmrc.vatsignupfrontend.controllers.AuthenticatedController
import uk.gov.hmrc.vatsignupfrontend.httpparsers.StoreEmailAddressHttpParser.StoreEmailAddressSuccess
import uk.gov.hmrc.vatsignupfrontend.services.StoreEmailAddressService
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.confirm_email

import scala.concurrent.Future

@Singleton
class ConfirmEmailController @Inject()(val controllerComponents: ControllerComponents,
                                       val storeEmailAddressService: StoreEmailAddressService)
  extends AuthenticatedController(AdministratorRolePredicate) {

  val show: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      val optVatNumber = request.session.get(SessionKeys.vatNumberKey).filter(_.nonEmpty)
      val optEmail = request.session.get(SessionKeys.emailKey).filter(_.nonEmpty)

      (optVatNumber, optEmail) match {
        case (Some(_), Some(email)) =>
          Future.successful(
            Ok(confirm_email(email, routes.ConfirmEmailController.submit()))
          )
        case (None, _) =>
          Future.successful(
            Redirect(routes.ResolveVatNumberController.resolve())
          )
        case _ =>
          Future.successful(
            Redirect(routes.CaptureEmailController.show())
          )
      }
    }
  }

  val submit: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      val optVatNumber = request.session.get(SessionKeys.vatNumberKey).filter(_.nonEmpty)
      val optEmail = request.session.get(SessionKeys.emailKey).filter(_.nonEmpty)

      (optVatNumber, optEmail) match {
        case (Some(vatNumber), Some(email)) => {
          if (isEnabled(ContactPreferencesJourney))
            storeEmailAddressService.storeTransactionEmailAddress(vatNumber, email)
          else
            storeEmailAddressService.storeEmailAddress(vatNumber, email)
        } map {
          case Right(StoreEmailAddressSuccess(false)) =>
            Redirect(routes.VerifyEmailController.show().url)
          case Right(StoreEmailAddressSuccess(true)) =>
            if(isEnabled(ContactPreferencesJourney)) Redirect(routes.ReceiveEmailNotificationsController.show())
            else if(isEnabled(FinalCheckYourAnswer)) Redirect(routes.CheckYourAnswersFinalController.show())
            else Redirect(routes.TermsController.show())
          case Left(errResponse) =>
            throw new InternalServerException("storeEmailAddress failed: status=" + errResponse.status)
        }
        case (None, _) =>
          Future.successful(
            Redirect(routes.ResolveVatNumberController.resolve())
          )
        case _ =>
          Future.successful(
            Redirect(routes.CaptureEmailController.show())
          )
      }
    }
  }

}
