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
import uk.gov.hmrc.vatsignupfrontend.forms.ContactPreferencesForm._
import uk.gov.hmrc.vatsignupfrontend.models.Digital
import uk.gov.hmrc.vatsignupfrontend.services.{StoreContactPreferenceService, StoreEmailAddressService}
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.receive_email_notifications

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class ReceiveEmailNotificationsController @Inject()(storeContactPreferenceService: StoreContactPreferenceService,
                                                    storeEmailAddressService: StoreEmailAddressService,
                                                    view: receive_email_notifications)
                                                   (implicit ec: ExecutionContext,
                                                    vcc: VatControllerComponents)
  extends AuthenticatedController(AdministratorRolePredicate) {

  val show: Action[AnyContent] = Action.async { implicit request =>
    authorised() {

      val optEmail = request.session.get(SessionKeys.emailKey).filter(_.nonEmpty)

      optEmail match {
        case Some(email) =>
          Future.successful(
            Ok(view(
              email,
              contactPreferencesForm(isAgent = false),
              routes.ReceiveEmailNotificationsController.submit()
            ))
          )
        case None =>
          Future.successful(
            Redirect(routes.CaptureEmailController.show())
          )
      }
    }
  }

  val submit: Action[AnyContent] = Action.async { implicit request =>
    authorised() {

      val optEmail = request.session.get(SessionKeys.emailKey).filter(_.nonEmpty)
      val optVatNumber = request.session.get(SessionKeys.vatNumberKey).filter(_.nonEmpty)
      val isDirectDebit = request.session.get(SessionKeys.hasDirectDebitKey).getOrElse("false").toBoolean

      contactPreferencesForm(isAgent = false).bindFromRequest.fold(
        formWithErrors => optEmail match {
          case Some(email) =>
            Future.successful(
              BadRequest(view(
                email,
                formWithErrors,
                routes.ReceiveEmailNotificationsController.submit()
              ))
            )
          case None =>
            Future.successful(
              Redirect(routes.CaptureEmailController.show())
            )
        }, {
          contactPreference =>
            optVatNumber match {
              case Some(vatNumber) =>
                storeContactPreferenceService.storeContactPreference(vatNumber, contactPreference) flatMap {
                  case Right(_) if contactPreference == Digital || isDirectDebit =>
                    optEmail match {
                      case Some(email) =>
                        storeEmailAddressService.storeEmailAddress(vatNumber, email) map {
                          case Right(_) =>
                            Redirect(routes.CheckYourAnswersFinalController.show())
                          case Left(status) =>
                            throw new InternalServerException(s"Store email address service failed with status= $status")
                        }
                      case None =>
                        Future.successful(
                          Redirect(routes.CaptureEmailController.show())
                        )
                    }
                  case Right(_) =>
                    Future.successful(
                      Redirect(routes.CheckYourAnswersFinalController.show())
                    )
                  case Left(status) => throw new InternalServerException(s"Store contact preference failed with status = $status")
                }
              case None =>
                Future.successful(
                  Redirect(routes.ResolveVatNumberController.resolve())
                )
            }
        }
      )
    }
  }
}
