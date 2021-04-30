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
import play.api.mvc.{Action, AnyContent, Result}
import uk.gov.hmrc.http.InternalServerException
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.VatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.config.auth.AgentEnrolmentPredicate
import uk.gov.hmrc.vatsignupfrontend.controllers.AuthenticatedController
import uk.gov.hmrc.vatsignupfrontend.forms.ContactPreferencesForm._
import uk.gov.hmrc.vatsignupfrontend.models.{ContactPreference, Digital, Paper}
import uk.gov.hmrc.vatsignupfrontend.services.StoreContactPreferenceService
import uk.gov.hmrc.vatsignupfrontend.utils.SessionUtils.ResultUtils
import uk.gov.hmrc.vatsignupfrontend.views.html.agent.receive_email_notifications

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class ContactPreferenceController @Inject()(contactPreferenceService: StoreContactPreferenceService,
                                            view: receive_email_notifications)
                                           (implicit ec: ExecutionContext,
                                            vcc: VatControllerComponents)
  extends AuthenticatedController(AgentEnrolmentPredicate) {

  val show: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      Future.successful(
        Ok(view(contactPreferencesForm(isAgent = true), routes.ContactPreferenceController.submit()))
      )
    }
  }

  val submit: Action[AnyContent] = Action.async { implicit request =>
    authorised() {

      val optVatNumber = request.session.get(SessionKeys.vatNumberKey).filter(_.nonEmpty)
      val hasDirectDebit = request.session.get(SessionKeys.hasDirectDebitKey).getOrElse("false").toBoolean

      def storeContactPreference(contactPreference: ContactPreference): Future[Result] = {
        optVatNumber match {
          case Some(vatNumber) =>
            contactPreferenceService.storeContactPreference(vatNumber, contactPreference) map {
              case Right(_) => redirect(contactPreference)
              case Left(status) => throw new InternalServerException(s"Store contact preference failed with status = $status")
            }
          case None =>
            Future.successful(Redirect(routes.CaptureVatNumberController.show()))
        }
      }

      def redirect(contactPreference: ContactPreference): Result =
        (contactPreference, hasDirectDebit) match {
          case (Digital, false) | (_, true) =>
            Redirect(routes.CaptureClientEmailController.show())
              .addingToSession(SessionKeys.contactPreferenceKey, contactPreference)
          case (Paper, false) =>
            Redirect(routes.CheckYourAnswersFinalController.show())
        }

      contactPreferencesForm(isAgent = true).bindFromRequest.fold(
        formWithErrors => Future.successful(
          BadRequest(view(formWithErrors, routes.ConfirmClientEmailController.submit()))
        ),
        storeContactPreference
      )
    }
  }
}
