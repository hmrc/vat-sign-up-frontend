/*
 * Copyright 2020 HM Revenue & Customs
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
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.ControllerComponents
import uk.gov.hmrc.vatsignupfrontend.config.auth.AgentEnrolmentPredicate
import uk.gov.hmrc.vatsignupfrontend.controllers.AuthenticatedController
import uk.gov.hmrc.vatsignupfrontend.controllers.agent.error.{routes => errorRoutes}
import uk.gov.hmrc.vatsignupfrontend.forms.EmailForm._
import uk.gov.hmrc.vatsignupfrontend.forms.prevalidation.PrevalidationAPI
import uk.gov.hmrc.vatsignupfrontend.views.html.agent.capture_client_email

import scala.concurrent.Future

@Singleton
class CaptureClientEmailController @Inject()(val controllerComponents: ControllerComponents)
  extends AuthenticatedController(AgentEnrolmentPredicate) {

  val validateEmailForm: PrevalidationAPI[String] = emailForm(isAgent = true)

  val show: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      val hasDirectDebit = request.session.get(SessionKeys.hasDirectDebitKey).getOrElse("false").toBoolean
      val contactPreference = request.session.get(SessionKeys.contactPreferenceKey).filter(_.nonEmpty)

      Future.successful(
        Ok(capture_client_email(
          hasDirectDebit = hasDirectDebit,
          contactPreference,
          validateEmailForm.form,
          routes.CaptureClientEmailController.submit()
        ))
      )
    }
  }

  val submit: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      val hasDirectDebit = request.session.get(SessionKeys.hasDirectDebitKey).getOrElse("false").toBoolean
      val contactPreference = request.session.get(SessionKeys.contactPreferenceKey).filter(_.nonEmpty)

      validateEmailForm.bindFromRequest.fold(
        formWithErrors =>
          Future.successful(
            BadRequest(capture_client_email(
              hasDirectDebit = hasDirectDebit,
              contactPreference,
              formWithErrors,
              routes.CaptureClientEmailController.submit()
            ))
          ),
        clientEmail =>
          request.session.get(SessionKeys.transactionEmailKey) match {
            case Some(agentEmail) if agentEmail == clientEmail =>
              Future.successful(Redirect(errorRoutes.UseDifferentEmailAddressController.show()))
            case _ =>
              Future.successful(Redirect(routes.ConfirmClientEmailController.show())
                .addingToSession(SessionKeys.emailKey -> clientEmail))
          }
      )
    }
  }
}
