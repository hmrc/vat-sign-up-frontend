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

package uk.gov.hmrc.vatsignupfrontend.controllers.agent

import javax.inject.{Inject, Singleton}
import play.api.mvc.{Action, AnyContent}
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.ControllerComponents
import uk.gov.hmrc.vatsignupfrontend.config.auth.AgentEnrolmentPredicate
import uk.gov.hmrc.vatsignupfrontend.controllers.AuthenticatedController
import uk.gov.hmrc.vatsignupfrontend.forms.EmailForm._
import uk.gov.hmrc.vatsignupfrontend.views.html.agent.capture_agent_email

import scala.concurrent.Future

@Singleton
class CaptureAgentEmailController @Inject()(val controllerComponents: ControllerComponents)
  extends AuthenticatedController(AgentEnrolmentPredicate) {

  val validateEmailForm = emailForm(isAgent = false)

  val show: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      request.session.get(SessionKeys.transactionEmailKey) match {
        case None => Future.successful(
          Ok(capture_agent_email(validateEmailForm.form, routes.CaptureAgentEmailController.submit()))
        )
        case Some(_) => Future.successful(
          Redirect(routes.ConfirmAgentEmailController.show())
        )
      }
    }
  }

  val change: Action[AnyContent] = Action.async { implicit request =>
    request.session - SessionKeys.transactionEmailKey
    Future.successful(
      Redirect(routes.CaptureAgentEmailController.show())
    )
  }

  val submit: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      validateEmailForm.bindFromRequest.fold(
        formWithErrors =>
          Future.successful(
            BadRequest(capture_agent_email(formWithErrors, routes.CaptureAgentEmailController.submit()))
          ),
        email =>
          Future.successful(Redirect(
            routes.ConfirmAgentEmailController.show()
          ).addingToSession(SessionKeys.transactionEmailKey -> email))
      )
    }
  }

}
