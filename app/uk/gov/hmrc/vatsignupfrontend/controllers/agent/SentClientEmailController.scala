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
import uk.gov.hmrc.vatsignupfrontend.config.VatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.config.auth.AgentEnrolmentPredicate
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch.FinalCheckYourAnswer
import uk.gov.hmrc.vatsignupfrontend.controllers.AuthenticatedController
import uk.gov.hmrc.vatsignupfrontend.views.html.agent.sent_client_email

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class SentClientEmailController @Inject()(implicit ec: ExecutionContext,
                                            vcc: VatControllerComponents)
  extends AuthenticatedController(AgentEnrolmentPredicate) {

  val show: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      request.session.get(SessionKeys.emailKey) match {
        case Some(email) if email.nonEmpty =>
          Future.successful(
            Ok(sent_client_email(email, routes.SentClientEmailController.submit()))
          )
        case _ =>
          Future.successful(
            Redirect(routes.CaptureClientEmailController.show())
          )
      }
    }
  }

  val submit: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      request.session.get(SessionKeys.emailKey) match {
        case Some(email) if email.nonEmpty & isEnabled(FinalCheckYourAnswer) =>
          Future.successful(
            Redirect(routes.CheckYourAnswersFinalController.show())
          )
        case Some(email) if email.nonEmpty =>
          Future.successful(
            Redirect(routes.AgentSendYourApplicationController.show())
          )
        case _ =>
          Future.successful(
            Redirect(routes.CaptureClientEmailController.show())
          )
      }
    }
  }

}
