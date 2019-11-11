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
import uk.gov.hmrc.http.InternalServerException
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.ControllerComponents
import uk.gov.hmrc.vatsignupfrontend.config.auth.AgentEnrolmentPredicate
import uk.gov.hmrc.vatsignupfrontend.controllers.AuthenticatedController
import uk.gov.hmrc.vatsignupfrontend.httpparsers.SubmissionHttpParser.SubmissionFailureResponse
import uk.gov.hmrc.vatsignupfrontend.services.MigratedSubmissionService
import uk.gov.hmrc.vatsignupfrontend.views.html.agent.send_your_application

import scala.concurrent.Future

@Singleton
class AgentSendYourApplicationController @Inject()(val controllerComponents: ControllerComponents, val migratedSubmissionService: MigratedSubmissionService)
  extends AuthenticatedController(AgentEnrolmentPredicate){

  val show: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      Future.successful(Ok(send_your_application(routes.AgentSendYourApplicationController.submit())))
    }
  }

  val submit: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      request.session.get(SessionKeys.vatNumberKey) match {
        case Some(vatNumber) => migratedSubmissionService.submit(vatNumber) map {
          case  Right(_) => Redirect(resignup.routes.SignUpCompleteController.show())
          case  Left(SubmissionFailureResponse(status)) =>  throw new InternalServerException(s"Submission failed, backend returned: $status")
        }
        case None => Future.successful(Redirect(routes.CaptureVatNumberController.show()))
      }


    }
  }

}