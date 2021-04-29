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

import play.api.mvc.{Action, AnyContent}
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.VatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.config.auth.AgentEnrolmentPredicate
import uk.gov.hmrc.vatsignupfrontend.controllers.AuthenticatedController
import uk.gov.hmrc.vatsignupfrontend.forms.EmailForm._
import uk.gov.hmrc.vatsignupfrontend.views.html.agent.capture_agent_email

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class CaptureAgentEmailController @Inject()(view: capture_agent_email)(implicit ec: ExecutionContext,
                                            vcc: VatControllerComponents)
  extends AuthenticatedController(AgentEnrolmentPredicate) {

  val validateEmailForm = emailForm(isAgent = false)

  val show: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      val optIsMigrated = request.session.get(SessionKeys.isMigratedKey) map (_.toBoolean)
      val optTransactionEmail = request.session.get(SessionKeys.transactionEmailKey)

      (optIsMigrated, optTransactionEmail) match {
        case (Some(isMigrated), _) if isMigrated =>
          Future.successful(Redirect(routes.AgentSendYourApplicationController.show()))
        case (_, None) =>
          Future.successful(Ok(view(validateEmailForm.form, routes.CaptureAgentEmailController.submit())))
        case (_, Some(_)) =>
          Future.successful(Redirect(routes.ConfirmAgentEmailController.show()))
      }
    }
  }

  val change: Action[AnyContent] = Action.async { implicit request =>
    Future.successful(
      Redirect(routes.CaptureAgentEmailController.show())
        .removingFromSession(SessionKeys.transactionEmailKey)
    )
  }

  val submit: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      validateEmailForm.bindFromRequest.fold(
        formWithErrors =>
          Future.successful(
            BadRequest(view(formWithErrors, routes.CaptureAgentEmailController.submit()))
          ),
        email =>
          // UPDATE WHEN NEW ENTER PASSCODE PAGE IS UPDATED
          //          if (isEnabled(EmailVerification)) {
          //            val lang: String = request.lang.code
          //            emailVerificationService.requestEmailVerificationPasscode(email, lang).map {
          //              case RequestEmailPasscodeSuccessful => Redirect(enterPasscodePage)
          //              case AlreadyVerifiedEmailAddress => Redirect(routes.AgentVerifiedEmailController.show())
          //            }
          //          }
          Future.successful(Redirect(
            routes.ConfirmAgentEmailController.show()
          ).addingToSession(SessionKeys.transactionEmailKey -> email))
      )
    }
  }

}
