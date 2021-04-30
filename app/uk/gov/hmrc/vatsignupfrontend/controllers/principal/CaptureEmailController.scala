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
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.VatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.config.auth.AdministratorRolePredicate
import uk.gov.hmrc.vatsignupfrontend.controllers.AuthenticatedController
import uk.gov.hmrc.vatsignupfrontend.forms.EmailForm._
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.capture_email

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class CaptureEmailController @Inject()(view: capture_email)
                                      (implicit ec: ExecutionContext,
                                       vcc: VatControllerComponents)
  extends AuthenticatedController(AdministratorRolePredicate) {

  val validateEmailForm = emailForm(isAgent = false)

  val show: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      val hasDirectDebit = request.session.get(SessionKeys.hasDirectDebitKey).getOrElse("false").toBoolean

      Future.successful(
        Ok(view(
          hasDirectDebit = hasDirectDebit,
          validateEmailForm.form,
          routes.CaptureEmailController.submit())
        )
      )
    }
  }

  val submit: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      val hasDirectDebit = request.session.get(SessionKeys.hasDirectDebitKey).getOrElse("false").toBoolean

      validateEmailForm.bindFromRequest.fold(
        formWithErrors =>
          Future.successful(
            BadRequest(view(
              hasDirectDebit = hasDirectDebit,
              formWithErrors,
              routes.CaptureEmailController.submit())
            ))
        ,
        email =>
          Future.successful(Redirect(routes.ConfirmEmailController.show()).addingToSession(SessionKeys.emailKey -> email))
      )
    }
  }
}
