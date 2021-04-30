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
import uk.gov.hmrc.vatsignupfrontend.forms.RegisteredSocietyUtrForm._
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.capture_registered_society_utr

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class CaptureRegisteredSocietyUtrController @Inject()(view: capture_registered_society_utr)(implicit ec: ExecutionContext,
                                                        vcc: VatControllerComponents)
  extends AuthenticatedController(AdministratorRolePredicate) {

  val show: Action[AnyContent] = Action.async {
    implicit request =>
      authorised() {
        Future.successful(
          Ok(view(registeredSocietyUtrForm.form, routes.CaptureRegisteredSocietyUtrController.submit()))
        )
      }
  }

  val submit: Action[AnyContent] = Action.async {
    implicit request =>
      authorised() {
        registeredSocietyUtrForm.bindFromRequest.fold(
          formWithErrors =>
            Future.successful(
              BadRequest(view(formWithErrors, routes.CaptureRegisteredSocietyUtrController.submit()))
            ),
          companyUtr =>
            Future.successful(
              Redirect(routes.RegisteredSocietyCheckYourAnswersController.show()).addingToSession(SessionKeys.registeredSocietyUtrKey -> companyUtr)
            )
        )
      }
  }

}

