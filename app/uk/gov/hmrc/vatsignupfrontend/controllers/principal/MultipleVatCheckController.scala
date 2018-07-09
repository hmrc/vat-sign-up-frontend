/*
 * Copyright 2018 HM Revenue & Customs
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
import uk.gov.hmrc.vatsignupfrontend.config.ControllerComponents
import uk.gov.hmrc.vatsignupfrontend.config.auth.AdministratorRolePredicate
import uk.gov.hmrc.vatsignupfrontend.controllers.AuthenticatedController
import uk.gov.hmrc.vatsignupfrontend.forms.YesNoForm._
import uk.gov.hmrc.vatsignupfrontend.models.{No, Yes}
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.multiple_vat_check

import scala.concurrent.Future

@Singleton
class MultipleVatCheckController @Inject()(val controllerComponents: ControllerComponents)
  extends AuthenticatedController(AdministratorRolePredicate) {

  val show: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      Future.successful(Ok(multiple_vat_check(yesNoForm, routes.MultipleVatCheckController.submit())))
    }
  }

  val submit: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      yesNoForm.bindFromRequest.fold(
        formWithErrors =>
          Future.successful(
            BadRequest(multiple_vat_check(formWithErrors, routes.MultipleVatCheckController.submit()))
          ),
        yesNo => yesNo match {
          case Yes =>
            Future.successful(Redirect(routes.CaptureVatNumberController.show()))
          case No =>
            Future.successful(Redirect(routes.CaptureBusinessEntityController.show()))
        }
      )
    }
  }
}