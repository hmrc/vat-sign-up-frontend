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
import uk.gov.hmrc.vatsignupfrontend.forms.PreviousVatReturnForm._
import uk.gov.hmrc.vatsignupfrontend.models.{No, Yes}
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.previous_vat_return

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class PreviousVatReturnController @Inject()(view: previous_vat_return)
                                           (implicit ec: ExecutionContext,
                                            vcc: VatControllerComponents)

  extends AuthenticatedController(AdministratorRolePredicate) {

  val show: Action[AnyContent] = Action.async {
    implicit request =>
      authorised() {
        Future.successful(
          Ok(view(previousVatReturnForm, routes.PreviousVatReturnController.submit()))
        )
      }
  }

  val submit: Action[AnyContent] = Action.async {
    implicit request =>
      authorised() {
        previousVatReturnForm.bindFromRequest.fold(
          formWithErrors =>
            Future.successful(
              BadRequest(view(formWithErrors, routes.PreviousVatReturnController.submit()))
            ), {
            case Yes =>
              Future.successful(
                Redirect(routes.CaptureBox5FigureController.show())
                  .addingToSession(SessionKeys.previousVatReturnKey -> Yes.stringValue)
              )
            case No =>
              Future.successful(
                Redirect(routes.CheckYourAnswersController.show())
                  .addingToSession(SessionKeys.previousVatReturnKey -> No.stringValue)
                  .removingFromSession(
                    SessionKeys.lastReturnMonthPeriodKey,
                    SessionKeys.box5FigureKey
                  )
              )
          }
        )
      }
  }

}
