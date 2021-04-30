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
import uk.gov.hmrc.http.InternalServerException
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.VatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.config.auth.AdministratorRolePredicate
import uk.gov.hmrc.vatsignupfrontend.controllers.AuthenticatedController
import uk.gov.hmrc.vatsignupfrontend.forms.VatRegistrationDateForm._
import uk.gov.hmrc.vatsignupfrontend.models.Overseas
import uk.gov.hmrc.vatsignupfrontend.utils.SessionUtils._
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.vat_registration_date

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class CaptureVatRegistrationDateController @Inject()(view: vat_registration_date)
                                                    (implicit ec: ExecutionContext,
                                                     vcc: VatControllerComponents)
  extends AuthenticatedController(AdministratorRolePredicate) {

  def show: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      Future.successful(
        Ok(view(vatRegistrationDateForm, routes.CaptureVatRegistrationDateController.submit()))
      )
    }
  }

  def submit: Action[AnyContent] = Action.async { implicit request =>

    val optBusinessEntity = request.session.get(SessionKeys.businessEntityKey).filter(_.nonEmpty)
    val alreadySubscribed = request.session.get(SessionKeys.isAlreadySubscribedKey).fold(false)(_.toBoolean)
    val isMigrated = request.session.get(SessionKeys.isMigratedKey).fold(false)(_.toBoolean)

    authorised() {
      vatRegistrationDateForm.bindFromRequest.fold(
        formWithErrors =>
          Future.successful(
            BadRequest(view(formWithErrors, routes.CaptureVatRegistrationDateController.submit()))
          ),
        vatRegistrationDate => {
          optBusinessEntity match {
            case Some(entity) if entity == Overseas.toString =>
              if (isMigrated || alreadySubscribed)
                Future.successful(Redirect(routes.CheckYourAnswersController.show().url))
              else
                Future.successful(Redirect(routes.PreviousVatReturnController.show().url))
            case _ =>
              Future.successful(Redirect(routes.BusinessPostCodeController.show().url))
          }
        } map (_.addingToSession(SessionKeys.vatRegistrationDateKey, vatRegistrationDate))
      )

    }

  }

}
