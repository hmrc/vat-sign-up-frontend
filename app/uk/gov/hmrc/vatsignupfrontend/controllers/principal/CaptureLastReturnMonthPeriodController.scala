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
import uk.gov.hmrc.vatsignupfrontend.forms.MonthForm._
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.capture_last_return_month_period

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class CaptureLastReturnMonthPeriodController @Inject()(view: capture_last_return_month_period)(implicit ec: ExecutionContext,
                                                         vcc: VatControllerComponents)
  extends AuthenticatedController(AdministratorRolePredicate) {

  def show: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      Future.successful(Ok(view(
        monthForm = monthForm,
        postAction = routes.CaptureLastReturnMonthPeriodController.submit()
      )))
    }
  }

  def submit: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      monthForm.bindFromRequest.fold(
        formWithErrors =>
          Future.successful(BadRequest(
            view(formWithErrors, routes.CaptureLastReturnMonthPeriodController.submit())
          )),
        lastReturnMonthPeriod =>
          Future.successful(
            Redirect(routes.CheckYourAnswersController.show())
              .addingToSession(SessionKeys.lastReturnMonthPeriodKey -> lastReturnMonthPeriod.toString)
          )
      )
    }
  }

}
