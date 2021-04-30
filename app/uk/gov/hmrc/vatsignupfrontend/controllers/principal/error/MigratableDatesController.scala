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

package uk.gov.hmrc.vatsignupfrontend.controllers.principal.error

import javax.inject.{Inject, Singleton}
import play.api.mvc.{Action, AnyContent}
import uk.gov.hmrc.vatsignupfrontend.SessionKeys._
import uk.gov.hmrc.vatsignupfrontend.config.VatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.config.auth.AdministratorRolePredicate
import uk.gov.hmrc.vatsignupfrontend.controllers.AuthenticatedController
import uk.gov.hmrc.vatsignupfrontend.controllers.principal.{routes => principalRoutes}
import uk.gov.hmrc.vatsignupfrontend.models.MigratableDates
import uk.gov.hmrc.vatsignupfrontend.utils.SessionUtils._
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.{sign_up_after_this_date, sign_up_between_these_dates}

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class MigratableDatesController @Inject()(signUpAfterView: sign_up_after_this_date,
                                          signUpBetweenView: sign_up_between_these_dates)
                                         (implicit ec: ExecutionContext,
                                          vcc: VatControllerComponents)
  extends AuthenticatedController(AdministratorRolePredicate) {

  val show: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      val optMigratableDates = request.session.getModel[MigratableDates](migratableDatesKey)
      Future.successful(
        optMigratableDates match {
          case Some(MigratableDates(Some(migratableDate), None)) => Ok(signUpAfterView(migratableDate))
          case Some(MigratableDates(Some(migratableDate), Some(migratableCutoffDate))) => Ok(signUpBetweenView(migratableDate, migratableCutoffDate))
          case _ => Redirect(principalRoutes.CaptureVatNumberController.show())
        }
      )
    }
  }

}
