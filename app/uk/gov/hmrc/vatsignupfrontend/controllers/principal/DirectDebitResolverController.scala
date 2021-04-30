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

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class DirectDebitResolverController @Inject()(implicit ec: ExecutionContext,
                                              vcc: VatControllerComponents)
  extends AuthenticatedController(retrievalPredicate = AdministratorRolePredicate) {

  def show: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      val directDebitFlagFromSession: Boolean = request.session.get(SessionKeys.hasDirectDebitKey).getOrElse("false").toBoolean
      val isMigratedFlagFromSession: Boolean = request.session.get(SessionKeys.isMigratedKey).getOrElse("false").toBoolean

      if (isMigratedFlagFromSession) {
        Future.successful(Redirect(routes.SendYourApplicationController.show()))
      }
      else if (directDebitFlagFromSession) {
        Future.successful(Redirect(routes.DirectDebitTermsAndConditionsController.show()))
      }
      else
        Future.successful(Redirect(routes.CaptureEmailController.show()))
    }
  }

}
