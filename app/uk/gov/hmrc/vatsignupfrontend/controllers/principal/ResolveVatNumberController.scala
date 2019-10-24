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

package uk.gov.hmrc.vatsignupfrontend.controllers.principal

import javax.inject.{Inject, Singleton}
import play.api.mvc.{Action, AnyContent}
import uk.gov.hmrc.auth.core.retrieve.Retrievals
import uk.gov.hmrc.vatsignupfrontend.config.ControllerComponents
import uk.gov.hmrc.vatsignupfrontend.config.auth.AdministratorRolePredicate
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch.ReSignUpJourney
import uk.gov.hmrc.vatsignupfrontend.controllers.AuthenticatedController
import uk.gov.hmrc.vatsignupfrontend.utils.EnrolmentUtils._

import scala.concurrent.Future

@Singleton
class ResolveVatNumberController @Inject()(val controllerComponents: ControllerComponents)
  extends AuthenticatedController(AdministratorRolePredicate) {

  val resolve: Action[AnyContent] = Action.async { implicit request =>
    authorised()(Retrievals.allEnrolments) {

      enrolments =>
        if (isEnabled(ReSignUpJourney) && enrolments.getAnyVatNumber.isDefined || enrolments.vatNumber.isDefined)
          Future.successful(Redirect(routes.MultipleVatCheckController.show()))
         else
          Future.successful(Redirect(routes.CaptureVatNumberController.show()))
    }
  }
}
