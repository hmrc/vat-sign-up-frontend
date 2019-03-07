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

import javax.inject.Inject
import play.api.mvc.{Action, AnyContent}
import uk.gov.hmrc.http.NotFoundException
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.ControllerComponents
import uk.gov.hmrc.vatsignupfrontend.config.auth.AdministratorRolePredicate
import uk.gov.hmrc.vatsignupfrontend.controllers.AuthenticatedController

import scala.concurrent.Future

class DirectDebitResolverController @Inject()(val controllerComponents: ControllerComponents)
  extends AuthenticatedController(
    retrievalPredicate = AdministratorRolePredicate,
    featureSwitches = Set(???)
  ) {

  override protected def featureEnabled[T](func: => T): T =
    if (featureSwitches exists isEnabled) func
    else throw new NotFoundException(featureSwitchError)

  def resolve: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      val directDebitFlag: Boolean = request.session.get(SessionKeys.directDebitKey).getOrElse("false").toBoolean

      if (directDebitFlag)
        ??? // Not implemented
      else Future.successful(Redirect(???)) // Email
    }
  }

}
