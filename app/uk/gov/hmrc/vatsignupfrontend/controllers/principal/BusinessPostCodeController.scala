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
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.ControllerComponents
import uk.gov.hmrc.vatsignupfrontend.config.auth.AdministratorRolePredicate
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch.AdditionalKnownFacts
import uk.gov.hmrc.vatsignupfrontend.controllers.AuthenticatedController
import uk.gov.hmrc.vatsignupfrontend.forms.BusinessPostCodeForm._
import uk.gov.hmrc.vatsignupfrontend.utils.SessionUtils._
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.principal_place_of_business

import scala.concurrent.Future

@Singleton
class BusinessPostCodeController @Inject()(val controllerComponents: ControllerComponents)
  extends AuthenticatedController(AdministratorRolePredicate) {

  def show: Action[AnyContent] = Action.async {
    implicit request =>
      authorised() {
        Future.successful(
          Ok(principal_place_of_business(businessPostCodeForm.form, routes.BusinessPostCodeController.submit()))
        )
      }
  }

  def submit: Action[AnyContent] = Action.async {
    implicit request =>
      authorised() {
        businessPostCodeForm.bindFromRequest.fold(
          formWithErrors =>
            Future.successful(
              BadRequest(principal_place_of_business(formWithErrors, routes.BusinessPostCodeController.submit()))
            ),
          businessPostCode => {
            val isMigrated: Boolean = request.session.get(SessionKeys.isMigratedKey).contains("true")

            if(isMigrated || !isEnabled(AdditionalKnownFacts)) {
              Future.successful(
                Redirect(routes.CheckYourAnswersController.show())
                  .addingToSession(SessionKeys.businessPostCodeKey, businessPostCode)
              )
            } else {
              Future.successful(
                Redirect(routes.PreviousVatReturnController.show())
                  .addingToSession(SessionKeys.businessPostCodeKey, businessPostCode)
              )
            }
          }
        )
      }
  }

}
