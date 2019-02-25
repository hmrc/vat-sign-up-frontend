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
import uk.gov.hmrc.vatsignupfrontend.forms.BoxFiveValueForm._
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.capture_box_five_value

import scala.concurrent.Future

@Singleton
class CaptureBoxFiveValueController @Inject()(val controllerComponents: ControllerComponents)

  extends AuthenticatedController(AdministratorRolePredicate, featureSwitches = Set(AdditionalKnownFacts)) {

  def show: Action[AnyContent] = Action.async {
    implicit request =>
      authorised() {
        Future.successful(Ok(capture_box_five_value(boxFiveValueForm.form, routes.CaptureBoxFiveValueController.submit())))
      }
  }

  def submit: Action[AnyContent] = Action.async {
    implicit request =>
      authorised() {
        boxFiveValueForm.bindFromRequest.fold(
          formWithErrors =>
            Future.successful(
              BadRequest(capture_box_five_value(formWithErrors, routes.CaptureBoxFiveValueController.submit()))
            )
          , formBoxFiveValue =>
            Future.successful(
              NotImplemented // TODO: Redirect to most recent vat payment page
                //.addingToSession(SessionKeys.boxFiveValueKey -> formBoxFiveValue)
            )
        )
      }
  }

}