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

package uk.gov.hmrc.vatsignupfrontend.controllers.agent

import javax.inject.{Inject, Singleton}
import play.api.mvc.{Action, AnyContent}
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.VatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.config.auth.AgentEnrolmentPredicate
import uk.gov.hmrc.vatsignupfrontend.controllers.AuthenticatedController
import uk.gov.hmrc.vatsignupfrontend.forms.VatNumberForm._
import uk.gov.hmrc.vatsignupfrontend.views.html.agent.capture_vat_number

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class CaptureVatNumberController @Inject()(view: capture_vat_number)
                                          (implicit ec: ExecutionContext,
                                           vcc: VatControllerComponents)
  extends AuthenticatedController(AgentEnrolmentPredicate) {

  private val validateVatNumberForm = vatNumberForm(isAgent = true)

  val show: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      Future.successful(
        Ok(view(validateVatNumberForm.form, routes.CaptureVatNumberController.submit()))
      )
    }
  }

  val submit: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      validateVatNumberForm.bindFromRequest.fold(
        formWithErrors =>
          Future.successful(
            BadRequest(view(formWithErrors, routes.CaptureVatNumberController.submit()))
          ),
        vatNumber =>
          Future.successful(
            Redirect(routes.ConfirmVatNumberController.show()).addingToSession(SessionKeys.vatNumberKey -> vatNumber)
          )
      )
    }
  }
}
