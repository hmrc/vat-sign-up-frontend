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

package uk.gov.hmrc.vatsignupfrontend.controllers.agent.partnerships

import javax.inject.{Inject, Singleton}
import play.api.mvc.{Action, AnyContent}
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.VatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.config.auth.AgentEnrolmentPredicate
import uk.gov.hmrc.vatsignupfrontend.controllers.AuthenticatedController
import uk.gov.hmrc.vatsignupfrontend.forms.PartnershipPostCodeForm._
import uk.gov.hmrc.vatsignupfrontend.forms.prevalidation.PrevalidationAPI
import uk.gov.hmrc.vatsignupfrontend.models.PostCode
import uk.gov.hmrc.vatsignupfrontend.utils.SessionUtils._
import uk.gov.hmrc.vatsignupfrontend.views.html.agent.partnerships.partnership_ppob

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class PartnershipPostCodeController @Inject()(view: partnership_ppob)
                                             (implicit ec: ExecutionContext,
                                              vcc: VatControllerComponents)
  extends AuthenticatedController(AgentEnrolmentPredicate) {

  val postCodeForm: PrevalidationAPI[PostCode] = partnershipPostCodeForm(isAgent = true)

  def show: Action[AnyContent] = Action.async {
    implicit request =>
      authorised() {
        Future.successful(
          Ok(view(postCodeForm.form, routes.PartnershipPostCodeController.submit()))
        )
      }
  }

  def submit: Action[AnyContent] = Action.async {
    implicit request =>
      authorised() {
        postCodeForm.bindFromRequest.fold(
          formWithErrors =>
            Future.successful(
              BadRequest(view(formWithErrors, routes.PartnershipPostCodeController.submit()))
            ),
          partnershipPostCode =>
            Future.successful(
              Redirect(routes.CheckYourAnswersPartnershipController.show()) addingToSession(SessionKeys.partnershipPostCodeKey, partnershipPostCode)
            )
        )
      }
  }

}
