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

package uk.gov.hmrc.vatsignupfrontend.controllers.agent.partnerships

import javax.inject.{Inject, Singleton}
import play.api.mvc.{Action, AnyContent}
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.ControllerComponents
import uk.gov.hmrc.vatsignupfrontend.config.auth.AgentEnrolmentPredicate
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch._
import uk.gov.hmrc.vatsignupfrontend.controllers.AuthenticatedController
import uk.gov.hmrc.vatsignupfrontend.forms.DoYouHaveAUtrForm.doYouHaveAUtrForm
import uk.gov.hmrc.vatsignupfrontend.models.{No, Yes}
import uk.gov.hmrc.vatsignupfrontend.views.html.agent.partnerships.does_your_client_have_a_utr

import scala.concurrent.Future

@Singleton
class DoesYourClientHaveAUtrController @Inject()(val controllerComponents: ControllerComponents)
  extends AuthenticatedController(AgentEnrolmentPredicate, featureSwitches = Set(OptionalSautrJourney)) {

  val show: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      Future.successful(
        Ok(does_your_client_have_a_utr(
          doYouHaveAUtrForm = doYouHaveAUtrForm(isAgent = true),
          postAction = routes.DoesYourClientHaveAUtrController.submit()
        ))
      )
    }
  }

  val submit: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      Future.successful(doYouHaveAUtrForm(isAgent = true).bindFromRequest.fold(
        formWithErrors => BadRequest(does_your_client_have_a_utr(
          doYouHaveAUtrForm = formWithErrors,
          postAction = routes.DoesYourClientHaveAUtrController.submit()
        )), {
          case Yes => Redirect(routes.CapturePartnershipUtrController.show())
            .addingToSession(SessionKeys.hasOptionalSautrKey -> true.toString)
          case No => Redirect(routes.CheckYourAnswersPartnershipController.show())
            .removingFromSession(SessionKeys.partnershipSautrKey)
            .removingFromSession(SessionKeys.partnershipPostCodeKey)
            .addingToSession(SessionKeys.hasOptionalSautrKey -> false.toString)
        }
      ))
    }
  }
}
