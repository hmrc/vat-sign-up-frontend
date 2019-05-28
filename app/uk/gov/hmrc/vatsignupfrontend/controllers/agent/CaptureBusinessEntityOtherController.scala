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

package uk.gov.hmrc.vatsignupfrontend.controllers.agent

import javax.inject.{Inject, Singleton}
import play.api.mvc.{Action, AnyContent}
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.ControllerComponents
import uk.gov.hmrc.vatsignupfrontend.config.auth.AgentEnrolmentPredicate
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch._
import uk.gov.hmrc.vatsignupfrontend.controllers.AuthenticatedController
import uk.gov.hmrc.vatsignupfrontend.forms.OtherBusinessEntityForm._
import uk.gov.hmrc.vatsignupfrontend.models._
import uk.gov.hmrc.vatsignupfrontend.utils.SessionUtils._
import uk.gov.hmrc.vatsignupfrontend.views.html.agent.capture_business_entity_other

import scala.concurrent.Future

@Singleton
class CaptureBusinessEntityOtherController @Inject()(val controllerComponents: ControllerComponents)
  extends AuthenticatedController(AgentEnrolmentPredicate) {

  val show: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      Future.successful(
        Ok(capture_business_entity_other(
          businessEntityForm,
          routes.CaptureBusinessEntityOtherController.submit(),
          isEnabled(DivisionJourney),
          isEnabled(UnincorporatedAssociationJourney),
          isEnabled(TrustJourney),
          isEnabled(RegisteredSocietyJourney),
          isEnabled(GovernmentOrganisationJourney)
        ))
      )
    }
  }

  val submit: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      businessEntityForm.bindFromRequest.fold(
        formWithErrors =>
          Future.successful(
            BadRequest(capture_business_entity_other(
              formWithErrors,
              routes.CaptureBusinessEntityOtherController.submit(),
              isEnabled(DivisionJourney),
              isEnabled(UnincorporatedAssociationJourney),
              isEnabled(TrustJourney),
              isEnabled(RegisteredSocietyJourney),
              isEnabled(GovernmentOrganisationJourney)
            ))
          ),
        entityType => {
          entityType match {
            case VatGroup => Future.successful(Redirect(routes.VatGroupResolverController.resolve()))
            case Division => Future.successful(Redirect(routes.DivisionResolverController.resolve()))
            case UnincorporatedAssociation => Future.successful(Redirect(routes.UnincorporatedAssociationResolverController.resolve()))
            case Trust => Future.successful(Redirect(routes.TrustResolverController.resolve()))
            case RegisteredSociety => Future.successful(Redirect(routes.CaptureRegisteredSocietyCompanyNumberController.show()))
            case Charity => Future.successful(Redirect(routes.CharityResolverController.resolve()))
            case GovernmentOrganisation => Future.successful(Redirect(routes.GovernmentOrganisationResolverController.resolve()))
          }
        } map (_.addingToSession(SessionKeys.businessEntityKey, entityType))
      )
    }
  }
}