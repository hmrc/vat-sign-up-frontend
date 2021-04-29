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
import play.api.mvc.{Action, AnyContent, Call}
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.VatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.config.auth.AdministratorRolePredicate
import uk.gov.hmrc.vatsignupfrontend.controllers.AuthenticatedController
import uk.gov.hmrc.vatsignupfrontend.forms.OtherBusinessEntityForm._
import uk.gov.hmrc.vatsignupfrontend.models._
import uk.gov.hmrc.vatsignupfrontend.utils.SessionUtils._
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.capture_business_entity_other

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class CaptureBusinessEntityOtherController @Inject()(view: capture_business_entity_other)
                                                    (implicit ec: ExecutionContext,
                                                     vcc: VatControllerComponents)
  extends AuthenticatedController(AdministratorRolePredicate) {

  private lazy val businessEntityRoute: Map[BusinessEntity, Call] = Map(
    VatGroup -> routes.VatGroupResolverController.resolve(),
    Division -> routes.DivisionResolverController.resolve(),
    UnincorporatedAssociation -> routes.UnincorporatedAssociationResolverController.resolve(),
    Trust -> routes.TrustResolverController.resolve(),
    RegisteredSociety -> routes.CaptureRegisteredSocietyCompanyNumberController.show(),
    Charity -> routes.CharityResolverController.resolve(),
    GovernmentOrganisation -> routes.GovernmentOrganisationResolverController.resolve()
  )

  val show: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      Future.successful(
        Ok(view(
          businessEntityForm = businessEntityForm(false),
          postAction = routes.CaptureBusinessEntityOtherController.submit()
        ))
      )
    }
  }

  def submit: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      businessEntityForm(false).bindFromRequest.fold(
        formWithErrors =>
          Future.successful(
            BadRequest(view(
              businessEntityForm = formWithErrors,
              postAction = routes.CaptureBusinessEntityOtherController.submit()
            ))
          ),
        businessEntity => Future.successful(
          Redirect(businessEntityRoute(businessEntity)).addingToSession(SessionKeys.businessEntityKey, businessEntity)
        )
      )
    }
  }

}
