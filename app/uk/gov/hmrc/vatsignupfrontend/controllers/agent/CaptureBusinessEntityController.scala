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
import uk.gov.hmrc.vatsignupfrontend.forms.BusinessEntityForm._
import uk.gov.hmrc.vatsignupfrontend.models.BusinessEntity.BusinessEntitySessionFormatter
import uk.gov.hmrc.vatsignupfrontend.models._
import uk.gov.hmrc.vatsignupfrontend.services.AdministrativeDivisionLookupService
import uk.gov.hmrc.vatsignupfrontend.utils.SessionUtils._
import uk.gov.hmrc.vatsignupfrontend.views.html.agent.capture_business_entity

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class CaptureBusinessEntityController @Inject()(administrativeDivisionLookupService: AdministrativeDivisionLookupService,
                                                view: capture_business_entity)
                                               (implicit ec: ExecutionContext,
                                                vcc: VatControllerComponents)
  extends AuthenticatedController(AgentEnrolmentPredicate) {

  val show: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      val entity = request.session.getModel[BusinessEntity](SessionKeys.businessEntityKey)
      val optVatNumber = request.session.get(SessionKeys.vatNumberKey)

      if (entity.contains(Overseas))
        Future.successful(
          Redirect(routes.OverseasResolverController.resolve())
        )
      else
        optVatNumber match {
          case Some(vatNumber) if administrativeDivisionLookupService.isAdministrativeDivision(vatNumber) =>
            Future.successful(
              Redirect(routes.DivisionResolverController.resolve())
                .addingToSession(SessionKeys.businessEntityKey, Division.asInstanceOf[BusinessEntity])
            )
          case Some(_) =>
            Future.successful(Ok(view(businessEntityForm, routes.CaptureBusinessEntityController.submit())))
          case _ =>
            Future.successful(
              Redirect(routes.CaptureVatNumberController.show())
            )
        }
    }
  }

  val submit: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      businessEntityForm.bindFromRequest.fold(
        formWithErrors =>
          Future.successful(
            BadRequest(view(
              formWithErrors,
              routes.CaptureBusinessEntityController.submit()
            ))
          ),
        entityType => {
          entityType match {
            case LimitedCompany => Future.successful(Redirect(routes.CaptureCompanyNumberController.show()))
            case SoleTrader => Future.successful(Redirect(soletrader.routes.CaptureNinoController.show()))
            case GeneralPartnership | LimitedPartnership => Future.successful(Redirect(partnerships.routes.ResolvePartnershipController.resolve()))
            case Other => Future.successful(Redirect(routes.CaptureBusinessEntityOtherController.show()))
          }
        } map (_.addingToSession(SessionKeys.businessEntityKey, entityType))
      )
    }
  }
}
