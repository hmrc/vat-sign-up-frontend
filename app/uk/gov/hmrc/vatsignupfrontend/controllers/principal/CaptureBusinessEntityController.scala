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
import play.api.mvc._
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.VatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.config.auth.AdministratorRolePredicate
import uk.gov.hmrc.vatsignupfrontend.controllers.AuthenticatedController
import uk.gov.hmrc.vatsignupfrontend.forms.BusinessEntityForm._
import uk.gov.hmrc.vatsignupfrontend.models.BusinessEntity.BusinessEntitySessionFormatter
import uk.gov.hmrc.vatsignupfrontend.models._
import uk.gov.hmrc.vatsignupfrontend.services.{AdministrativeDivisionLookupService, StoreOverseasInformationService}
import uk.gov.hmrc.vatsignupfrontend.utils.SessionUtils._
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.capture_business_entity

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class CaptureBusinessEntityController @Inject()(storeOverseasInformationService: StoreOverseasInformationService,
                                                administrativeDivisionLookupService: AdministrativeDivisionLookupService,
                                                view: capture_business_entity)
                                               (implicit ec: ExecutionContext,
                                                vcc: VatControllerComponents)
  extends AuthenticatedController(AdministratorRolePredicate) {

  private lazy val businessEntityRoute: Map[BusinessEntity, Call] = Map(
    SoleTrader -> soletrader.routes.SoleTraderResolverController.resolve(),
    LimitedCompany -> routes.CaptureCompanyNumberController.show(),
    GeneralPartnership -> partnerships.routes.ResolvePartnershipUtrController.resolve(),
    LimitedPartnership -> partnerships.routes.CapturePartnershipCompanyNumberController.show(),
    Other -> routes.CaptureBusinessEntityOtherController.show()
  )

  val show: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      val entity = request.session.getModel[BusinessEntity](SessionKeys.businessEntityKey)
      val optVatNumber = request.session.get(SessionKeys.vatNumberKey)

      if (entity.contains(Overseas))
        Future.successful(
          Redirect(routes.OverseasResolverController.resolve())
        )
      else optVatNumber match {
        case Some(vatNumber) if administrativeDivisionLookupService.isAdministrativeDivision(vatNumber) =>
          Future.successful(
            Redirect(routes.DivisionResolverController.resolve())
              .addingToSession(SessionKeys.businessEntityKey, Division.asInstanceOf[BusinessEntity])
          )
        case Some(_) =>
          Future.successful(
            Ok(view(businessEntityForm, routes.CaptureBusinessEntityController.submit()))
          )
        case _ =>
          Future.successful(
            Redirect(routes.CaptureVatNumberController.show())
          )
      }
    }
  }

  def submit: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      businessEntityForm.bindFromRequest.fold(
        formWithErrors =>
          Future.successful(
            BadRequest(view(
              businessEntityForm = formWithErrors,
              postAction = routes.CaptureBusinessEntityController.submit()
            ))
          ),
        businessEntity => Future.successful(
          Redirect(businessEntityRoute(businessEntity))
            .addingToSession(SessionKeys.businessEntityKey, businessEntity)
        )
      )
    }
  }

}
