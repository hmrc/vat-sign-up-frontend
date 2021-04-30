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

package uk.gov.hmrc.vatsignupfrontend.controllers.principal.partnerships

import javax.inject.{Inject, Singleton}
import play.api.mvc.{Action, AnyContent}
import uk.gov.hmrc.http.{BadGatewayException, InternalServerException}
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.VatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.config.auth.AdministratorRolePredicate
import uk.gov.hmrc.vatsignupfrontend.controllers.AuthenticatedController
import uk.gov.hmrc.vatsignupfrontend.controllers.principal.error.{routes => errorRoutes}
import uk.gov.hmrc.vatsignupfrontend.controllers.principal.{routes => principalRoutes}
import uk.gov.hmrc.vatsignupfrontend.forms.ConfirmGeneralPartnershipForm._
import uk.gov.hmrc.vatsignupfrontend.httpparsers.StorePartnershipInformationHttpParser.StorePartnershipInformationSuccess
import uk.gov.hmrc.vatsignupfrontend.models.{No, PartnershipEntityType, Yes}
import uk.gov.hmrc.vatsignupfrontend.services.StorePartnershipInformationService
import uk.gov.hmrc.vatsignupfrontend.utils.SessionUtils._
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.partnerships.confirm_partnership_utr

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class ConfirmLimitedPartnershipController @Inject()(storePartnershipInformationService: StorePartnershipInformationService,
                                                    view: confirm_partnership_utr)
                                                   (implicit ec: ExecutionContext,
                                                    vcc: VatControllerComponents)
  extends AuthenticatedController(AdministratorRolePredicate) {

  val show: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      val optVatNumber = request.session.get(SessionKeys.vatNumberKey).filter(_.nonEmpty)
      val optPartnershipUtr = request.session.get(SessionKeys.partnershipSautrKey).filter(_.nonEmpty)
      val optCompanyName = request.session.get(SessionKeys.companyNameKey).filter(_.nonEmpty)
      val optCompanyNumber = request.session.get(SessionKeys.companyNumberKey).filter(_.nonEmpty)
      val optPartnershipType = request.session.get(SessionKeys.partnershipTypeKey).filter(_.nonEmpty)

      (optVatNumber, optPartnershipUtr, optCompanyName, optCompanyNumber, optPartnershipType) match {
        case (Some(_), Some(partnershipUtr), Some(_), Some(_), Some(_)) =>
          Future.successful(
            Ok(view(
              partnershipUtr, limitedPartnershipName = optCompanyName, confirmPartnershipForm, routes.ConfirmLimitedPartnershipController.submit()
            ))
          )
        case (None, _, _, _, _) =>
          Future.successful(Redirect(principalRoutes.ResolveVatNumberController.resolve()))
        case (_, None, _, _, _) =>
          Future.failed(new InternalServerException("Cannot capture user's UTR")) // TODO Create Capture partnership Utr flow
        case (_, _, None, _, _) | (_, _, _, None, _) | (_, _, _, _, None) =>
          Future.successful(Redirect(routes.CapturePartnershipCompanyNumberController.show()))
      }
    }
  }

  val submit: Action[AnyContent] = Action.async { implicit request =>
    val optVatNumber = request.session.get(SessionKeys.vatNumberKey).filter(_.nonEmpty)
    val optPartnershipUtr = request.session.get(SessionKeys.partnershipSautrKey).filter(_.nonEmpty)
    val optCompanyName = request.session.get(SessionKeys.companyNameKey).filter(_.nonEmpty)
    val optCompanyNumber = request.session.get(SessionKeys.companyNumberKey).filter(_.nonEmpty)
    val optPartnershipType = request.session.getModel[PartnershipEntityType](SessionKeys.partnershipTypeKey)

    authorised() {
      (optVatNumber, optPartnershipUtr, optCompanyNumber, optPartnershipType) match {
        case (Some(vatNumber), Some(partnershipUtr), Some(companyNumber), Some(partnershipType)) =>
          confirmPartnershipForm.bindFromRequest().fold(
            error => Future.successful(BadRequest(
              view(partnershipUtr, limitedPartnershipName = optCompanyName, error, routes.ConfirmLimitedPartnershipController.submit())
            ))
            , {
              case Yes =>
                storePartnershipInformationService.storePartnershipInformation(
                  vatNumber = vatNumber,
                  sautr = Some(partnershipUtr),
                  companyNumber = companyNumber,
                  partnershipEntity = partnershipType,
                  postCode = None
                ) flatMap {
                  case Right(StorePartnershipInformationSuccess) =>
                    Future.successful(Redirect(principalRoutes.DirectDebitResolverController.show()))
                  case Left(_) =>
                    Future.failed(new BadGatewayException("Store partnership information failed"))
                }
              case No =>
                Future.successful(Redirect(errorRoutes.SignInWithDifferentDetailsPartnershipController.show()))
            }
          )
        case (None, _, _, _) =>
          Future.successful(Redirect(principalRoutes.ResolveVatNumberController.resolve()))
        case (_, None, _, _) =>
          Future.failed(new InternalServerException("Cannot capture user's UTR")) // TODO Create Capture partnership Utr flow
        case _ =>
          Future.successful(Redirect(routes.CapturePartnershipCompanyNumberController.show()))
      }
    }
  }
}
