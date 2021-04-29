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
import uk.gov.hmrc.http.InternalServerException
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.VatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.config.auth.AgentEnrolmentPredicate
import uk.gov.hmrc.vatsignupfrontend.controllers.AuthenticatedController
import uk.gov.hmrc.vatsignupfrontend.controllers.agent.error.{routes => errorRoutes}
import uk.gov.hmrc.vatsignupfrontend.controllers.agent.{routes => agentRoutes}
import uk.gov.hmrc.vatsignupfrontend.httpparsers.StorePartnershipInformationHttpParser._
import uk.gov.hmrc.vatsignupfrontend.models._
import uk.gov.hmrc.vatsignupfrontend.services.StorePartnershipInformationService
import uk.gov.hmrc.vatsignupfrontend.utils.SessionUtils._
import uk.gov.hmrc.vatsignupfrontend.views.html.agent.partnerships.check_your_answers

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class CheckYourAnswersPartnershipController @Inject()(storePartnershipInformationService: StorePartnershipInformationService,
                                                      view: check_your_answers)
                                                     (implicit ec: ExecutionContext,
                                                      vcc: VatControllerComponents)
  extends AuthenticatedController(retrievalPredicate = AgentEnrolmentPredicate) {

  def show: Action[AnyContent] = Action.async { implicit request =>
    authorised() {

      val optVatNumber = request.session.get(SessionKeys.vatNumberKey).filter(_.nonEmpty)
      val optBusinessEntityType = request.session.getModel[BusinessEntity](SessionKeys.businessEntityKey)
      val optPartnershipUtr = request.session.get(SessionKeys.partnershipSautrKey).filter(_.nonEmpty)
      val optPartnershipPostCode = request.session.getModel[PostCode](SessionKeys.partnershipPostCodeKey)
      val optPartnershipCrn = request.session.get(SessionKeys.companyNumberKey).filter(_.nonEmpty)

      (optVatNumber, optBusinessEntityType, optPartnershipCrn, optPartnershipUtr) match {
        case (Some(_), Some(GeneralPartnership), _, _) =>
          Future.successful(Ok(view(
            entityType = GeneralPartnership,
            utr = optPartnershipUtr,
            companyNumber = None,
            postCode = optPartnershipPostCode,
            postAction = routes.CheckYourAnswersPartnershipController.submit()
          )))
        case (Some(_), Some(entity: LimitedPartnershipBase), Some(_), Some(_)) =>
          Future.successful(Ok(view(
            entityType = entity,
            utr = optPartnershipUtr,
            companyNumber = optPartnershipCrn,
            postCode = optPartnershipPostCode,
            postAction = routes.CheckYourAnswersPartnershipController.submit()
          )))
        case (None, _, _, _) =>
          Future.successful(Redirect(agentRoutes.CaptureVatNumberController.show()))
        case _ =>
          Future.successful(Redirect(agentRoutes.CaptureBusinessEntityController.show()))
      }
    }
  }

  def submit: Action[AnyContent] = Action.async { implicit request =>
    authorised() {

      val optVatNumber = request.session.get(SessionKeys.vatNumberKey).filter(_.nonEmpty)
      val optBusinessEntityType = request.session.getModel[BusinessEntity](SessionKeys.businessEntityKey)
      val optPartnershipUtr = request.session.get(SessionKeys.partnershipSautrKey).filter(_.nonEmpty)
      val optPartnershipPostCode = request.session.getModel[PostCode](SessionKeys.partnershipPostCodeKey)
      val optPartnershipCrn = request.session.get(SessionKeys.companyNumberKey).filter(_.nonEmpty)
      val optPartnershipType = request.session.getModel[PartnershipEntityType](SessionKeys.partnershipTypeKey)

      (optVatNumber, optBusinessEntityType) match {
        case (Some(vrn), Some(GeneralPartnership)) =>
          storePartnershipInformationService.storePartnershipInformation(
            vatNumber = vrn,
            sautr = optPartnershipUtr,
            postCode = optPartnershipPostCode
          ) map {
            case Right(StorePartnershipInformationSuccess) =>
              Redirect(agentRoutes.CaptureAgentEmailController.show())
            case Left(PartnershipUtrNotFound) =>
              Redirect(errorRoutes.CouldNotConfirmPartnershipController.show())
            case Left(StorePartnershipKnownFactsFailure) =>
              Redirect(errorRoutes.CouldNotConfirmPartnershipController.show())
            case Left(StorePartnershipInformationFailureResponse(status)) =>
              throw new InternalServerException("Store Partnership failed with status code: " + status)
          }
        case (Some(vrn), Some(entity: LimitedPartnershipBase)) =>
          (optPartnershipCrn, optPartnershipType, optPartnershipUtr) match {
            case (Some(crn), Some(partnershipType), Some(utr)) =>
              storePartnershipInformationService.storePartnershipInformation(
                vatNumber = vrn,
                sautr = optPartnershipUtr,
                companyNumber = crn,
                partnershipEntity = partnershipType,
                postCode = optPartnershipPostCode
              ) map {
                case Right(StorePartnershipInformationSuccess) =>
                  Redirect(agentRoutes.CaptureAgentEmailController.show())
                case Left(PartnershipUtrNotFound) =>
                  Redirect(errorRoutes.CouldNotConfirmPartnershipController.show())
                case Left(StorePartnershipKnownFactsFailure) =>
                  Redirect(errorRoutes.CouldNotConfirmPartnershipController.show())
                case Left(StorePartnershipInformationFailureResponse(status)) =>
                  throw new InternalServerException("Store Partnership failed with status code: " + status)
              }
            case _ =>
              Future.successful(Redirect(agentRoutes.CaptureBusinessEntityController.show()))
          }
        case (None, _) =>
          Future.successful(Redirect(agentRoutes.CaptureVatNumberController.show()))
        case _ =>
          Future.successful(Redirect(agentRoutes.CaptureBusinessEntityController.show()))
      }
    }
  }

}
