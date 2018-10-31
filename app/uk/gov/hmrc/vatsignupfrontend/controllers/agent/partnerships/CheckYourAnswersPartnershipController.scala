/*
 * Copyright 2018 HM Revenue & Customs
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
import uk.gov.hmrc.vatsignupfrontend.config.ControllerComponents
import uk.gov.hmrc.vatsignupfrontend.config.auth.AgentEnrolmentPredicate
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch.GeneralPartnershipJourney
import uk.gov.hmrc.vatsignupfrontend.controllers.AuthenticatedController
import uk.gov.hmrc.vatsignupfrontend.controllers.agent.{routes => agentRoutes}
import uk.gov.hmrc.vatsignupfrontend.httpparsers.StorePartnershipInformationHttpParser.{StorePartnershipInformationFailureResponse, StorePartnershipInformationSuccess}
import uk.gov.hmrc.vatsignupfrontend.models._
import uk.gov.hmrc.vatsignupfrontend.services.StorePartnershipInformationService
import uk.gov.hmrc.vatsignupfrontend.utils.SessionUtils._
import uk.gov.hmrc.vatsignupfrontend.views.html.agent.partnerships.check_your_answers

import scala.concurrent.Future

@Singleton
class CheckYourAnswersPartnershipController @Inject()(val controllerComponents: ControllerComponents,
                                                      val storePartnershipInformationService: StorePartnershipInformationService)
  extends AuthenticatedController(AgentEnrolmentPredicate, featureSwitches = Set(GeneralPartnershipJourney)) {

  def show: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      val optVatNumber = request.session.get(SessionKeys.vatNumberKey).filter(_.nonEmpty)
      val optSaUtr = request.session.get(SessionKeys.partnershipSautrKey).filter(_.nonEmpty)
      val optPartnershipType = request.session.get(SessionKeys.partnershipTypeKey).filter(_.nonEmpty)
      val optCompanyNumber = request.session.get(SessionKeys.companyNumberKey).filter(_.nonEmpty)
      val optBusinessPostCode = request.session.getModel[PostCode](SessionKeys.businessPostCodeKey)

      (optVatNumber, optPartnershipType, optSaUtr)  match {
        case (Some(vatNumber), Some(_), Some(saUtr)) =>
          Future.successful(
            Ok(check_your_answers(
              saUtr,
              optCompanyNumber,
              optBusinessPostCode,
              routes.CheckYourAnswersPartnershipController.submit())
            )
          )
        case (None, _, _) =>
          Future.successful(Redirect(agentRoutes.CaptureVatNumberController.show()))
        case (_, None, _) =>
          Future.successful(
            Redirect(agentRoutes.CaptureBusinessEntityController.show())
          )
        case (_, _, _) =>
          Future.successful(
            Redirect(routes.CapturePartnershipUtrController.show())
          )
      }
    }
  }

  def submit: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      val optVatNumber = request.session.get(SessionKeys.vatNumberKey).filter(_.nonEmpty)
      val optSaUtr = request.session.get(SessionKeys.partnershipSautrKey).filter(_.nonEmpty)
      val optPartnershipType = request.session.get(SessionKeys.partnershipTypeKey).filter(_.nonEmpty)
      val optCompanyNumber = request.session.get(SessionKeys.companyNumberKey).filter(_.nonEmpty)
      val optBusinessPostCode = request.session.getModel[PostCode](SessionKeys.businessPostCodeKey)

      (optVatNumber, optPartnershipType, optSaUtr) match {
        case (Some(vatNumber), Some(_), Some(saUtr)) =>
          storePartnershipInformationService.storePartnershipInformation(
            vatNumber = vatNumber,
            sautr = saUtr,
            companyNumber = optCompanyNumber,
            partnershipEntity = optPartnershipType,
            postCode = optBusinessPostCode
          ) map {
            case Right(StorePartnershipInformationSuccess) =>
              Redirect(agentRoutes.CaptureAgentEmailController.show())
            case Left(StorePartnershipInformationFailureResponse(failure)) =>
              throw new InternalServerException(s"Failed to save partnership information with error $failure")
          }
        case (None, _, _) =>
          Future.successful(
            Redirect(agentRoutes.CaptureVatNumberController.show())
          )
        case (_, None, _) =>
          Future.successful(
            Redirect(agentRoutes.CaptureBusinessEntityController.show())
          )
        case (_, _, _) =>
          Future.successful(
            Redirect(routes.CapturePartnershipUtrController.show())
          )
      }
    }
  }

}
