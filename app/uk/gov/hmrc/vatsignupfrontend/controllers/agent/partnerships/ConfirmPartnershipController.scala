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
import uk.gov.hmrc.vatsignupfrontend.controllers.agent.{routes => agentRoutes}
import uk.gov.hmrc.vatsignupfrontend.views.html.agent.partnerships.confirm_partnership

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class ConfirmPartnershipController @Inject()(view: confirm_partnership)
                                            (implicit ec: ExecutionContext,
                                             vcc: VatControllerComponents)
  extends AuthenticatedController(AgentEnrolmentPredicate) {

  val show: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      val optVatNumber = request.session.get(SessionKeys.vatNumberKey).filter(_.nonEmpty)
      val optCompanyNumber = request.session.get(SessionKeys.companyNumberKey).filter(_.nonEmpty)
      val optCompanyName = request.session.get(SessionKeys.companyNameKey).filter(_.nonEmpty)
      val optPartnershipType = request.session.get(SessionKeys.partnershipTypeKey).filter(_.nonEmpty)
      Future.successful(
        (optVatNumber, optCompanyNumber, optCompanyName, optPartnershipType) match {
          case (Some(_), Some(_), Some(companyName), Some(_)) =>
            Ok(view(
              companyName = companyName,
              postAction = routes.ConfirmPartnershipController.submit(),
              changeLink = agentRoutes.CaptureBusinessEntityController.show().url
            ))
          case (None, _, _, _) =>
            Redirect(agentRoutes.CaptureVatNumberController.show())
          case _ =>
            Redirect(routes.AgentCapturePartnershipCompanyNumberController.show())
        }
      )
    }
  }

  val submit: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      val optVatNumber = request.session.get(SessionKeys.vatNumberKey).filter(_.nonEmpty)
      val optCompanyNumber = request.session.get(SessionKeys.companyNumberKey).filter(_.nonEmpty)
      val optCompanyName = request.session.get(SessionKeys.companyNameKey).filter(_.nonEmpty)
      val optPartnershipType = request.session.get(SessionKeys.partnershipTypeKey).filter(_.nonEmpty)

      (optVatNumber, optCompanyNumber, optCompanyName, optPartnershipType) match {
        case (Some(_), Some(_), Some(_), Some(_)) =>
          Future.successful(Redirect(routes.CapturePartnershipUtrController.show()))
        case (None, _, _, _) =>
          Future.successful(Redirect(agentRoutes.CaptureVatNumberController.show()))
        case _ =>
          Future.successful(Redirect(routes.AgentCapturePartnershipCompanyNumberController.show()))
      }
    }
  }

}
