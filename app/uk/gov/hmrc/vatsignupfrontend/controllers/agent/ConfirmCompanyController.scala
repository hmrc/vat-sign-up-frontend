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
import uk.gov.hmrc.http.InternalServerException
import uk.gov.hmrc.vatsignupfrontend.SessionKeys._
import uk.gov.hmrc.vatsignupfrontend.config.VatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.config.auth.AgentEnrolmentPredicate
import uk.gov.hmrc.vatsignupfrontend.controllers.AuthenticatedController
import uk.gov.hmrc.vatsignupfrontend.services.StoreCompanyNumberService
import uk.gov.hmrc.vatsignupfrontend.views.html.agent.confirm_company

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class ConfirmCompanyController @Inject()(storeCompanyNumberService: StoreCompanyNumberService,
                                         view: confirm_company)
                                        (implicit ec: ExecutionContext,
                                         vcc: VatControllerComponents)
  extends AuthenticatedController(AgentEnrolmentPredicate) {

  val show: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      val optCompanyName = request.session.get(companyNameKey).filter(_.nonEmpty)

      Future.successful(
        optCompanyName match {
          case Some(companyName) =>
            val changeLink = routes.CaptureCompanyNumberController.show().url
            Ok(view(
              companyName = companyName,
              postAction = routes.ConfirmCompanyController.submit(),
              changeLink = changeLink
            ))
          case _ =>
            Redirect(routes.CaptureCompanyNumberController.show())
        }
      )
    }
  }

  val submit: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      val optVatNumber = request.session.get(vatNumberKey).filter(_.nonEmpty)
      val optCompanyNumber = request.session.get(companyNumberKey).filter(_.nonEmpty)

      (optVatNumber, optCompanyNumber) match {
        case (Some(vatNumber), Some(companyNumber)) =>
          storeCompanyNumberService.storeCompanyNumber(vatNumber, companyNumber, companyUtr = None) map {
            case Right(_) =>
              Redirect(routes.CaptureAgentEmailController.show().url)
            case Left(errResponse) =>
              throw new InternalServerException("storeCompanyNumber failed: status=" + errResponse.status)
          }
        case (None, _) =>
          Future.successful(
            Redirect(routes.CaptureVatNumberController.show())
          )
        case _ =>
          Future.successful(
            Redirect(routes.CaptureCompanyNumberController.show())
          )
      }

    }
  }

}
