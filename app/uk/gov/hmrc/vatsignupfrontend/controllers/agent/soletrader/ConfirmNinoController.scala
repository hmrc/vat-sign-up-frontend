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

package uk.gov.hmrc.vatsignupfrontend.controllers.agent.soletrader

import javax.inject.{Inject, Singleton}
import play.api.mvc.{Action, AnyContent}
import uk.gov.hmrc.http.InternalServerException
import uk.gov.hmrc.vatsignupfrontend.SessionKeys.{businessEntityKey, ninoKey, vatNumberKey}
import uk.gov.hmrc.vatsignupfrontend.config.VatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.config.auth.AgentEnrolmentPredicate
import uk.gov.hmrc.vatsignupfrontend.controllers.AuthenticatedController
import uk.gov.hmrc.vatsignupfrontend.controllers.agent.routes._
import uk.gov.hmrc.vatsignupfrontend.httpparsers.StoreNinoHttpParser.{NoVATNumberFailure, StoreNinoFailureResponse, StoreNinoSuccess}
import uk.gov.hmrc.vatsignupfrontend.models.BusinessEntity
import uk.gov.hmrc.vatsignupfrontend.services.StoreNinoService
import uk.gov.hmrc.vatsignupfrontend.utils.SessionUtils._
import uk.gov.hmrc.vatsignupfrontend.views.html.agent.soletrader.confirm_nino

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class ConfirmNinoController @Inject()(storeNinoService: StoreNinoService,
                                      view: confirm_nino)
                                     (implicit ec: ExecutionContext,
                                      vcc: VatControllerComponents)
  extends AuthenticatedController(AgentEnrolmentPredicate) {

  val show: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      val optNino = request.session.get(ninoKey).filter(_.nonEmpty)
      val optBusinessEntity = request.session.getModel[BusinessEntity](businessEntityKey)

      (optBusinessEntity, optNino) match {
        case (None, _) =>
          Future.successful(Redirect(CaptureBusinessEntityController.show()))
        case (_, None) =>
          Future.successful(Redirect(routes.CaptureNinoController.show()))
        case (Some(businessEntity), Some(nino)) =>
          Future.successful(
            Ok(view(businessEntity, nino, routes.ConfirmNinoController.submit()))
          )
      }
    }
  }

  val submit: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      val optVatNumber = request.session.get(vatNumberKey).filter(_.nonEmpty)
      val optNino = request.session.get(ninoKey).filter(_.nonEmpty)

      (optVatNumber, optNino) match {
        case (None, _) =>
          Future.successful(Redirect(CaptureVatNumberController.show()))
        case (_, None) =>
          Future.successful(Redirect(routes.CaptureNinoController.show()))
        case (Some(vatNumber), Some(nino)) =>
          storeNinoService.storeNino(vatNumber, nino) map {
            case Right(StoreNinoSuccess) =>
              Redirect(CaptureAgentEmailController.show())
            case Left(NoVATNumberFailure) =>
              throw new InternalServerException(s"Failure calling store nino: vat number is not found")
            case Left(StoreNinoFailureResponse(status)) =>
              throw new InternalServerException(s"Failure calling store nino: status=$status")
          }
      }
    }
  }

}
