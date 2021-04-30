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
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.VatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.config.auth.AgentEnrolmentPredicate
import uk.gov.hmrc.vatsignupfrontend.controllers.AuthenticatedController
import uk.gov.hmrc.vatsignupfrontend.httpparsers.StoreEmailAddressHttpParser.StoreEmailAddressSuccess
import uk.gov.hmrc.vatsignupfrontend.services.StoreEmailAddressService
import uk.gov.hmrc.vatsignupfrontend.views.html.agent.confirm_email

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class ConfirmClientEmailController @Inject()(storeEmailAddressService: StoreEmailAddressService,
                                             view: confirm_email)
                                            (implicit ec: ExecutionContext,
                                             vcc: VatControllerComponents)
  extends AuthenticatedController(AgentEnrolmentPredicate) {

  val show: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      val optVatNumber = request.session.get(SessionKeys.vatNumberKey).filter(_.nonEmpty)
      val optEmail = request.session.get(SessionKeys.emailKey).filter(_.nonEmpty)

      (optVatNumber, optEmail) match {
        case (Some(_), Some(email)) =>
          Future.successful(
            Ok(view(email, routes.ConfirmClientEmailController.submit()))
          )
        case (None, _) =>
          Future.successful(
            Redirect(routes.CaptureVatNumberController.show())
          )
        case _ =>
          Future.successful(
            Redirect(routes.CaptureClientEmailController.show())
          )
      }
    }
  }

  val submit: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      val optVatNumber = request.session.get(SessionKeys.vatNumberKey).filter(_.nonEmpty)
      val optEmail = request.session.get(SessionKeys.emailKey).filter(_.nonEmpty)

      (optVatNumber, optEmail) match {
        case (Some(vatNumber), Some(email)) =>
          storeEmailAddressService.storeEmailAddress(vatNumber, email) map {
            case Right(StoreEmailAddressSuccess(false)) =>
              Redirect(routes.SentClientEmailController.show().url)
            case Right(StoreEmailAddressSuccess(true)) =>
              Redirect(routes.CheckYourAnswersFinalController.show())
            case Left(_) =>
              throw new InternalServerException("storeEmailAddress failed")
          }
        case (None, _) =>
          Future.successful(
            Redirect(routes.CaptureVatNumberController.show())
          )
        case _ =>
          Future.successful(
            Redirect(routes.CaptureClientEmailController.show())
          )
      }
    }
  }

}
