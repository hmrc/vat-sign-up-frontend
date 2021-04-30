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

package uk.gov.hmrc.vatsignupfrontend.controllers.agent.resignup

import javax.inject.{Inject, Singleton}
import play.api.mvc.{Action, AnyContent}
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.VatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.config.auth.AgentEnrolmentPredicate
import uk.gov.hmrc.vatsignupfrontend.controllers.AuthenticatedController
import uk.gov.hmrc.vatsignupfrontend.controllers.agent.routes.{CaptureVatNumberController, SignUpAnotherClientController}
import uk.gov.hmrc.vatsignupfrontend.models.BusinessEntity
import uk.gov.hmrc.vatsignupfrontend.utils.SessionUtils._
import uk.gov.hmrc.vatsignupfrontend.views.html.agent.resignup.sign_up_complete

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class SignUpCompleteController @Inject()(view: sign_up_complete)
                                        (implicit ec: ExecutionContext,
                                         vcc: VatControllerComponents)
  extends AuthenticatedController(AgentEnrolmentPredicate) {

  val show: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      val optVatNumber = request.session.get(SessionKeys.vatNumberKey).filter(_.nonEmpty)
      val optBusinessEntity = request.session.getModel[BusinessEntity](SessionKeys.businessEntityKey)

      (optVatNumber, optBusinessEntity) match {
        case (Some(vatNumber), Some(businessEntity)) =>
          Future.successful(
            Ok(view(businessEntity, vatNumber, SignUpAnotherClientController.submit()))
          )
        case _ =>
          Future.successful(Redirect(CaptureVatNumberController.show()))
      }
    }
  }

}

