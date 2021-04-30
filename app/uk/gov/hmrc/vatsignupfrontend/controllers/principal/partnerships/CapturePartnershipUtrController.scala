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
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.VatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.config.auth.AdministratorRolePredicate
import uk.gov.hmrc.vatsignupfrontend.controllers.AuthenticatedController
import uk.gov.hmrc.vatsignupfrontend.forms.PartnershipUtrForm.partnershipUtrForm
import uk.gov.hmrc.vatsignupfrontend.models.{BusinessEntity, GeneralPartnership}
import uk.gov.hmrc.vatsignupfrontend.utils.SessionUtils._
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.partnerships.capture_partnership_utr

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class CapturePartnershipUtrController @Inject()(view: capture_partnership_utr)
                                               (implicit ec: ExecutionContext,
                                                vcc: VatControllerComponents)
  extends AuthenticatedController(AdministratorRolePredicate) {

  val show: Action[AnyContent] = Action.async {
    implicit request =>
      val isGeneralPartnership = request.session.getModel[BusinessEntity](SessionKeys.businessEntityKey).contains(GeneralPartnership)
      authorised() {
        Future.successful(
          Ok(view(
            partnershipUtrForm.form,
            routes.CapturePartnershipUtrController.submit(),
            isGeneralPartnership)
          )
        )
      }
  }

  val noUtrSelected: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      Future.successful(
        Redirect(routes.CheckYourAnswersPartnershipsController.show())
          .removingFromSession(
            SessionKeys.partnershipSautrKey,
            SessionKeys.partnershipPostCodeKey
          )
      )
    }
  }

  val submit: Action[AnyContent] = Action.async {
    implicit request =>
      val isGeneralPartnership = request.session.getModel[BusinessEntity](SessionKeys.businessEntityKey).contains(GeneralPartnership)
      authorised() {
        partnershipUtrForm.form.bindFromRequest.fold(
          formWithErrors =>
            Future.successful(
              BadRequest(view(
                formWithErrors,
                routes.CapturePartnershipUtrController.submit(),
                isGeneralPartnership
              ))
            ),
          utr => Future.successful(
            if (isGeneralPartnership) {
              Redirect(routes.PrincipalPlacePostCodeController.show())
                .addingToSession(SessionKeys.partnershipSautrKey -> utr)
            }
            else {
              Redirect(routes.PrincipalPlacePostCodeController.show())
                .addingToSession(SessionKeys.partnershipSautrKey -> utr)
            }
          )
        )
      }
  }
}
