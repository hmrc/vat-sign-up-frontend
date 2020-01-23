/*
 * Copyright 2020 HM Revenue & Customs
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
import play.api.mvc.{Action, AnyContent, Request}
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.VatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.config.auth.AdministratorRolePredicate
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch.GeneralPartnershipNoSAUTR
import uk.gov.hmrc.vatsignupfrontend.controllers.AuthenticatedController
import uk.gov.hmrc.vatsignupfrontend.forms.PartnershipUtrForm.partnershipUtrForm
import uk.gov.hmrc.vatsignupfrontend.models.{BusinessEntity, GeneralPartnership}
import uk.gov.hmrc.vatsignupfrontend.utils.SessionUtils._
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.partnerships.capture_partnership_utr

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class CapturePartnershipUtrController @Inject()(implicit ec: ExecutionContext,
                                                  vcc: VatControllerComponents)
  extends AuthenticatedController(AdministratorRolePredicate) {

  val show: Action[AnyContent] = Action.async {
    implicit request =>
      authorised() {
        Future.successful(
          Ok(capture_partnership_utr(
            partnershipUtrForm.form,
            routes.CapturePartnershipUtrController.submit(),
            generalPartnershipNoSAUTRAndGeneralPartnership)
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
          .addingToSession(SessionKeys.hasOptionalSautrKey -> false.toString)
      )
    }
  }

  val submit: Action[AnyContent] = Action.async {
    implicit request =>
      authorised() {
        partnershipUtrForm.form.bindFromRequest.fold(
          formWithErrors =>
            Future.successful(
              BadRequest(capture_partnership_utr(
                formWithErrors,
                routes.CapturePartnershipUtrController.submit(),
                generalPartnershipNoSAUTRAndGeneralPartnership
              ))
            ),
          utr => Future.successful(
            if (generalPartnershipNoSAUTRAndGeneralPartnership) {
              Redirect(routes.PrincipalPlacePostCodeController.show())
                .addingToSession(SessionKeys.partnershipSautrKey -> utr)
                .removingFromSession(SessionKeys.hasOptionalSautrKey)
            }
            else {
              Redirect(routes.PrincipalPlacePostCodeController.show())
                .addingToSession(SessionKeys.partnershipSautrKey -> utr)
            }
          )
        )
      }
  }

  private def generalPartnershipNoSAUTRAndGeneralPartnership(implicit request: Request[AnyContent]): Boolean = {
    val generalPartnership = request.session.getModel[BusinessEntity](SessionKeys.businessEntityKey).contains(GeneralPartnership)
    generalPartnership && isEnabled(GeneralPartnershipNoSAUTR)
  }
}
