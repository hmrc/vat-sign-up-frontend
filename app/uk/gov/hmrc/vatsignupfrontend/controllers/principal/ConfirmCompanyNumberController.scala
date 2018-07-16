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

package uk.gov.hmrc.vatsignupfrontend.controllers.principal

import javax.inject.{Inject, Singleton}

import play.api.mvc.{Action, AnyContent}
import uk.gov.hmrc.http.InternalServerException
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.ControllerComponents
import uk.gov.hmrc.vatsignupfrontend.config.auth.AdministratorRolePredicate
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch.CtKnownFactsIdentityVerification
import uk.gov.hmrc.vatsignupfrontend.controllers.AuthenticatedController
import uk.gov.hmrc.vatsignupfrontend.services.StoreCompanyNumberService
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.confirm_company_number

import scala.concurrent.Future

@Singleton
class ConfirmCompanyNumberController @Inject()(val controllerComponents: ControllerComponents,
                                               val storeCompanyNumberService: StoreCompanyNumberService)
  extends AuthenticatedController(AdministratorRolePredicate) {

  val show: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      val optVatNumber = request.session.get(SessionKeys.vatNumberKey).filter(_.nonEmpty)
      val optCompanyNumber = request.session.get(SessionKeys.companyNumberKey).filter(_.nonEmpty)

      (optVatNumber, optCompanyNumber) match {
        case (Some(_), Some(companyNumber)) =>
          Future.successful(
            Ok(confirm_company_number(companyNumber, routes.ConfirmCompanyNumberController.submit()))
          )
        case (None, _) =>
          Future.successful(
            Redirect(routes.ResolveVatNumberController.resolve())
          )
        case _ =>
          Future.successful(
            Redirect(routes.CaptureCompanyNumberController.show())
          )
      }
    }
  }

  val submit: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      val optVatNumber = request.session.get(SessionKeys.vatNumberKey).filter(_.nonEmpty)
      val optCompanyNumber = request.session.get(SessionKeys.companyNumberKey).filter(_.nonEmpty)

      (optVatNumber, optCompanyNumber) match {
        case (Some(vatNumber), Some(companyNumber)) =>
          if (isEnabled(CtKnownFactsIdentityVerification))
            Future.successful(Redirect(routes.CaptureCompanyUtrController.show()))
          else
            storeCompanyNumberService.storeCompanyNumber(vatNumber, companyNumber, companyUtr = None) map {
              case Right(_) =>
                Redirect(routes.AgreeCaptureEmailController.show().url)
              case Left(errResponse) =>
                throw new InternalServerException("storeCompanyNumber failed: status=" + errResponse.status)
            }
        case (None, _) =>
          Future.successful(
            Redirect(routes.ResolveVatNumberController.resolve())
          )
        case _ =>
          Future.successful(
            Redirect(routes.CaptureCompanyNumberController.show())
          )
      }
    }
  }
}
