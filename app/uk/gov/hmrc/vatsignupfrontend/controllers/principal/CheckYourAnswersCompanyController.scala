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

package uk.gov.hmrc.vatsignupfrontend.controllers.principal

import javax.inject.{Inject, Singleton}
import play.api.mvc.{Action, AnyContent}
import uk.gov.hmrc.http.InternalServerException
import uk.gov.hmrc.vatsignupfrontend.SessionKeys._
import uk.gov.hmrc.vatsignupfrontend.config.VatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.config.auth.AdministratorRolePredicate
import uk.gov.hmrc.vatsignupfrontend.controllers.AuthenticatedController
import uk.gov.hmrc.vatsignupfrontend.controllers.principal.error.{routes => errorRoutes}
import uk.gov.hmrc.vatsignupfrontend.httpparsers.StoreCompanyNumberHttpParser.{CtReferenceMismatch, StoreCompanyNumberSuccess}
import uk.gov.hmrc.vatsignupfrontend.models._
import uk.gov.hmrc.vatsignupfrontend.services.StoreCompanyNumberService
import uk.gov.hmrc.vatsignupfrontend.utils.SessionUtils._
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.check_your_answers_company

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class CheckYourAnswersCompanyController @Inject()(storeCompanyNumberService: StoreCompanyNumberService,
                                                  view: check_your_answers_company)
                                                 (implicit ec: ExecutionContext,
                                                  vcc: VatControllerComponents)
  extends AuthenticatedController(AdministratorRolePredicate) {

  def show: Action[AnyContent] = Action.async { implicit request =>
    authorised() {

      val optBusinessEntity = request.session.getModel[BusinessEntity](businessEntityKey)
      val optCompanyNumber = request.session.get(companyNumberKey).filter(_.nonEmpty)
      val optCompanyUtr = request.session.get(companyUtrKey).filter(_.nonEmpty)

      if (optBusinessEntity.contains(LimitedCompany))
        (optCompanyNumber, optCompanyUtr) match {
          case (Some(companyNumber), Some(companyUtr)) =>
            Future.successful(
              Ok(view(
                companyNumber = companyNumber,
                companyUtr = companyUtr,
                businessEntity = LimitedCompany,
                routes.CheckYourAnswersCompanyController.submit()))
            )
          case (None, _) =>
            Future.successful(
              Redirect(routes.CaptureCompanyNumberController.show())
            )
          case (_, None) =>
            Future.successful(
              Redirect(routes.CaptureCompanyUtrController.show())
            )
        }
      else
        Future.successful(
          Redirect(routes.CaptureBusinessEntityController.show()).removingFromSession(
            businessEntityKey,
            companyNumberKey,
            companyUtrKey,
            ninoKey,
            partnershipSautrKey
          )
        )
    }
  }

  def submit: Action[AnyContent] = Action.async { implicit request =>
    authorised() {

      val optVatNumber = request.session.get(vatNumberKey).filter(_.nonEmpty)
      val optBusinessEntity = request.session.getModel[BusinessEntity](businessEntityKey)
      val optCompanyNumber = request.session.get(companyNumberKey).filter(_.nonEmpty)
      val optCompanyUtr = request.session.get(companyUtrKey).filter(_.nonEmpty)

      if (optBusinessEntity.contains(LimitedCompany))
        (optCompanyNumber, optCompanyUtr, optVatNumber) match {
          case (Some(companyNumber), Some(companyUtr), Some(vatNumber)) =>
            storeCompanyNumberService.storeCompanyNumber(vatNumber, companyNumber, Some(companyUtr)).map {
              case Right(StoreCompanyNumberSuccess) => Redirect(routes.DirectDebitResolverController.show())
              case Left(CtReferenceMismatch) => Redirect(errorRoutes.CouldNotConfirmBusinessController.show())
              case Left(failure) => throw new InternalServerException("unexpected response on store company number " + failure.status)
            }
          case (None, _, _) =>
            Future.successful(
              Redirect(routes.CaptureCompanyNumberController.show())
            )
          case (_, None, _) =>
            Future.successful(
              Redirect(routes.CaptureCompanyUtrController.show())
            )
          case (_, _, None) =>
            Future.successful(
              Redirect(routes.CaptureVatNumberController.show())
            )
        }
      else
        Future.successful(
          Redirect(routes.CaptureBusinessEntityController.show()).removingFromSession(
            businessEntityKey,
            companyNumberKey,
            companyUtrKey,
            ninoKey,
            partnershipSautrKey
          )
        )
    }
  }
}
