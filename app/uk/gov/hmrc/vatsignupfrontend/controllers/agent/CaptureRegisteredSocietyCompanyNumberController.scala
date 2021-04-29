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
import uk.gov.hmrc.vatsignupfrontend.controllers.agent.error.{routes => errorRoutes}
import uk.gov.hmrc.vatsignupfrontend.forms.CompanyNumberForm._
import uk.gov.hmrc.vatsignupfrontend.httpparsers.GetCompanyNameHttpParser._
import uk.gov.hmrc.vatsignupfrontend.services.GetCompanyNameService
import uk.gov.hmrc.vatsignupfrontend.views.html.agent.capture_company_number

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class CaptureRegisteredSocietyCompanyNumberController @Inject()(getCompanyNameService: GetCompanyNameService,
                                                                view: capture_company_number)
                                                               (implicit ec: ExecutionContext,
                                                                vcc: VatControllerComponents)
  extends AuthenticatedController(AgentEnrolmentPredicate) {

  val validateCompanyNumberForm = companyNumberForm(isAgent = true, isPartnership = false)

  val show: Action[AnyContent] = Action.async {
    implicit request =>
      authorised() {
        Future.successful(
          Ok(view(validateCompanyNumberForm.form, routes.CaptureRegisteredSocietyCompanyNumberController.submit()))
        )
      }
  }

  val submit: Action[AnyContent] = Action.async {
    implicit request =>
      authorised() {
        validateCompanyNumberForm.bindFromRequest.fold(
          formWithErrors =>
            Future.successful(
              BadRequest(view(formWithErrors, routes.CaptureCompanyNumberController.submit()))
            ),
          companyNumber =>
            if (companyNumber.startsWith("BR")) {
              Future.successful(
                Redirect(errorRoutes.CompanyNameNotFoundController.show())
              )
            } else {
              getCompanyNameService.getCompanyName(companyNumber) map {
                case Right(CompanyDetails(companyName, _)) =>
                  Redirect(routes.ConfirmRegisteredSocietyController.show())
                    .addingToSession(
                      SessionKeys.registeredSocietyCompanyNumberKey -> companyNumber,
                      SessionKeys.registeredSocietyNameKey -> companyName
                    )
                case Right(CompanyClosed(companyName)) =>
                  Redirect(errorRoutes.DissolvedCompanyController.show())
                    .addingToSession(
                      SessionKeys.companyNameKey -> companyName
                    )
                case Left(CompanyNumberNotFound) =>
                  Redirect(errorRoutes.RegisteredSocietyCompanyNameNotFoundController.show())
                case Left(GetCompanyNameFailureResponse(status)) =>
                  throw new InternalServerException(s"getCompanyName failed: status=$status")
              }
            }
        )
      }
  }
}