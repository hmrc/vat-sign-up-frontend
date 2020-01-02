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

package uk.gov.hmrc.vatsignupfrontend.controllers.principal

import javax.inject.{Inject, Singleton}
import play.api.mvc.{Action, AnyContent}
import uk.gov.hmrc.http.InternalServerException
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.ControllerComponents
import uk.gov.hmrc.vatsignupfrontend.config.auth.AdministratorRolePredicate
import uk.gov.hmrc.vatsignupfrontend.controllers.AuthenticatedController
import uk.gov.hmrc.vatsignupfrontend.forms.CompanyNumberForm._
import uk.gov.hmrc.vatsignupfrontend.forms.prevalidation.PrevalidationAPI
import uk.gov.hmrc.vatsignupfrontend.httpparsers.GetCompanyNameHttpParser._
import uk.gov.hmrc.vatsignupfrontend.services.GetCompanyNameService
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.capture_company_number

import scala.concurrent.Future

@Singleton
class CaptureRegisteredSocietyCompanyNumberController @Inject()(val controllerComponents: ControllerComponents,
                                                                val getCompanyNameService: GetCompanyNameService
                                                               )
  extends AuthenticatedController(AdministratorRolePredicate) {

  val validateCompanyNumberForm: PrevalidationAPI[String] = companyNumberForm(isAgent = false, isPartnership = false)


  val show: Action[AnyContent] = Action.async {
    implicit request =>
      authorised() {
        Future.successful(
          Ok(capture_company_number(
            validateCompanyNumberForm.form,
            routes.CaptureRegisteredSocietyCompanyNumberController.submit())
          )
        )
      }
  }

  val submit: Action[AnyContent] = Action.async {
    implicit request =>
      authorised() {
        validateCompanyNumberForm.bindFromRequest.fold(
          formWithErrors =>
            Future.successful(
              BadRequest(capture_company_number(
                formWithErrors,
                routes.CaptureRegisteredSocietyCompanyNumberController.submit()
              ))
            ),
          companyNumber =>
            if (companyNumber.startsWith("BR")) {
              Future.successful(
                Redirect(routes.RegisteredSocietyCompanyNameNotFoundController.show())
              )
            } else {
              getCompanyNameService.getCompanyName(companyNumber) map {
                case Right(CompanyDetails(societyName, _)) =>
                  Redirect(routes.ConfirmRegisteredSocietyController.show())
                    .addingToSession(
                      SessionKeys.registeredSocietyCompanyNumberKey -> companyNumber,
                      SessionKeys.registeredSocietyNameKey -> societyName
                    )
                case Right(CompanyClosed(companyName)) =>
                  Redirect(routes.DissolvedCompanyController.show())
                    .addingToSession(
                      SessionKeys.companyNameKey -> companyName
                    )
                case Left(CompanyNumberNotFound) =>
                  Redirect(routes.RegisteredSocietyCompanyNameNotFoundController.show())
                case Left(GetCompanyNameFailureResponse(status)) =>
                  throw new InternalServerException(s"getCompanyName failed: status=$status")
              }
            }
        )
      }
  }

}
