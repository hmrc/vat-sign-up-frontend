/*
 * Copyright 2019 HM Revenue & Customs
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
import uk.gov.hmrc.vatsignupfrontend.httpparsers.GetCompanyNameHttpParser.{CompanyNumberNotFound, GetCompanyNameFailureResponse, GetCompanyNameSuccess}
import uk.gov.hmrc.vatsignupfrontend.models.companieshouse.PartnershipCompanyType
import uk.gov.hmrc.vatsignupfrontend.services.GetCompanyNameService
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.capture_company_number

import scala.concurrent.Future

@Singleton
class CaptureCompanyNumberController @Inject()(val controllerComponents: ControllerComponents,
                                               val getCompanyNameService: GetCompanyNameService
                                              )
  extends AuthenticatedController(AdministratorRolePredicate) {

  val validateCompanyNumberForm: PrevalidationAPI[String] = companyNumberForm(isAgent = false, isPartnership = false)

  val show: Action[AnyContent] = Action.async {
    implicit request =>
      authorised() {
        Future.successful(
          Ok(capture_company_number(validateCompanyNumberForm.form, routes.CaptureCompanyNumberController.submit()))
        )
      }
  }

  val submit: Action[AnyContent] = Action.async {
    implicit request =>
      authorised() {
        validateCompanyNumberForm.bindFromRequest.fold(
          formWithErrors =>
            Future.successful(
              BadRequest(capture_company_number(formWithErrors, routes.CaptureCompanyNumberController.submit()))
            ),
          companyNumber =>
            getCompanyNameService.getCompanyName(companyNumber) map {
              case Right(GetCompanyNameSuccess(_, _: PartnershipCompanyType)) =>
                Redirect(routes.PartnershipAsCompanyErrorController.show())
              case Right(GetCompanyNameSuccess(companyName, _)) =>
                Redirect(routes.ConfirmCompanyController.show())
                  .addingToSession(
                    SessionKeys.companyNumberKey -> companyNumber,
                    SessionKeys.companyNameKey -> companyName
                  )
              case Left(CompanyNumberNotFound) =>
                Redirect(routes.CompanyNameNotFoundController.show())
              case Left(GetCompanyNameFailureResponse(status)) =>
                throw new InternalServerException(s"getCompanyName failed: status=$status")
            }
        )
      }
  }

}
