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
import uk.gov.hmrc.http.InternalServerException
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.VatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.config.auth.AdministratorRolePredicate
import uk.gov.hmrc.vatsignupfrontend.controllers.AuthenticatedController
import uk.gov.hmrc.vatsignupfrontend.controllers.principal.error.{routes => errorRoutes}
import uk.gov.hmrc.vatsignupfrontend.forms.CompanyNumberForm._
import uk.gov.hmrc.vatsignupfrontend.forms.prevalidation.PrevalidationAPI
import uk.gov.hmrc.vatsignupfrontend.httpparsers.GetCompanyNameHttpParser._
import uk.gov.hmrc.vatsignupfrontend.models.PartnershipEntityType.{LimitedLiabilityPartnership, LimitedPartnership, ScottishLimitedPartnership}
import uk.gov.hmrc.vatsignupfrontend.models.companieshouse.{NonPartnershipEntity, PartnershipCompanyType}
import uk.gov.hmrc.vatsignupfrontend.models.{PartnershipEntityType, companieshouse}
import uk.gov.hmrc.vatsignupfrontend.services.GetCompanyNameService
import uk.gov.hmrc.vatsignupfrontend.utils.SessionUtils._
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.partnerships.capture_partnership_company_number

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class CapturePartnershipCompanyNumberController @Inject()(getCompanyNameService: GetCompanyNameService,
                                                          view: capture_partnership_company_number)
                                                         (implicit ec: ExecutionContext,
                                                          vcc: VatControllerComponents)
  extends AuthenticatedController(AdministratorRolePredicate) {

  val validateCompanyNumberForm: PrevalidationAPI[String] = companyNumberForm(isAgent = false, isPartnership = true)

  private def toPartnershipEntityType(companyType: PartnershipCompanyType): PartnershipEntityType = {
    companyType match {
      case companieshouse.LimitedPartnership => LimitedPartnership
      case companieshouse.LimitedLiabilityPartnership => LimitedLiabilityPartnership
      case companieshouse.ScottishLimitedPartnership => ScottishLimitedPartnership
    }
  }

  val show: Action[AnyContent] = Action.async {
    implicit request =>
      authorised() {
        Future.successful(
          Ok(view(
            validateCompanyNumberForm.form,
            routes.CapturePartnershipCompanyNumberController.submit())
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
              BadRequest(view(
                formWithErrors,
                routes.CapturePartnershipCompanyNumberController.submit()
              ))
            ),
          companyNumber =>
            if (companyNumber.startsWith("BR")) {
              Future.successful(
                Redirect(errorRoutes.CouldNotConfirmCompanyController.show())
              )
            } else {
              getCompanyNameService.getCompanyName(companyNumber) map {
                case Right(CompanyDetails(companyName, companyType: PartnershipCompanyType)) =>
                  val partnershipEntity = toPartnershipEntityType(companyType)
                  Redirect(routes.ConfirmPartnershipController.show())
                    .addingToSession(
                      SessionKeys.companyNumberKey -> companyNumber,
                      SessionKeys.companyNameKey -> companyName
                    ).addingToSession(SessionKeys.partnershipTypeKey, partnershipEntity)
                case Right(CompanyDetails(_, NonPartnershipEntity)) =>
                  Redirect(errorRoutes.CouldNotConfirmLimitedPartnershipController.show())
                case Right(CompanyClosed(companyName)) =>
                  Redirect(errorRoutes.DissolvedCompanyController.show())
                    .addingToSession(
                      SessionKeys.companyNameKey -> companyName
                    )
                case Left(CompanyNumberNotFound) =>
                  Redirect(errorRoutes.CouldNotConfirmCompanyController.show())
                case Left(GetCompanyNameFailureResponse(status)) =>
                  throw new InternalServerException(s"getCompanyName failed: status=$status")
              }
            }
        )
      }
  }

}
