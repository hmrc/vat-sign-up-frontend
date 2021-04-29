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
import play.api.mvc.{Action, AnyContent, Request, Result}
import uk.gov.hmrc.http.InternalServerException
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.VatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.config.auth.AdministratorRolePredicate
import uk.gov.hmrc.vatsignupfrontend.connectors.SubscriptionRequestSummaryConnector
import uk.gov.hmrc.vatsignupfrontend.controllers.AuthenticatedController
import uk.gov.hmrc.vatsignupfrontend.httpparsers.GetCompanyNameHttpParser.{CompanyClosed, CompanyDetails}
import uk.gov.hmrc.vatsignupfrontend.httpparsers.SubmissionHttpParser.SubmissionFailureResponse
import uk.gov.hmrc.vatsignupfrontend.httpparsers.SubscriptionRequestSummaryHttpParser.SubscriptionRequestUnexpectedError
import uk.gov.hmrc.vatsignupfrontend.models._
import uk.gov.hmrc.vatsignupfrontend.services.{AdministrativeDivisionLookupService, GetCompanyNameService, StoreVatNumberService, SubmissionService}
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.check_your_answers_final

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class CheckYourAnswersFinalController @Inject()(storeVatNumberService: StoreVatNumberService,
                                                subscriptionRequestSummary: SubscriptionRequestSummaryConnector,
                                                submissionService: SubmissionService,
                                                getCompanyNameService: GetCompanyNameService,
                                                administrativeDivisionLookupService: AdministrativeDivisionLookupService,
                                                view: check_your_answers_final)
                                               (implicit ec: ExecutionContext,
                                                vcc: VatControllerComponents) extends AuthenticatedController(AdministratorRolePredicate) {
  def show: Action[AnyContent] = Action.async { implicit request =>
    authorised() {

      val optVatNumber = request.session.get(SessionKeys.vatNumberKey).filter(_.nonEmpty)

      optVatNumber match {
        case None =>
          Future.successful(Redirect(routes.CaptureVatNumberController.show()))
        case Some(vatNumber) =>
          subscriptionRequestSummary.getSubscriptionRequest(vatNumber) flatMap {
            case Left(SubscriptionRequestUnexpectedError(status, _)) =>
              throw new InternalServerException(s"Subscription Request summary failed with status = $status")
            case Left(_) => Future.successful(Redirect(routes.CaptureVatNumberController.show()))
            case Right(summary) => {
              summary.optCompanyNumber match {
                case Some(companyNumber) =>
                  getCompanyNameService.getCompanyName(companyNumber) map {
                    case Left(_) => throw new InternalServerException(s"Get Company Name Service failed when retrieving data for the CYA final")
                    case Right(CompanyClosed(_)) => throw new InternalServerException(s"Get company name service should not return dissolved")
                    case Right(CompanyDetails(companyName, _)) =>
                      Ok(view(
                        summary.vatNumber,
                        summary.businessEntity,
                        summary.optNino,
                        summary.optSautr,
                        summary.optCompanyNumber,
                        Some(companyName),
                        summary.transactionEmail,
                        summary.contactPreference,
                        routes.CheckYourAnswersFinalController.submit(),
                        isAdministrativeDivision = administrativeDivisionLookupService.isAdministrativeDivision(vatNumber)
                      ))
                  }
                case None =>
                  Future.successful(Ok(view(
                    summary.vatNumber,
                    summary.businessEntity,
                    summary.optNino,
                    summary.optSautr,
                    None,
                    None,
                    summary.transactionEmail,
                    summary.contactPreference,
                    routes.CheckYourAnswersFinalController.submit(),
                    isAdministrativeDivision = administrativeDivisionLookupService.isAdministrativeDivision(vatNumber)
                  )))
              }
            }
          }
      }
    }
  }

  private def captureCompanyNumber(businessEntity: BusinessEntity)(implicit request: Request[AnyContent]) = {
    businessEntity match {
      case LimitedCompany => Redirect(routes.CaptureCompanyNumberController.show())
      case LimitedPartnership => Redirect(partnerships.routes.CapturePartnershipCompanyNumberController.show())
      case RegisteredSociety => Redirect(routes.CaptureRegisteredSocietyCompanyNumberController.show())
    }
  }

  def submit: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      val optVatNumber = request.session.get(SessionKeys.vatNumberKey).filter(_.nonEmpty)
      val acceptedDirectDebitTerms = request.session.get(SessionKeys.acceptedDirectDebitTermsKey).getOrElse("false").toBoolean
      val hasDirectDebit = request.session.get(SessionKeys.hasDirectDebitKey).getOrElse("false").toBoolean

      def submit(vatNumber: String): Future[Result] = {
        submissionService.submit(vatNumber).map {
          case Right(_) =>
            Redirect(routes.InformationReceivedController.show())
          case Left(SubmissionFailureResponse(status)) =>
            throw new InternalServerException(s"Submission failed, backend returned: $status")
        }
      }

      (optVatNumber, hasDirectDebit) match {
        case (Some(vatNumber), true) if acceptedDirectDebitTerms =>
          submit(vatNumber)
        case (Some(vatNumber), false) =>
          submit(vatNumber)
        case (Some(_), true) =>
          Future.successful(
            Redirect(routes.DirectDebitTermsAndConditionsController.show())
          )
        case _ =>
          Future.successful(
            Redirect(routes.ResolveVatNumberController.resolve())
          )
      }
    }
  }
}
