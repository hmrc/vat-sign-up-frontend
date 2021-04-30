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
import uk.gov.hmrc.http.{HeaderCarrier, InternalServerException}
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.VatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.config.auth.AgentEnrolmentPredicate
import uk.gov.hmrc.vatsignupfrontend.connectors.SubscriptionRequestSummaryConnector
import uk.gov.hmrc.vatsignupfrontend.controllers.AuthenticatedController
import uk.gov.hmrc.vatsignupfrontend.httpparsers.GetCompanyNameHttpParser.{CompanyClosed, CompanyDetails}
import uk.gov.hmrc.vatsignupfrontend.httpparsers.SubmissionHttpParser.SubmissionFailureResponse
import uk.gov.hmrc.vatsignupfrontend.httpparsers.SubscriptionRequestSummaryHttpParser.SubscriptionRequestUnexpectedError
import uk.gov.hmrc.vatsignupfrontend.models.{Division, Overseas}
import uk.gov.hmrc.vatsignupfrontend.services.{GetCompanyNameService, StoreVatNumberService, SubmissionService}
import uk.gov.hmrc.vatsignupfrontend.views.html.agent.check_your_answers_final

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class CheckYourAnswersFinalController @Inject()(storeVatNumberService: StoreVatNumberService,
                                                subscriptionRequestSummary: SubscriptionRequestSummaryConnector,
                                                submissionService: SubmissionService,
                                                getCompanyNameService: GetCompanyNameService,
                                                view: check_your_answers_final)
                                               (implicit ec: ExecutionContext,
                                                vcc: VatControllerComponents)
  extends AuthenticatedController(AgentEnrolmentPredicate) {

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
            case Right(summary) =>
              optCompanyNameFromOptCompanyNumber(summary.optCompanyNumber).map { optCompanyName =>
                val optBusinessEntity = summary.businessEntity match {
                  case Overseas => None
                  case Division => None
                  case _ => Some(summary.businessEntity)
                }

                Ok(view(
                  subSummary = summary,
                  optBusinessEntity = optBusinessEntity,
                  optCompanyName = optCompanyName,
                  postAction = routes.CheckYourAnswersFinalController.submit()
                ))
              }
          }
      }
    }
  }

  private def optCompanyNameFromOptCompanyNumber(optCompanyNumber: Option[String])(implicit hc: HeaderCarrier): Future[Option[String]] = {
    optCompanyNumber match {
      case Some(companyNumber) =>
        getCompanyNameService.getCompanyName(companyNumber).map {
          case Right(CompanyDetails(companyName, _)) =>
            Some(companyName)
          case Right(CompanyClosed(_)) =>
            throw new InternalServerException("Get Company Name Service returned that the company is converted-closed or dissolved")
          case Left(_) =>
            throw new InternalServerException("Get Company Name Service failed when retrieving data for the CYA final")
        }
      case None =>
        Future.successful(None)
    }
  }

  def submit: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      submissionService.submit(request.session(SessionKeys.vatNumberKey)).map {
        case Right(_) => Redirect(routes.ConfirmationController.show())
        case Left(SubmissionFailureResponse(status)) => throw new InternalServerException(s"Submission failed, backend returned: $status")
      }
    }
  }
}
