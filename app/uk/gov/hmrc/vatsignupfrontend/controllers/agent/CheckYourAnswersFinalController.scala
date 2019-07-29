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

package uk.gov.hmrc.vatsignupfrontend.controllers.agent

import javax.inject.{Inject, Singleton}
import play.api.mvc.{Action, AnyContent}
import uk.gov.hmrc.http.{HeaderCarrier, InternalServerException}
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.ControllerComponents
import uk.gov.hmrc.vatsignupfrontend.config.auth.AgentEnrolmentPredicate
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch.{FinalCheckYourAnswer, SkipCidCheck}
import uk.gov.hmrc.vatsignupfrontend.connectors.SubscriptionRequestSummaryConnector
import uk.gov.hmrc.vatsignupfrontend.controllers.AuthenticatedController
import uk.gov.hmrc.vatsignupfrontend.httpparsers.GetCompanyNameHttpParser.GetCompanyNameSuccess
import uk.gov.hmrc.vatsignupfrontend.httpparsers.SubmissionHttpParser.SubmissionFailureResponse
import uk.gov.hmrc.vatsignupfrontend.httpparsers.SubscriptionRequestSummaryHttpParser.SubscriptionRequestUnexpectedError
import uk.gov.hmrc.vatsignupfrontend.services.{GetCompanyNameService, StoreVatNumberService, SubmissionService}
import uk.gov.hmrc.vatsignupfrontend.views.html.agent.check_your_answers_final

import scala.concurrent.Future

@Singleton
class CheckYourAnswersFinalController @Inject()(val controllerComponents: ControllerComponents,
                                                val storeVatNumberService: StoreVatNumberService,
                                                val subscriptionRequestSummary: SubscriptionRequestSummaryConnector,
                                                val submissionService: SubmissionService,
                                                val getCompanyNameService: GetCompanyNameService
                                               ) extends AuthenticatedController(AgentEnrolmentPredicate, featureSwitches = Set(FinalCheckYourAnswer)) {

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
                Ok(check_your_answers_final(
                  summary.vatNumber,
                  summary.businessEntity,
                  summary.optNino,
                  summary.optCompanyNumber,
                  optCompanyName,
                  summary.optSautr,
                  summary.transactionEmail,
                  summary.optSignUpEmail,
                  summary.contactPreference,
                  isEnabled(SkipCidCheck),
                  routes.CheckYourAnswersFinalController.submit()
                ))
              }
          }
      }
    }
  }

  private def optCompanyNameFromOptCompanyNumber(optCompanyNumber: Option[String])(implicit hc: HeaderCarrier): Future[Option[String]] = {
    optCompanyNumber.map { companyNumber =>
      getCompanyNameService.getCompanyName(companyNumber).map {
        case Left(_) => throw new InternalServerException(s"Get Company Name Service failed when retrieving data for the CYA final")
        case Right(GetCompanyNameSuccess(companyName, _)) => companyName
      }
    } match {
      case Some(future) => future.map(Some(_))
      case None => Future.successful(None)
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