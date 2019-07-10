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
import play.api.mvc.{Action, AnyContent, Request, Result}
import uk.gov.hmrc.http.InternalServerException
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.ControllerComponents
import uk.gov.hmrc.vatsignupfrontend.config.auth.AdministratorRolePredicate
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch.{DirectDebitTermsJourney, FinalCheckYourAnswer}
import uk.gov.hmrc.vatsignupfrontend.connectors.SubscriptionRequestSummaryConnector
import uk.gov.hmrc.vatsignupfrontend.controllers.AuthenticatedController
import uk.gov.hmrc.vatsignupfrontend.httpparsers.SubmissionHttpParser.SubmissionFailureResponse
import uk.gov.hmrc.vatsignupfrontend.httpparsers.SubscriptionRequestSummaryHttpParser.SubscriptionRequestUnexpectedError
import uk.gov.hmrc.vatsignupfrontend.models._
import uk.gov.hmrc.vatsignupfrontend.services.{StoreVatNumberService, SubmissionService}
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.check_your_answers_final

import scala.concurrent.Future

@Singleton
class CheckYourAnswersFinalController @Inject()(val controllerComponents: ControllerComponents,
                                                val storeVatNumberService: StoreVatNumberService,
                                                val subscriptionRequestSummary: SubscriptionRequestSummaryConnector,
                                                val submissionService: SubmissionService
                                               ) extends AuthenticatedController(AdministratorRolePredicate, featureSwitches = Set(FinalCheckYourAnswer)) {
  def show: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      val optVatNumber = request.session.get(SessionKeys.vatNumberKey).filter(_.nonEmpty)
      optVatNumber match {
        case None =>
          Future.successful(Redirect(routes.CaptureVatNumberController.show()))
        case Some(vatNumber) =>
          subscriptionRequestSummary.getSubscriptionRequest(vatNumber) map {
            case Left(SubscriptionRequestUnexpectedError(status, _)) => throw new InternalServerException(s"Subscription Request summary failed with status = $status")
            case Left(error) => Redirect(routes.CaptureVatNumberController.show())
            case Right(summary) => {
              val optNino = request.session.get(SessionKeys.ninoKey).filter(_.nonEmpty)
              val optPartnershipUtr = request.session.get(SessionKeys.partnershipSautrKey).filter(_.nonEmpty)
              val optCompanyNumber = companyNumberFromRequest(summary.businessEntity)
              val optCompanyName = companyNameFromRequest(summary.businessEntity)

              (summary.businessEntity, optNino, optCompanyNumber, optPartnershipUtr) match {
                case (SoleTrader, None, _, _) =>
                  Redirect(soletrader.routes.CaptureNinoController.show())
                case (LimitedCompany, _, None, _) =>
                  Redirect(routes.CaptureCompanyNumberController.show())
                case (LimitedPartnership, _, None, _) =>
                  Redirect(partnerships.routes.CapturePartnershipCompanyNumberController.show())
                case (RegisteredSociety, _, None, _) =>
                  Redirect(routes.CaptureRegisteredSocietyCompanyNumberController.show())
                case (LimitedPartnership | GeneralPartnership, _, _, None) =>
                  Redirect(partnerships.routes.CapturePartnershipUtrController.show())
                case _ =>
                  Ok(check_your_answers_final(
                    summary.vatNumber,
                    summary.businessEntity,
                    optNino.filter(_ => summary.businessEntity.toString() == BusinessEntity.SoleTraderKey),
                    optPartnershipUtr.filter(_ => summary.businessEntity.toString() == BusinessEntity.LimitedPartnershipKey | summary.businessEntity.toString() == BusinessEntity.GeneralPartnershipKey),
                    optCompanyNumber.filter(_ => summary.businessEntity.toString() == BusinessEntity.LimitedCompanyKey | summary.businessEntity.toString() == BusinessEntity.LimitedPartnershipKey | summary.businessEntity.toString() == BusinessEntity.RegisteredSocietyKey),
                    optCompanyName.filter(_ => summary.businessEntity.toString() == BusinessEntity.LimitedCompanyKey | summary.businessEntity.toString() == BusinessEntity.LimitedPartnershipKey | summary.businessEntity.toString() == BusinessEntity.RegisteredSocietyKey),
                    summary.transactionEmail,
                    summary.contactPreference,
                    routes.CheckYourAnswersFinalController.submit()
                  ))
              }
            }
          }
      }
    }
  }

  private def companyNumberFromRequest(businessEntity: BusinessEntity)(implicit request: Request[AnyContent]) = {
    businessEntity match {
      case RegisteredSociety =>
        request.session.get(SessionKeys.registeredSocietyCompanyNumberKey).filter(_.nonEmpty)
      case _ =>
        request.session.get(SessionKeys.companyNumberKey).filter(_.nonEmpty)
    }
  }

  private def companyNameFromRequest(businessEntity: BusinessEntity)(implicit request: Request[AnyContent]) = {
    businessEntity match {
      case RegisteredSociety =>
        request.session.get(SessionKeys.registeredSocietyNameKey).filter(_.nonEmpty)
      case _ =>
        request.session.get(SessionKeys.companyNameKey).filter(_.nonEmpty)
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
        case (Some(vatNumber), true) if acceptedDirectDebitTerms || !isEnabled(DirectDebitTermsJourney) =>
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
