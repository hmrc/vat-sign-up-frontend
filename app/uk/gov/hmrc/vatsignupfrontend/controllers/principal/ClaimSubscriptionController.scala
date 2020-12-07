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
import uk.gov.hmrc.auth.core.retrieve.Retrievals
import uk.gov.hmrc.http.InternalServerException
import uk.gov.hmrc.vatsignupfrontend.SessionKeys.{businessEntityKey, isAlreadySubscribedKey, isFromBtaKey, vatNumberKey}
import uk.gov.hmrc.vatsignupfrontend.config.VatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.config.auth.AdministratorRolePredicate
import uk.gov.hmrc.vatsignupfrontend.controllers.AuthenticatedController
import uk.gov.hmrc.vatsignupfrontend.controllers.principal.error.{routes => errorRoutes}
import uk.gov.hmrc.vatsignupfrontend.httpparsers.ClaimSubscriptionHttpParser.{AlreadyEnrolledOnDifferentCredential, InvalidVatNumber, SubscriptionClaimed}
import uk.gov.hmrc.vatsignupfrontend.models.Overseas
import uk.gov.hmrc.vatsignupfrontend.services.{CheckVatNumberEligibilityService, ClaimSubscriptionService}
import uk.gov.hmrc.vatsignupfrontend.utils.EnrolmentUtils._
import uk.gov.hmrc.vatsignupfrontend.utils.SessionUtils._
import uk.gov.hmrc.vatsignupfrontend.utils.VatNumberChecksumValidation.isValidChecksum

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class ClaimSubscriptionController @Inject()(claimSubscriptionService: ClaimSubscriptionService,
                                            checkVatNumberEligibilityService: CheckVatNumberEligibilityService)
                                           (implicit ec: ExecutionContext,
                                            vcc: VatControllerComponents)
  extends AuthenticatedController(AdministratorRolePredicate) {

  // scalastyle:off
  def show(btaVatNumber: String): Action[AnyContent] = Action.async { implicit request =>
    authorised()(Retrievals.allEnrolments) {
      enrolments =>
        if (enrolments.mtdVatNumber.isDefined) Future.successful(Redirect(errorRoutes.AlreadySignedUpController.show()))
        else enrolments.vatNumber match {
          case Some(enrolmentVatNumber) if enrolmentVatNumber == btaVatNumber =>
            claimSubscriptionService.claimSubscription(enrolmentVatNumber, isFromBta = true) map {
              case Right(SubscriptionClaimed) =>
                Redirect(appConfig.btaRedirectUrl)
              case Left(AlreadyEnrolledOnDifferentCredential) =>
                Redirect(errorRoutes.BusinessAlreadySignedUpController.show())
              case Left(InvalidVatNumber(message)) =>
                throw new InternalServerException(s"Failed to claim a subscription for $btaVatNumber as backend returned $message")
              case subscriptionNotClaimedReason =>
                throw new InternalServerException(s"Claim subscription was not successful, result was $subscriptionNotClaimedReason")
            }
          case Some(_) =>
            Future.failed(
              new InternalServerException("Supplied VAT number did not match enrolment")
            )
          case None if btaVatNumber.length == 9 && isValidChecksum(btaVatNumber) =>
            checkVatNumberEligibilityService.isOverseas(btaVatNumber).map { isOverseas =>
              if (isOverseas)
                Redirect(routes.CaptureVatRegistrationDateController.show())
                  .addingToSession(vatNumberKey -> btaVatNumber)
                  .addingToSession(isAlreadySubscribedKey, true)
                  .addingToSession(isFromBtaKey, true)
                  .addingToSession(businessEntityKey -> Overseas.toString)
              else
                Redirect(routes.CaptureVatRegistrationDateController.show())
                  .addingToSession(vatNumberKey -> btaVatNumber)
                  .addingToSession(isAlreadySubscribedKey, true)
                  .addingToSession(isFromBtaKey, true)
            }
          case None =>
            Future.failed(
              new InternalServerException("Supplied VAT number was invalid")
            )
        }
    }
  }
}
