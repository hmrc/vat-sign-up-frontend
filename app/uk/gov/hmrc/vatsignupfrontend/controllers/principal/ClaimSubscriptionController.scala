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
import uk.gov.hmrc.vatsignupfrontend.SessionKeys.vatNumberKey
import uk.gov.hmrc.vatsignupfrontend.config.ControllerComponents
import uk.gov.hmrc.vatsignupfrontend.config.auth.AdministratorRolePredicate
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch.BTAClaimSubscription
import uk.gov.hmrc.vatsignupfrontend.controllers.AuthenticatedController
import uk.gov.hmrc.vatsignupfrontend.controllers.principal.bta.{routes => btaRoutes}
import uk.gov.hmrc.vatsignupfrontend.httpparsers.ClaimSubscriptionHttpParser.{AlreadyEnrolledOnDifferentCredential, SubscriptionClaimed}
import uk.gov.hmrc.vatsignupfrontend.services.ClaimSubscriptionService
import uk.gov.hmrc.vatsignupfrontend.utils.EnrolmentUtils._
import uk.gov.hmrc.vatsignupfrontend.utils.VatNumberChecksumValidation.isValidChecksum

import scala.concurrent.Future

@Singleton
class ClaimSubscriptionController @Inject()(val controllerComponents: ControllerComponents,
                                            claimSubscriptionService: ClaimSubscriptionService)
  extends AuthenticatedController(AdministratorRolePredicate, featureSwitches = Set(BTAClaimSubscription)) {

  def show(btaVatNumber: String): Action[AnyContent] = Action.async { implicit request =>
    authorised()(Retrievals.allEnrolments) {
      enrolments =>
        enrolments.vatNumber match {
          case Some(enrolmentVatNumber) if enrolmentVatNumber == btaVatNumber =>
            claimSubscriptionService.claimSubscription(enrolmentVatNumber, isFromBta = true) map {
              case Right(SubscriptionClaimed) =>
                Redirect(appConfig.btaRedirectUrl)
              case Left(AlreadyEnrolledOnDifferentCredential) =>
                Redirect(btaRoutes.BusinessAlreadySignedUpController.show())
              case subscriptionNotClaimedReason =>
                throw new InternalServerException(s"Claim subscription was not successful, result was $subscriptionNotClaimedReason")
            }
          case Some(_) =>
            Future.failed(
              new InternalServerException("Supplied VAT number did not match enrolment")
            )
          case None if btaVatNumber.length == 9 && isValidChecksum(btaVatNumber) =>
            Future.successful(
              Redirect(btaRoutes.CaptureBtaVatRegistrationDateController.show()) addingToSession(vatNumberKey -> btaVatNumber)
            )
          case None =>
            Future.failed(
              new InternalServerException("Supplied VAT number was invalid")
            )
        }
    }
  }
}
