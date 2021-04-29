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
import play.api.mvc.{Action, AnyContent, Result}
import uk.gov.hmrc.auth.core.retrieve.Retrievals
import uk.gov.hmrc.http.InternalServerException
import uk.gov.hmrc.vatsignupfrontend.SessionKeys._
import uk.gov.hmrc.vatsignupfrontend.config.VatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.config.auth.AdministratorRolePredicate
import uk.gov.hmrc.vatsignupfrontend.controllers.AuthenticatedController
import uk.gov.hmrc.vatsignupfrontend.httpparsers.CtReferenceLookupHttpParser.{CtReferenceIsFound, CtReferenceLookupFailureResponse, CtReferenceNotFound}
import uk.gov.hmrc.vatsignupfrontend.httpparsers.StoreRegisteredSocietyHttpParser.CtReferenceMismatch
import uk.gov.hmrc.vatsignupfrontend.services.{CtReferenceLookupService, StoreRegisteredSocietyService}
import uk.gov.hmrc.vatsignupfrontend.utils.EnrolmentUtils._
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.confirm_registered_society

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class ConfirmRegisteredSocietyController @Inject()(storeRegisteredSocietyService: StoreRegisteredSocietyService,
                                                   ctReferenceLookupService: CtReferenceLookupService,
                                                   view: confirm_registered_society)
                                                  (implicit ec: ExecutionContext,
                                                   vcc: VatControllerComponents)
  extends AuthenticatedController(AdministratorRolePredicate) {

  val show: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      val optRegisteredSocietyName = request.session.get(registeredSocietyNameKey).filter(_.nonEmpty)
      Future.successful(
        optRegisteredSocietyName match {
          case Some(registeredSocietyName) =>
            val changeLink = routes.CaptureRegisteredSocietyCompanyNumberController.show().url
            Ok(view(
              registeredSocietyName = registeredSocietyName,
              postAction = routes.ConfirmRegisteredSocietyController.submit(),
              changeLink = changeLink
            ))
          case _ =>
            Redirect(routes.CaptureRegisteredSocietyCompanyNumberController.show())
        }
      )
    }
  }

  val submit: Action[AnyContent] = Action.async { implicit request =>
    authorised()(Retrievals.allEnrolments) {
      enrolments => {
        val optVatNumber = request.session.get(vatNumberKey).filter(_.nonEmpty)
        val optCompanyNumber = request.session.get(registeredSocietyCompanyNumberKey).filter(_.nonEmpty)
        val optCtReference = enrolments.companyUtr

        def storeRegisteredSociety(vatNumber: String, companyNumber: String, companyUtr: Option[String]): Future[Result] = {
          storeRegisteredSocietyService.storeRegisteredSociety(vatNumber, companyNumber, companyUtr) flatMap {
            case Right(_) =>
              Future.successful(Redirect(routes.DirectDebitResolverController.show()))
            case Left(CtReferenceMismatch) =>
              Future.successful(Redirect(routes.CaptureRegisteredSocietyUtrController.show()))
            case Left(status) =>
              throw new InternalServerException("StoreRegisteredSociety failed: status = " + status)
          }
        }

        (optCompanyNumber, optVatNumber) match {
          case (Some(companyNumber), Some(vatNumber)) =>
            ctReferenceLookupService.checkCtReferenceExists(companyNumber) flatMap {
              case Right(CtReferenceIsFound) =>
                optCtReference match {
                  case Some(_) =>
                    storeRegisteredSociety(vatNumber, companyNumber, optCtReference)
                  case None =>
                    Future.successful(Redirect(routes.CaptureRegisteredSocietyUtrController.show()))
                }
              case Left(CtReferenceNotFound) =>
                storeRegisteredSociety(vatNumber, companyNumber, None)
              case Left(CtReferenceLookupFailureResponse(status)) =>
                throw new InternalServerException("CtReferenceLookup failed: status = " + status)
            }
          case (_, None) =>
            Future.successful(Redirect(routes.ResolveVatNumberController.resolve()))
          case (None, _) =>
            Future.successful(Redirect(routes.CaptureRegisteredSocietyCompanyNumberController.show()))
        }
      }
    }
  }
}
