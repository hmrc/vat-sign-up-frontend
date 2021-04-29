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

//$COVERAGE-OFF$Disabling scoverage

package uk.gov.hmrc.vatsignupfrontend.testonly.controllers

import javax.inject.{Inject, Singleton}
import play.api.mvc.{Action, AnyContent}
import uk.gov.hmrc.vatsignupfrontend.config.VatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.controllers.AuthenticatedController
import uk.gov.hmrc.vatsignupfrontend.testonly.forms.StubIssuerRequestForm._
import uk.gov.hmrc.vatsignupfrontend.testonly.models.StubIssuerRequest
import uk.gov.hmrc.vatsignupfrontend.testonly.services.StubIssuerService
import uk.gov.hmrc.vatsignupfrontend.testonly.views.html.stub_issuer

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class StubIssuerController @Inject()(stubIssuerService: StubIssuerService,
                                     view: stub_issuer)
                                    (implicit ec: ExecutionContext,
                                     vcc: VatControllerComponents)
  extends AuthenticatedController() {

  val show: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      Future.successful(
        Ok(view(stubIssuerForm.fill(StubIssuerRequest(
          vatNumber = "",
          isSuccessful = true,
          errorMessage = None
        )), routes.StubIssuerController.submit()))
      )
    }
  }


  val submit: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      stubIssuerForm.bindFromRequest.fold(
        formWithErrors =>
          Future.successful(
            BadRequest(view(formWithErrors, routes.StubIssuerController.submit()))
          ),
        stubIssuerRequest =>
          stubIssuerService.callIssuer(stubIssuerRequest).map {
            case Right(_) => Ok("success")
            case Left(err) => BadGateway(err.toString)
          }
      )
    }
  }

}

// $COVERAGE-ON$
