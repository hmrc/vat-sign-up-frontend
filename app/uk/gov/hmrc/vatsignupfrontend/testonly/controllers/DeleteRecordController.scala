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
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent}
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import uk.gov.hmrc.vatsignupfrontend.config.{AppConfig, VatControllerComponents}
import uk.gov.hmrc.vatsignupfrontend.forms.VatNumberForm._
import uk.gov.hmrc.vatsignupfrontend.testonly.connectors.DeleteRecordConnector
import uk.gov.hmrc.vatsignupfrontend.testonly.models.DeleteRecordFailure
import uk.gov.hmrc.vatsignupfrontend.testonly.views.html.delete_record

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class DeleteRecordController @Inject()(deleteRecordConnector: DeleteRecordConnector,
                                       vcc: VatControllerComponents,
                                       view: delete_record)
                                      (implicit ec: ExecutionContext)
  extends FrontendController(vcc.controllerComponents) with I18nSupport {

  private val validateVatNumberForm = vatNumberForm(isAgent = false)

  implicit val appConfig: AppConfig = vcc.appConfig

  val show: Action[AnyContent] = Action.async { implicit request =>
    Future.successful(
      Ok(view(validateVatNumberForm.form, routes.DeleteRecordController.submit()))
    )
  }

  val submit: Action[AnyContent] = Action.async { implicit request =>
    validateVatNumberForm.bindFromRequest.fold(
      formWithErrors =>
        Future.successful(
          BadRequest(view(formWithErrors, routes.DeleteRecordController.submit()))
        ),
      vatNumber =>
        deleteRecordConnector.deleteRecord(vatNumber).map {
          case Right(_) => Ok("successful")
          case Left(DeleteRecordFailure(status)) => Status(status)(s"failed: status=$status")
        }.recover {
          case e => Ok("failed: " + e.getMessage)
        }
    )
  }

}

// $COVERAGE-ON$
