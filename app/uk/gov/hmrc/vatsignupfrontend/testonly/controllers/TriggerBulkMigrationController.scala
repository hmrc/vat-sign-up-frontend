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
import uk.gov.hmrc.vatsignupfrontend.forms.VatNumberForm._
import uk.gov.hmrc.vatsignupfrontend.testonly.connectors.TriggerBulkMigrationConnector
import uk.gov.hmrc.vatsignupfrontend.testonly.views.html.trigger_bulk_migration

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class TriggerBulkMigrationController @Inject()(triggerBulkMigrationConnector: TriggerBulkMigrationConnector,
                                               view: trigger_bulk_migration)
                                              (implicit ec: ExecutionContext,
                                               vcc: VatControllerComponents)
  extends AuthenticatedController() {

  val show: Action[AnyContent] = Action { implicit request =>
    Ok(view(
      vatNumberForm(false).form,
      routes.TriggerBulkMigrationController.submit())
    )
  }

  val submit: Action[AnyContent] = Action.async { implicit request =>
    vatNumberForm(isAgent = false).bindFromRequest.fold(
      formWithErrors =>
        Future.successful(
          BadRequest(view(formWithErrors, routes.TriggerBulkMigrationController.submit()))
        ),
      vatNumber =>
        triggerBulkMigrationConnector.triggerBulkMigration(vatNumber).map {
          _ => Ok("success")
        }
    )
  }
}

// $COVERAGE-ON$
