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

package uk.gov.hmrc.vatsignupfrontend.controllers.principal.error

import javax.inject.{Inject, Singleton}
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent}
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import uk.gov.hmrc.vatsignupfrontend.config.{AppConfig, VatControllerComponents}
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.choose_software_error

@Singleton
class ChooseSoftwareErrorController @Inject()(implicit vcc: VatControllerComponents) extends FrontendController(vcc.controllerComponents) with I18nSupport {

  implicit val appConfig: AppConfig = vcc.appConfig

  val show: Action[AnyContent] = Action { implicit request =>
    Ok(choose_software_error(routes.ChooseSoftwareErrorController.submit()))
  }

  val submit: Action[AnyContent] = Action { implicit request =>
    Redirect(appConfig.guidancePageUrl)
  }

}