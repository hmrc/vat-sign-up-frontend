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

package uk.gov.hmrc.vatsignupfrontend.controllers

import javax.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.i18n.Lang
import play.api.mvc.MessagesControllerComponents
import uk.gov.hmrc.play.language.{LanguageController, LanguageUtils}
import uk.gov.hmrc.vatsignupfrontend.config.AppConfig

@Singleton
class LanguageSwitchController @Inject()(appConfig: AppConfig,
                                         configuration: Configuration,
                                         mcc: MessagesControllerComponents,
                                         languageUtils: LanguageUtils
                                        ) extends LanguageController(configuration, languageUtils, mcc) {

  def languageMap: Map[String, Lang] = appConfig.languageMap

  protected[controllers] def fallbackURL: String = principal.routes.IndexResolverController.resolve().url

}
