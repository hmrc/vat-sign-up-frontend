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

package uk.gov.hmrc.vatsignupfrontend.testonly.controllers

import javax.inject.{Inject, Singleton}
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Request}
import play.twirl.api.Html
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import uk.gov.hmrc.vatsignupfrontend.config.AppConfig
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch.FeatureSwitch.switches
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch.{FeatureSwitch, FeatureSwitching}
import uk.gov.hmrc.vatsignupfrontend.testonly.connectors.BackendFeatureSwitchConnector
import uk.gov.hmrc.vatsignupfrontend.testonly.controllers.routes.FeatureSwitchController
import uk.gov.hmrc.vatsignupfrontend.testonly.models.FeatureSwitchSetting
import uk.gov.hmrc.vatsignupfrontend.testonly.views.html.feature_switch

import scala.collection.immutable.ListMap
import scala.concurrent.ExecutionContext

@Singleton
class FeatureSwitchController @Inject()(view: feature_switch)
                                       (val appConfig: AppConfig,
                                        mcc: MessagesControllerComponents,
                                        featureSwitchConnector: BackendFeatureSwitchConnector)
                                       (implicit ec: ExecutionContext)
  extends FrontendController(mcc) with FeatureSwitching with I18nSupport {

  implicit val config: AppConfig = appConfig

  private def view(switchNames: Map[FeatureSwitch, Boolean], backendFeatureSwitches: Map[String, Boolean])(implicit request: Request[_]): Html =
    view(
      switchNames = switchNames,
      backendFeatureSwitches = backendFeatureSwitches,
      FeatureSwitchController.submit()
    )

  def show: Action[AnyContent] = Action.async { implicit req =>
    for {
      backendFeatureSwitches <- featureSwitchConnector.getBackendFeatureSwitches
      featureSwitches = ListMap(switches.toSeq sortBy (_.displayText) map (switch => switch -> isEnabled(switch)): _*)
    } yield Ok(view(featureSwitches, backendFeatureSwitches))
  }

  def submit: Action[AnyContent] = Action.async { implicit req =>
    val submittedData: Set[String] = req.body.asFormUrlEncoded match {
      case None => Set.empty
      case Some(data) => data.keySet
    }

    val frontendFeatureSwitches = submittedData flatMap FeatureSwitch.get

    switches.foreach(fs =>
      if (frontendFeatureSwitches.contains(fs)) enable(fs)
      else disable(fs)
    )

    featureSwitchConnector.getBackendFeatureSwitches map {
      _.keySet map { switchName =>
        FeatureSwitchSetting(
          feature = switchName,
          enable = submittedData contains switchName
        )
      }
    } flatMap featureSwitchConnector.submitBackendFeatureSwitches map {
      _ => Redirect(FeatureSwitchController.show())
    }
  }

}
