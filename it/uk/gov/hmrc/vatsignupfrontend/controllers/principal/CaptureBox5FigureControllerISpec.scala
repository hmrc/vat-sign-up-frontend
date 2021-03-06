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

import play.api.http.Status._
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch.FeatureSwitching
import uk.gov.hmrc.vatsignupfrontend.forms.Box5FigureForm
import uk.gov.hmrc.vatsignupfrontend.helpers.IntegrationTestConstants._
import uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks.AuthStub._
import uk.gov.hmrc.vatsignupfrontend.helpers.{ComponentSpecBase, CustomMatchers}

class CaptureBox5FigureControllerISpec extends ComponentSpecBase with CustomMatchers with FeatureSwitching {


  "GET /box-5-figure" should {
      "return an OK" in {
        stubAuth(OK, successfulAuthResponse())

        val res = get("/box-5-figure")

        res should have(
          httpStatus(OK)
        )
      }
  }

  "POST /box-5-figure" should {
      "redirect to CaptureLastMonthReturnPeriod Page" in {
        stubAuth(OK, successfulAuthResponse())

        val res = post("/box-5-figure")(Box5FigureForm.box5Figure -> testBox5Figure)

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(routes.CaptureLastReturnMonthPeriodController.show().url)
        )
    }
  }
}
