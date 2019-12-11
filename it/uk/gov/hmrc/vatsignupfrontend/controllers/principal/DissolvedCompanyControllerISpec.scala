/*
 * Copyright 2019 HM Revenue & Customs
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

import play.api.http.Status.{OK, SEE_OTHER}
import uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks.AuthStub.{stubAuth, successfulAuthResponse}
import uk.gov.hmrc.vatsignupfrontend.helpers.{ComponentSpecBase, CustomMatchers}

class DissolvedCompanyControllerISpec extends ComponentSpecBase with CustomMatchers {

  "GET /dissolved-company" should {
    "return an OK" in {
      stubAuth(OK, successfulAuthResponse())

      val res = get("/dissolved-company")

      res should have(
        httpStatus(OK)
      )
    }
  }

  "POST /dissolved-company" should {
    "return a SEE OTHER with a redirect to capture company number" in {
      stubAuth(OK, successfulAuthResponse())

      val res = post("/dissolved-company")()

      res should have(
        httpStatus(SEE_OTHER),
        redirectUri(routes.CaptureCompanyNumberController.show().url)
      )
    }
  }

}
