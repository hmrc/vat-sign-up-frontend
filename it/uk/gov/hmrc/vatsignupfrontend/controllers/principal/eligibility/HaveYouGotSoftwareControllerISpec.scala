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

package uk.gov.hmrc.vatsignupfrontend.controllers.principal.eligibility

import play.api.http.Status._
import uk.gov.hmrc.vatsignupfrontend.forms.submapping.HaveSoftwareMapping
import uk.gov.hmrc.vatsignupfrontend.helpers.{ComponentSpecBase, CustomMatchers}
import uk.gov.hmrc.vatsignupfrontend.models.{AccountingSoftware, HaveSoftware, Neither, Spreadsheets}

class HaveYouGotSoftwareControllerISpec extends ComponentSpecBase with CustomMatchers {

  val uri = "/interruption/have-you-got-software"

  private def formPost(answer: HaveSoftware): (String, String) =
    HaveSoftwareMapping unbind("software", answer) head

  "GET /interruption/have-you-got-software" should {
    "render the 'Have you got software' page" in {
      val result = get(uri)

      result should have(
        httpStatus(OK)
      )
    }
  }

  "POST /interruption/have-you-got-software" when {
    "nothing has been selected" should {
      "return BAD_REQUEST and render the form" in {
        val result = post(uri)()

        result should have(
          httpStatus(BAD_REQUEST)
        )
      }
    }
    "the answer is 'I use accounting software'" should {
      "redirect to the 'Got software' page" in {
        val result = post(uri)(formPost(AccountingSoftware))

        result should have(
          httpStatus(SEE_OTHER),
          redirectUri(routes.GotSoftwareController.show().url)
        )
      }
    }
    "the answer is 'I use spreadsheets'" should {
      "redirect to the 'Use spreadsheets page" in {
        val result = post(uri)(formPost(Spreadsheets))

        result should have(
          httpStatus(SEE_OTHER),
          redirectUri(routes.UseSpreadsheetsController.show().url)
        )
      }
    }
    "the answer is 'I use neither'" should {
      "redirect to the 'Not got software' page" in {
        val result = post(uri)(formPost(Neither))

        result should have(
          httpStatus(SEE_OTHER),
          redirectUri(routes.NotGotSoftwareController.show().url)
        )
      }
    }
  }

}
