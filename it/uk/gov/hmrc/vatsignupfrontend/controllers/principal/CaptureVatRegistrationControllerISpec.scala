/*
 * Copyright 2018 HM Revenue & Customs
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

import java.time.LocalDate

import play.api.http.Status._
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.SessionKeys.{businessEntityKey, vatRegistrationDateKey}
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch.AdditionalKnownFacts
import uk.gov.hmrc.vatsignupfrontend.forms.VatRegistrationDateForm._
import uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks.AuthStub._
import uk.gov.hmrc.vatsignupfrontend.helpers.{ComponentSpecBase, CustomMatchers, SessionCookieCrumbler}
import uk.gov.hmrc.vatsignupfrontend.models.{DateModel, Overseas}

class CaptureVatRegistrationControllerISpec extends ComponentSpecBase with CustomMatchers {


  "GET /vat-registration-date" should {
    "return an OK" in {
      stubAuth(OK, successfulAuthResponse())

      val res = get("/vat-registration-date")

      res should have(
        httpStatus(OK)
      )
    }
  }

  "POST /vat-registration-date" should {
    "user is not overseas" when {
      "redirects to Capture Business Postcode page" in {
        stubAuth(OK, successfulAuthResponse())

        val yesterday = DateModel.dateConvert(LocalDate.now().minusDays(1))

        val res = post("/vat-registration-date")(vatRegistrationDate + ".dateDay" -> yesterday.day,
          vatRegistrationDate + ".dateMonth" -> yesterday.month,
          vatRegistrationDate + ".dateYear" -> yesterday.year
        )

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(routes.BusinessPostCodeController.show().url)
        )

        val session = SessionCookieCrumbler.getSessionMap(res)
        session.keys should contain(vatRegistrationDateKey)
      }
    }
    "user is overseas and the additional known facts feature switch is enabled" when {
      "redirects to Previous VAT return page" in {
        stubAuth(OK, successfulAuthResponse())
        enable(AdditionalKnownFacts)

        val yesterday = DateModel.dateConvert(LocalDate.now().minusDays(1))

        val res = post(
          "/vat-registration-date",
          Map(SessionKeys.businessEntityKey -> Overseas.toString))(
            vatRegistrationDate + ".dateDay" -> yesterday.day,
            vatRegistrationDate + ".dateMonth" -> yesterday.month,
            vatRegistrationDate + ".dateYear" -> yesterday.year
        )

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(routes.PreviousVatReturnController.show().url)
        )

        val session = SessionCookieCrumbler.getSessionMap(res)
        session.keys should contain(vatRegistrationDateKey)
        session.keys should contain(businessEntityKey)
      }
    }
    "user is overseas and the additional known facts feature switch is not enabled" when {
      "redirects to Check Your Answers page" in {
        stubAuth(OK, successfulAuthResponse())
        disable(AdditionalKnownFacts)

        val yesterday = DateModel.dateConvert(LocalDate.now().minusDays(1))

        val res = post(
          "/vat-registration-date",
          Map(SessionKeys.businessEntityKey -> Overseas.toString))(
          vatRegistrationDate + ".dateDay" -> yesterday.day,
          vatRegistrationDate + ".dateMonth" -> yesterday.month,
          vatRegistrationDate + ".dateYear" -> yesterday.year
        )

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(routes.CheckYourAnswersController.show().url)
        )

        val session = SessionCookieCrumbler.getSessionMap(res)
        session.keys should contain(vatRegistrationDateKey)
        session.keys should contain(businessEntityKey)
      }
    }
  }

}
