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
import play.api.libs.json.Json
import uk.gov.hmrc.vatsignupfrontend.SessionKeys._
import uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks.AuthStub._
import uk.gov.hmrc.vatsignupfrontend.helpers.{ComponentSpecBase, CustomMatchers}
import uk.gov.hmrc.vatsignupfrontend.models.MigratableDates


class MigratableDatesControllerISpec extends ComponentSpecBase with CustomMatchers {

  val testDate = LocalDate.now()
  "GET /sign-up-later" should {
    "return a See Other" when {
      "no dates are available" in {
        stubAuth(OK, successfulAuthResponse())
        val res = get("/sign-up-later",
          Map(
            migratableDatesKey -> Json.toJson(MigratableDates()).toString()
          )
        )
        res should have(
          httpStatus(SEE_OTHER)
        )
      }
    }

    "return an OK" when {
      "one date is available" in {
        stubAuth(OK, successfulAuthResponse())
        val res = get("/sign-up-later",
          Map(
            migratableDatesKey -> Json.toJson(MigratableDates(Some(testDate))).toString()
          )
        )
        res should have(
          httpStatus(OK)
        )
      }
    }

    "return an OK" when {
      "two dates are available" in {
        stubAuth(OK, successfulAuthResponse())
        val res = get("/sign-up-later",
          Map(
            migratableDatesKey -> Json.toJson(MigratableDates(Some(testDate), Some(testDate))).toString()
          )
        )
        res should have(
          httpStatus(OK)
        )
      }
    }
  }
}