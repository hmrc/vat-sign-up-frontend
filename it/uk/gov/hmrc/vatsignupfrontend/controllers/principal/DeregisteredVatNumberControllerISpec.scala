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

import play.api.http.Status.OK
import uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks.AuthStub.{stubAuth, successfulAuthResponse, vatDecEnrolment, mtdVatEnrolment}
import uk.gov.hmrc.vatsignupfrontend.helpers.{ComponentSpecBase, CustomMatchers}

class DeregisteredVatNumberControllerISpec extends ComponentSpecBase with CustomMatchers {

  "GET /error/deregistered-vat-number" when {
    "a user with an MTD VAT enrolment enters a deregistered VRN and is redirected to the deregistered page" should {
      "return an OK" in {
        stubAuth(OK, successfulAuthResponse(mtdVatEnrolment))

        val result = get("/error/deregistered-vat-number")

        result should have(
          httpStatus(OK)
        )
      }
    }

    "a user with a legacy enrolment enters a deregistered VRN and is redirected to the deregistered page" should {
      "return an OK" in {
        stubAuth(OK, successfulAuthResponse(vatDecEnrolment))

        val result = get("/error/deregistered-vat-number")

        result should have(
          httpStatus(OK)
        )
      }
    }

    "a user with no enrolment enters a deregistered VRN and is redirected to the deregistered page" should {
      "return an OK" in {
        stubAuth(OK, successfulAuthResponse())

        val result = get("/error/deregistered-vat-number")

        result should have(
          httpStatus(OK)
        )
      }
    }

  }
}
