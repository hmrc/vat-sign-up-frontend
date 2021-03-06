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
import uk.gov.hmrc.vatsignupfrontend.SessionKeys._
import uk.gov.hmrc.vatsignupfrontend.controllers.principal.error.{routes => errorRoutes}
import uk.gov.hmrc.vatsignupfrontend.helpers.IntegrationTestConstants._
import uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks.AuthStub._
import uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks.StoreCompanyNumberStub._
import uk.gov.hmrc.vatsignupfrontend.helpers.{ComponentSpecBase, CustomMatchers}
import uk.gov.hmrc.vatsignupfrontend.models.BusinessEntity.BusinessEntitySessionFormatter
import uk.gov.hmrc.vatsignupfrontend.models.{LimitedCompany, VatGroup}

class CheckYourAnswersCompanyControllerISpec extends ComponentSpecBase with CustomMatchers {

  "GET /check-your-answers-company" should {
    "return an OK" in {
      stubAuth(OK, successfulAuthResponse())

      val res = get("/check-your-answers-company",
        Map(
          companyNumberKey -> testCompanyNumber,
          companyUtrKey -> testCtUtr,
          businessEntityKey -> BusinessEntitySessionFormatter.toString(LimitedCompany)
        )
      )

      res should have(
        httpStatus(OK)
      )
    }
    "return a redirect to capture business entity if the entity is incorrect" in {
      stubAuth(OK, successfulAuthResponse())

      val res = get("/check-your-answers-company",
        Map(
          vatNumberKey -> testVatNumber,
          companyNumberKey -> testCompanyNumber,
          companyUtrKey -> testCtUtr,
          businessEntityKey -> BusinessEntitySessionFormatter.toString(VatGroup)
        )
      )

      res should have(
        httpStatus(SEE_OTHER),
        redirectUri(routes.CaptureBusinessEntityController.show().url)
      )

      val session = getSessionMap(res)
      session.keys should contain(vatNumberKey)
      session.keys shouldNot contain(companyNumberKey)
      session.keys shouldNot contain(companyUtrKey)
      session.keys shouldNot contain(businessEntityKey)
    }
  }

  "POST /check-your-answers-company" when {
    "store CRN and" when {
      "CTUTR is a match" should {
        "redirect to agree capture email" in {
          stubAuth(OK, successfulAuthResponse())
          stubStoreCompanyNumberSuccess(testVatNumber, testCompanyNumber, Some(testCtUtr))

          val res = post("/check-your-answers-company",
            Map(
              vatNumberKey -> testVatNumber,
              companyNumberKey -> testCompanyNumber,
              companyUtrKey -> testCtUtr,
              businessEntityKey -> BusinessEntitySessionFormatter.toString(LimitedCompany)
            )
          )()

          res should have(
            httpStatus(SEE_OTHER),
            redirectUri(routes.DirectDebitResolverController.show().url)
          )
        }
      }
      "CTUTR is a mismatch" should {
        "redirect to could not confirm company" in {
          stubAuth(OK, successfulAuthResponse())
          stubStoreCompanyNumberCtMismatch(testVatNumber, testCompanyNumber, testCtUtr)

          val res = post("/check-your-answers-company",
            Map(
              vatNumberKey -> testVatNumber,
              companyNumberKey -> testCompanyNumber,
              companyUtrKey -> testCtUtr,
              businessEntityKey -> BusinessEntitySessionFormatter.toString(LimitedCompany)
            )
          )()

          res should have(
            httpStatus(SEE_OTHER),
            redirectUri(errorRoutes.CouldNotConfirmBusinessController.show().url)
          )
        }
      }
      "the business entity is missing or incorrect" should {
        "redirect to capture business entity" in {
          stubAuth(OK, successfulAuthResponse())

          val res = get("/check-your-answers-company",
            Map(
              vatNumberKey -> testVatNumber,
              companyNumberKey -> testCompanyNumber,
              companyUtrKey -> testCtUtr,
              businessEntityKey -> BusinessEntitySessionFormatter.toString(VatGroup)
            )
          )

          res should have(
            httpStatus(SEE_OTHER),
            redirectUri(routes.CaptureBusinessEntityController.show().url)
          )

          val session = getSessionMap(res)
          session.keys should contain(vatNumberKey)
          session.keys shouldNot contain(companyNumberKey)
          session.keys shouldNot contain(companyUtrKey)
          session.keys shouldNot contain(businessEntityKey)
        }
      }
    }
  }
}
