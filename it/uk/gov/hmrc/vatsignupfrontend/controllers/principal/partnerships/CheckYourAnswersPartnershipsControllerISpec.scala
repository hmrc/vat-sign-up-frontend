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

package uk.gov.hmrc.vatsignupfrontend.controllers.principal.partnerships

import play.api.http.Status._
import play.api.libs.json.Json
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.controllers.principal.{routes => principalRoutes}
import uk.gov.hmrc.vatsignupfrontend.helpers.IntegrationTestConstants._
import uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks.AuthStub._
import uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks.StorePartnershipInformationStub._
import uk.gov.hmrc.vatsignupfrontend.helpers.{ComponentSpecBase, CustomMatchers}
import uk.gov.hmrc.vatsignupfrontend.models.BusinessEntity.BusinessEntitySessionFormatter
import uk.gov.hmrc.vatsignupfrontend.models.{GeneralPartnership, LimitedPartnership, PartnershipEntityType}

class CheckYourAnswersPartnershipsControllerISpec extends ComponentSpecBase with CustomMatchers {

  val generalPartnershipType = PartnershipEntityType.GeneralPartnership
  val limitedPartnershipType = PartnershipEntityType.LimitedPartnership

  "GET /check-your-answers-partnership" should {
    "return an OK for a general partnership without SAUTR" in {
      stubAuth(OK, successfulAuthResponse())

      val res = get(
        uri = "/check-your-answers-partnership",
        cookies = Map(
          SessionKeys.businessEntityKey -> BusinessEntitySessionFormatter.toString(GeneralPartnership),
          SessionKeys.hasOptionalSautrKey -> false.toString
        )
      )

      res should have(
        httpStatus(OK)
      )
    }

    "return an OK for a general partnership" in {
      stubAuth(OK, successfulAuthResponse())

      val res = get(
        uri = "/check-your-answers-partnership",
        cookies = Map(
          SessionKeys.businessEntityKey -> BusinessEntitySessionFormatter.toString(GeneralPartnership),
          SessionKeys.partnershipSautrKey -> testSaUtr,
          SessionKeys.partnershipPostCodeKey -> Json.toJson(testBusinessPostCode).toString()
        )
      )

      res should have(
        httpStatus(OK)
      )
    }

    "return an OK for a limited partnership" in {
      stubAuth(OK, successfulAuthResponse())

      val res = get(
        uri = "/check-your-answers-partnership",
        cookies = Map(
          SessionKeys.businessEntityKey -> BusinessEntitySessionFormatter.toString(LimitedPartnership),
          SessionKeys.partnershipTypeKey -> limitedPartnershipType.toString,
          SessionKeys.partnershipSautrKey -> testSaUtr,
          SessionKeys.companyNumberKey -> testCompanyNumber,
          SessionKeys.partnershipPostCodeKey -> Json.toJson(testBusinessPostCode).toString()
        )
      )

      res should have(
        httpStatus(OK)
      )
    }
  }

  "POST /check-your-answers-partnership" when {
    "the user is a general partnership" when {
      "the user is has an optional Sautr" when {
        "store partnership information is successful" should {
          "redirect to agree to Direct Debit resolver" in {
            stubAuth(OK, successfulAuthResponse())
            stubStorePartnershipInformation(
              vatNumber = testVatNumber,
              partnershipEntityType = generalPartnershipType,
              sautr = Some(testSaUtr),
              companyNumber = None,
              postCode = Some(testBusinessPostCode)
            )(NO_CONTENT)

            val res = post(
              uri = "/check-your-answers-partnership",
              cookies = Map(
                SessionKeys.businessEntityKey -> BusinessEntitySessionFormatter.toString(GeneralPartnership),
                SessionKeys.vatNumberKey -> testVatNumber,
                SessionKeys.partnershipSautrKey -> testSaUtr,
                SessionKeys.partnershipPostCodeKey -> Json.toJson(testBusinessPostCode).toString()
              )
            )()

            res should have(
              httpStatus(SEE_OTHER),
              redirectUri(principalRoutes.DirectDebitResolverController.show().url)
            )
          }
        }

        "store partnership information failed" should {
          "throw internal server error" in {
            stubAuth(OK, successfulAuthResponse())
            stubStorePartnershipInformation(
              vatNumber = testVatNumber,
              partnershipEntityType = generalPartnershipType,
              sautr = Some(testSaUtr),
              companyNumber = None,
              postCode = Some(testBusinessPostCode)
            )(BAD_REQUEST)

            val res = post(
              uri = "/check-your-answers-partnership",
              cookies = Map(
                SessionKeys.businessEntityKey -> BusinessEntitySessionFormatter.toString(GeneralPartnership),
                SessionKeys.vatNumberKey -> testVatNumber,
                SessionKeys.partnershipSautrKey -> testSaUtr,
                SessionKeys.partnershipPostCodeKey -> Json.toJson(testBusinessPostCode).toString()
              )
            )()

            res should have(
              httpStatus(INTERNAL_SERVER_ERROR)
            )
          }
        }

      }

      "the user does not have an Sautr" when {
        "store partnership information is successful" should {
          "redirect to agree to Direct Debit resolver" in {
            stubAuth(OK, successfulAuthResponse())
            stubStorePartnershipInformation(
              vatNumber = testVatNumber,
              sautr = None,
              partnershipEntityType = generalPartnershipType,
              companyNumber = None,
              postCode = None
            )(NO_CONTENT)

            val res = post(
              uri = "/check-your-answers-partnership",
              cookies = Map(
                SessionKeys.businessEntityKey -> BusinessEntitySessionFormatter.toString(GeneralPartnership),
                SessionKeys.vatNumberKey -> testVatNumber
              )
            )()

            res should have(
              httpStatus(SEE_OTHER),
              redirectUri(principalRoutes.DirectDebitResolverController.show().url)
            )
          }
        }

        "store partnership information failed" should {
          "throw internal server error" in {
            stubAuth(OK, successfulAuthResponse())
            stubStorePartnershipInformation(
              vatNumber = testVatNumber,
              sautr = None,
              partnershipEntityType = generalPartnershipType,
              companyNumber = None,
              postCode = None
            )(BAD_REQUEST)

            val res = post(
              uri = "/check-your-answers-partnership",
              cookies = Map(
                SessionKeys.businessEntityKey -> BusinessEntitySessionFormatter.toString(GeneralPartnership),
                SessionKeys.vatNumberKey -> testVatNumber
              )
            )()

            res should have(
              httpStatus(INTERNAL_SERVER_ERROR)
            )
          }
        }

      }
    }

    "the user is a limited partnership" when {
      "store partnership information is successful" should {
        "redirect to agree to Direct Debit resolver" in {
          stubAuth(OK, successfulAuthResponse())
          stubStorePartnershipInformation(
            vatNumber = testVatNumber,
            partnershipEntityType = limitedPartnershipType,
            sautr = Some(testSaUtr),
            companyNumber = Some(testCompanyNumber),
            postCode = Some(testBusinessPostCode)
          )(NO_CONTENT)

          val res = post(
            uri = "/check-your-answers-partnership",
            cookies = Map(
              SessionKeys.businessEntityKey -> BusinessEntitySessionFormatter.toString(LimitedPartnership),
              SessionKeys.partnershipTypeKey -> limitedPartnershipType.toString,
              SessionKeys.vatNumberKey -> testVatNumber,
              SessionKeys.companyNumberKey -> testCompanyNumber,
              SessionKeys.partnershipSautrKey -> testSaUtr,
              SessionKeys.partnershipPostCodeKey -> Json.toJson(testBusinessPostCode).toString()
            )
          )()

          res should have(
            httpStatus(SEE_OTHER),
            redirectUri(principalRoutes.DirectDebitResolverController.show().url)
          )
        }
      }

      "store partnership information failed" should {
        "throw internal server error" in {
          stubAuth(OK, successfulAuthResponse())
          stubStorePartnershipInformation(
            vatNumber = testVatNumber,
            partnershipEntityType = limitedPartnershipType,
            sautr = Some(testSaUtr),
            companyNumber = Some(testCompanyNumber),
            postCode = Some(testBusinessPostCode)
          )(BAD_REQUEST)

          val res = post(
            uri = "/check-your-answers-partnership",
            cookies = Map(
              SessionKeys.businessEntityKey -> BusinessEntitySessionFormatter.toString(LimitedPartnership),
              SessionKeys.partnershipTypeKey -> limitedPartnershipType.toString,
              SessionKeys.vatNumberKey -> testVatNumber,
              SessionKeys.companyNumberKey -> testCompanyNumber,
              SessionKeys.partnershipSautrKey -> testSaUtr,
              SessionKeys.partnershipPostCodeKey -> Json.toJson(testBusinessPostCode).toString()
            )
          )()

          res should have(
            httpStatus(INTERNAL_SERVER_ERROR)
          )
        }
      }

    }

    "store partnership information known facts mismatch" should {
      "redirect to known facts error page" in {
        stubAuth(OK, successfulAuthResponse())
        stubStorePartnershipInformation(
          vatNumber = testVatNumber,
          partnershipEntityType = generalPartnershipType,
          sautr = Some(testSaUtr),
          companyNumber = None,
          postCode = Some(testBusinessPostCode)
        )(FORBIDDEN)

        val res = post(
          uri = "/check-your-answers-partnership",
          cookies = Map(
            SessionKeys.businessEntityKey -> BusinessEntitySessionFormatter.toString(GeneralPartnership),
            SessionKeys.vatNumberKey -> testVatNumber,
            SessionKeys.partnershipSautrKey -> testSaUtr,
            SessionKeys.partnershipPostCodeKey -> Json.toJson(testBusinessPostCode).toString()
          )
        )()

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(routes.CouldNotConfirmKnownFactsController.show().url)
        )
      }
    }
  }
}
