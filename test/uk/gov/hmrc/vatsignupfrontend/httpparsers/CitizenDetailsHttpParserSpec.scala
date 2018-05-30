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

package uk.gov.hmrc.vatsignupfrontend.httpparsers

import play.api.http.Status._
import play.api.libs.json.Json
import uk.gov.hmrc.http.HttpResponse
import uk.gov.hmrc.play.test.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.httpparsers.CitizenDetailsHttpParser._
import uk.gov.hmrc.vatsignupfrontend.models.{DateModel, UserDetailsModel}
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._

class CitizenDetailsHttpParserSpec extends UnitSpec {
  val testHttpVerb = "GET"
  val testUri = "/"

  val validJson =  Json.parse(
    s"""{
      |  "name": {
      |    "current": {
      |      "firstName": "$testName",
      |      "lastName": "$testName"
      |    },
      |    "previous": []
      |  },
      |  "ids": {
      |    "nino": "$testNino"
      |  },
      |  "dateOfBirth": "11121971"
      |}  """.stripMargin('|'))

  val invalidJson =  Json.parse(
    s"""{
      |  "ids": {
      |    "nino": "$testNino"
      |  }
      |}""".stripMargin('|'))


  "CitizenDetailsHttpReads" should {

    def res(httpResponse: HttpResponse) = CitizenDetailsHttpReads.read(testHttpVerb, testUri, httpResponse)

    "parse a OK response as an CitizenDetailsRetrievalSuccess" when {
      "an instance of Citizen Details can be successfully parsed from the response body" in {
        val httpResponse = HttpResponse(OK, Some(validJson))

        res(httpResponse) shouldBe Right(CitizenDetailsRetrievalSuccess(UserDetailsModel(testName, testName, testNino,  DateModel("11", "12", "1971"))))
      }
    }

    "parse a OK response as an CitizenDetailsRetrievalFailureResponse" when {
      "an instance of Citizen Details cannot be parsed from the response body" in {
        val httpResponse = HttpResponse(OK, Some(invalidJson))

        res(httpResponse) shouldBe Left(CitizenDetailsRetrievalFailureResponse(INTERNAL_SERVER_ERROR))
      }
    }
    "parse a NOT_FOUND response as an NoCitizenRecord" in {
      val httpResponse = HttpResponse(NOT_FOUND)

      res(httpResponse) shouldBe Left(NoCitizenRecord)
    }
    "parse a INTERNAL_SERVER_ERROR response as an MoreThanOneCitizenMatched" in {
      val httpResponse = HttpResponse(INTERNAL_SERVER_ERROR)

      res(httpResponse) shouldBe Left(MoreThanOneCitizenMatched)
    }
    "proxy any other server response as a CitizenDetailsRetrievalFailureResponse" in {
      val httpResponse = HttpResponse(UNAUTHORIZED)

      res(httpResponse) shouldBe  Left(CitizenDetailsRetrievalFailureResponse(UNAUTHORIZED))
    }
  }
}
