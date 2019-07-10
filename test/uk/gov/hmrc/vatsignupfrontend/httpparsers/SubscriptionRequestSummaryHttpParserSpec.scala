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

package uk.gov.hmrc.vatsignupfrontend.httpparsers

import play.api.Logger
import play.api.http.Status._
import play.api.libs.json.Json
import uk.gov.hmrc.http.HttpResponse
import uk.gov.hmrc.play.test.{LogCapturing, UnitSpec}
import uk.gov.hmrc.vatsignupfrontend.httpparsers.SubscriptionRequestSummaryHttpParser._
import uk.gov.hmrc.vatsignupfrontend.models.{Digital, LimitedCompany, SubscriptionRequestSummary}

class SubscriptionRequestSummaryHttpParserSpec extends UnitSpec with LogCapturing {
  val testHttpVerb = "GET"
  val testUri = "/"

  val validJson = Json.parse(
    s"""{
       |  "vatNumber": "vatNumberFoo",
       |  "businessEntity": {
       |    "entityType": "limitedCompany"
       |  },
       |  "transactionEmail": "transEmail",
       |  "optSignUpEmail": "emailSignFoo",
       |  "contactPreference": "Digital"
       |}  """.stripMargin)

  val invalidJsonWrongContactPreference = Json.parse(
    s"""{
      |  "vatNumber": "vatNumberFoo",
      |  "businessEntity": {
      |    "entityType": "limitedCompany"
      |  },
      |  "transactionEmail": "transEmail",
      |  "optSignUpEmail": "emailSignFoo",
      |  "contactPreference": "FOO this is incorrect"
      |}  """.stripMargin)

  val invalidJsonWrongBusinessEntityType = Json.parse(
   s"""{
      |  "vatNumber": "vatNumberFoo",
      |  "businessEntity": {
      |    "entityType": "FOO this is incorrect"
      |  },
      |  "transactionEmail": "transEmail",
      |  "optSignUpEmail": "emailSignFoo",
      |  "contactPreference": "Digital"
      |}  """.stripMargin)

  val expectedModelForValidJson = SubscriptionRequestSummary(
    "vatNumberFoo",
    LimitedCompany,
    Some("emailSignFoo"),
    "transEmail",
    Digital
  )

  val invalidJson = Json.parse(
    """
      |{
      |   "foo": "bar"
      |}
    """.stripMargin)

  "GetSubscriptionRequestSummaryHttpReads" should {
    "parse a OK as a successful Right response" in {
      val httpResponse = HttpResponse(OK, Some(validJson))

      val res = SubscriptionRequestSummaryHttpParser.GetSubscriptionRequestSummaryHttpReads.read(testHttpVerb, testUri, httpResponse)
      res.right.get shouldBe expectedModelForValidJson
    }
    "parse a OK with invalid Json as a Left SubscriptionRequestSomethingWentWrong with log" in {
      val httpResponse = HttpResponse(OK, Some(invalidJson))
      withCaptureOfLoggingFrom(Logger) { logger =>
        val res = SubscriptionRequestSummaryHttpParser.GetSubscriptionRequestSummaryHttpReads.read(testHttpVerb, testUri, httpResponse)
        res.left.get shouldBe SubscriptionRequestUnexpectedError(OK,s"JSON does not meet read requirements of SubscriptionRequestSummary")
        val log = logger.map(log => (log.getLevel,log.getMessage)).head
        log._1.levelStr shouldBe "ERROR"
        log._2 shouldBe "SubscriptionRequestUnexpectedError - 200 - JSON does not meet read requirements of SubscriptionRequestSummary"
      }
    }
    "parse a OK with invalid Json wrong business entity type but as a Left SubscriptionRequestUnexpectedError with log" in {
      val httpResponse = HttpResponse(OK, Some(invalidJsonWrongBusinessEntityType))
      withCaptureOfLoggingFrom(Logger) { logger =>
        val res = SubscriptionRequestSummaryHttpParser.GetSubscriptionRequestSummaryHttpReads.read(testHttpVerb, testUri, httpResponse)
        res.left.get shouldBe SubscriptionRequestUnexpectedError(OK,s"JSON does not meet read requirements of SubscriptionRequestSummary")
        val log = logger.map(log => (log.getLevel,log.getMessage)).head
        log._1.levelStr shouldBe "ERROR"
        log._2 shouldBe "SubscriptionRequestUnexpectedError - 200 - JSON does not meet read requirements of SubscriptionRequestSummary"
      }
    }
    "parse a NOT_FOUND as a Left SubscriptionRequestDoesNotExist" in {
      val httpResponse = HttpResponse(NOT_FOUND, None)
      val res = SubscriptionRequestSummaryHttpParser.GetSubscriptionRequestSummaryHttpReads.read(testHttpVerb, testUri, httpResponse)
      res.left.get shouldBe SubscriptionRequestDoesNotExist
    }
    "parse a BAD_REQUEST as a Left SubscriptionRequestExistsButNotComplete" in {
      val httpResponse = HttpResponse(BAD_REQUEST, None)
      val res = SubscriptionRequestSummaryHttpParser.GetSubscriptionRequestSummaryHttpReads.read(testHttpVerb, testUri, httpResponse)
      res.left.get shouldBe SubscriptionRequestExistsButNotComplete
    }
    "parse a unexpected status as a Left SubscriptionRequestUnexpectedError with log" in {
      val httpResponse = HttpResponse(INTERNAL_SERVER_ERROR, None)
      withCaptureOfLoggingFrom(Logger) { logger =>
        val res = SubscriptionRequestSummaryHttpParser.GetSubscriptionRequestSummaryHttpReads.read(testHttpVerb, testUri, httpResponse)
        res.left.get shouldBe SubscriptionRequestUnexpectedError(INTERNAL_SERVER_ERROR, "Unexpected status from Backend")
        val log = logger.map(log => (log.getLevel,log.getMessage)).head
        log._1.levelStr shouldBe "ERROR"
        log._2 shouldBe "SubscriptionRequestUnexpectedError - 500 - Unexpected status from Backend"
      }
    }
  }

  "SubscriptionRequestSummary json reads" should {
    "return model with valid model" in {
      validJson.as[SubscriptionRequestSummary] shouldBe expectedModelForValidJson
    }
    "return exception when invalid business entity is used but everything else is valid" in {
      intercept[Exception](invalidJsonWrongBusinessEntityType.as[SubscriptionRequestSummary])
    }
    "return exception when invalid contact preference is used but everything else is valid" in {
      intercept[Exception](invalidJsonWrongContactPreference.as[SubscriptionRequestSummary])
    }
  }
}
