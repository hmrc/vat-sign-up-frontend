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

package uk.gov.hmrc.vatsignupfrontend.httpparsers

import org.scalatest.EitherValues
import play.api.http.Status._
import play.api.libs.json.Json
import uk.gov.hmrc.http.HttpResponse
import uk.gov.hmrc.vatsignupfrontend.utils.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.httpparsers.ClaimSubscriptionHttpParser._

class ClaimSubscriptionHttpParserSpec extends UnitSpec with EitherValues {
  val testHttpVerb = "POST"
  val testUri = "/"

  "ClaimSubscriptionHttpReads" when {
    "read" should {
      "parse a NO_CONTENT response as a SubscriptionClaimed" in {
        val httpResponse = HttpResponse(NO_CONTENT)

        val res = ClaimSubscriptionHttpReads.read(testHttpVerb, testUri, httpResponse)

        res.right.value shouldBe ClaimSubscriptionHttpParser.SubscriptionClaimed
      }

      "parse a FORBIDDEN response as a KnownFactsMismatch" in {
        val httpResponse = HttpResponse(FORBIDDEN)

        val res = ClaimSubscriptionHttpReads.read(testHttpVerb, testUri, httpResponse)

        res.left.value shouldBe ClaimSubscriptionHttpParser.KnownFactsMismatch
      }

      "parse an BAD_REQUEST response as an InvalidVatNumber" in {
        val httpResponse = HttpResponse(BAD_REQUEST, "error message")

        val res = ClaimSubscriptionHttpReads.read(testHttpVerb, testUri, httpResponse)

        res.left.value shouldBe ClaimSubscriptionHttpParser.InvalidVatNumber("error message")
      }

      "parse an CONFLICT response as an AlreadyEnrolledOnDifferentCredential" in {
        val httpResponse = HttpResponse(CONFLICT)

        val res = ClaimSubscriptionHttpReads.read(testHttpVerb, testUri, httpResponse)

        res.left.value shouldBe ClaimSubscriptionHttpParser.AlreadyEnrolledOnDifferentCredential
      }

      "parse any other response as a ClaimSubscriptionFailureResponse" in {
        val httpResponse = HttpResponse(INTERNAL_SERVER_ERROR, "error message")

        val res = ClaimSubscriptionHttpReads.read(testHttpVerb, testUri, httpResponse)

        res.left.value shouldBe ClaimSubscriptionFailureResponse(INTERNAL_SERVER_ERROR, "error message")
      }

    }
  }

}
