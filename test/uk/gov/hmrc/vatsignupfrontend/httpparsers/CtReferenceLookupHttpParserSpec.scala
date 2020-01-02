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

import play.api.http.Status._
import uk.gov.hmrc.http.HttpResponse
import uk.gov.hmrc.play.test.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.httpparsers.CtReferenceLookupHttpParser._

class CtReferenceLookupHttpParserSpec extends UnitSpec {
  val testHttpVerb = "POST"
  val testUri = "/"

  "CtReferenceLookupHttpReads" when {
    "read" should {
      "parse a OK response as an CtReferenceIsFound" in {
        val httpResponse = HttpResponse(OK)

        val res = CtReferenceLookupHttpReads.read(testHttpVerb, testUri, httpResponse)

        res shouldBe Right(CtReferenceIsFound)
      }
      "parse NOT_FOUND response with the expected body as CtReferenceMismatch" in {
        val httpResponse = HttpResponse(NOT_FOUND)

        val res = CtReferenceLookupHttpReads.read(testHttpVerb, testUri, httpResponse)

        res shouldBe Left(CtReferenceNotFound)
      }
      "parse any other response as an CtReferenceLookupFailureResponse" in {
        val httpResponse = HttpResponse(BAD_REQUEST)

        val res = CtReferenceLookupHttpReads.read(testHttpVerb, testUri, httpResponse)

        res shouldBe Left(CtReferenceLookupFailureResponse(httpResponse.status))
      }
    }
  }
}
