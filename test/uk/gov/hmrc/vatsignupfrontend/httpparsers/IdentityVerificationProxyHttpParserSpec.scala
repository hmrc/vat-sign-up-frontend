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

import play.api.http.Status.{BAD_REQUEST, CREATED}
import play.api.libs.json.Json
import uk.gov.hmrc.http.HttpResponse
import uk.gov.hmrc.play.test.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.httpparsers.IdentityVerificationProxyHttpParser._

class IdentityVerificationProxyHttpParserSpec extends UnitSpec {
  val testHttpVerb = "POST"
  val testUri = "/"

  "IdentityVerificationProxyHttpReads" when {
    "read" should {
      "parse a CREATED response as an IdentityVerificationProxySuccessResponse" in {
        val httpResponse = HttpResponse(CREATED, Some(
          Json.parse(
            """{
              |  "link": "/some-journey-start-url",
              |  "journeyLink": "/some-journey-status-url"
              |}""".stripMargin('|')))
        )

        val res = IdentityVerificationProxyHttpReads.read(testHttpVerb, testUri, httpResponse)

        res shouldBe Right(IdentityVerificationProxySuccessResponse("/some-journey-start-url", "/some-journey-status-url"))
      }

      "parse any other response as an IdentityVerificationProxyFailureResponse" in {
        val httpResponse = HttpResponse(BAD_REQUEST, Some(Json.obj()))

        val res = IdentityVerificationProxyHttpReads.read(testHttpVerb, testUri, httpResponse)

        res shouldBe Left(IdentityVerificationProxyFailureResponse(httpResponse.status))
      }
    }
  }
}
