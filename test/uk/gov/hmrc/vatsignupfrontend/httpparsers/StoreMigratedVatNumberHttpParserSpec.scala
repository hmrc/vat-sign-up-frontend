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

import org.scalatest.EitherValues
import play.api.http.Status._
import uk.gov.hmrc.http.HttpResponse
import uk.gov.hmrc.play.test.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.httpparsers.StoreMigratedVatNumberHttpParser.{StoreMigratedVatNumberFailure, StoreMigratedVatNumberSuccess}

class StoreMigratedVatNumberHttpParserSpec extends UnitSpec with EitherValues {

  val testHttpVerb = "POST"
  val testUri = "/"

  "StoreVatNumberHttpReads" when {
    "read" should {
      "parse an OK response as a store migrated Vat number SuccessResponse" in {
        val httpResponse = HttpResponse(OK)
        val result = StoreMigratedVatNumberHttpParser.StoreMigratedVatNumberHttpReads.read(testHttpVerb, testUri, httpResponse)
        result shouldBe Right(StoreMigratedVatNumberSuccess)
      }

      "parse FORBIDDEN response as a store migrated Vat number FailureResponse" in {
        val httpResponse = HttpResponse(FORBIDDEN)
        val result = StoreMigratedVatNumberHttpParser.StoreMigratedVatNumberHttpReads.read(testHttpVerb, testUri, httpResponse)
        result shouldBe Left(StoreMigratedVatNumberFailure)
      }
    }
  }

}
