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

package uk.gov.hmrc.vatsignupfrontend.connectors

import play.api.http.Status._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.vatsignupfrontend.helpers.ComponentSpecBase
import uk.gov.hmrc.vatsignupfrontend.helpers.IntegrationTestConstants._
import uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks.StoreTrustInformationStub
import uk.gov.hmrc.vatsignupfrontend.httpparsers.StoreTrustInformationHttpParser.StoreTrustInformationSuccess

class StoreTrustInformationConnectorISpec extends ComponentSpecBase {

  lazy val connector: StoreTrustInformationConnector = app.injector.instanceOf[StoreTrustInformationConnector]

  private implicit val headerCarrier: HeaderCarrier = HeaderCarrier()

  "storeTrustInformation" when {
    "Backend returns a NO_CONTENT response" should {
      "return StoreTrustInformationSuccess" in {
        StoreTrustInformationStub.stubStoreTrustInformation(testVatNumber)(NO_CONTENT)

        val res = connector.storeTrustInformation(testVatNumber)

        await(res) shouldBe Right(StoreTrustInformationSuccess)
      }
    }
  }

}
