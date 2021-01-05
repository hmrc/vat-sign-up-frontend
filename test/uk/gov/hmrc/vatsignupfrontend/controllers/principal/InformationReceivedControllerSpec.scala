/*
 * Copyright 2021 HM Revenue & Customs
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

import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.vatsignupfrontend.utils.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockVatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.models.SoleTrader
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants.testVatNumber

class InformationReceivedControllerSpec extends UnitSpec with GuiceOneAppPerSuite with MockVatControllerComponents {

  object TestInformationReceivedController extends InformationReceivedController

  lazy val testGetRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/information-received").withSession(
    SessionKeys.businessEntityKey -> SoleTrader.toString,
    SessionKeys.vatNumberKey -> testVatNumber
  )

  "Calling the show action of the information received controller" should {
    "show the information received page" in {
      mockAuthAdminRole()
      val request = testGetRequest

      val result = TestInformationReceivedController.show(request)
      status(result) shouldBe Status.OK
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
    }
  }

}
