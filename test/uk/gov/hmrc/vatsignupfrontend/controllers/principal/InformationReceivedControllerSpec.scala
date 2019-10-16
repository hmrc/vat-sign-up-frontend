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

package uk.gov.hmrc.vatsignupfrontend.controllers.principal

import org.jsoup.Jsoup
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.play.test.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch.ReSignUpJourney
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockControllerComponents
import uk.gov.hmrc.vatsignupfrontend.models.SoleTrader
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants.testVatNumber

class InformationReceivedControllerSpec extends UnitSpec with GuiceOneAppPerSuite with MockControllerComponents {

  object TestInformationReceivedController extends InformationReceivedController(mockControllerComponents)

  lazy val testGetRequest = FakeRequest("GET", "/information-received").withSession(
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

  "Calling the show action of the information received controller with the resignup feature switch turned on" should {
    "show the resignup information received page with the continue button" in {
      mockAuthAdminRole()
      enable(ReSignUpJourney)
      val request = testGetRequest

      val result = TestInformationReceivedController.show(request)
      status(result) shouldBe Status.OK
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
      val doc = Jsoup.parse(contentAsString(result))
      Some(doc.getElementById("continue-button")).isEmpty shouldBe false
    }
  }

}
