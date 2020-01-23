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

package uk.gov.hmrc.vatsignupfrontend.controllers.principal.error

import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.vatsignupfrontend.utils.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockVatControllerComponents

class AgentUsingPrincipalJourneyControllerSpec extends UnitSpec with GuiceOneAppPerSuite with MockVatControllerComponents {

  object TestAgentUsingPrincipalJourneyController extends AgentUsingPrincipalJourneyController

  lazy val testGetRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/error/wrong-account-type")

  "Calling the show action of the agent using wrong journey controller" when {
    "using an agent account" should {
      "show the agent using principal journey page page" in {
        mockAuthRetrieveAgentEnrolment()
        val request = testGetRequest

        val result = TestAgentUsingPrincipalJourneyController.show(request)
        status(result) shouldBe Status.OK
        contentType(result) shouldBe Some("text/html")
        charset(result) shouldBe Some("utf-8")
      }
    }
  }

}
