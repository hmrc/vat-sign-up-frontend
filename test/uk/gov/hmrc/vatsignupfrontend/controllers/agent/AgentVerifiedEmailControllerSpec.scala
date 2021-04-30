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

package uk.gov.hmrc.vatsignupfrontend.controllers.agent

import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.i18n.Messages
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockVatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.utils.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.views.html.agent.agent_email_verified

class AgentVerifiedEmailControllerSpec extends UnitSpec with GuiceOneAppPerSuite with MockVatControllerComponents {

  val view = app.injector.instanceOf[agent_email_verified]

  object TestAgentVerifiedEmailController extends AgentVerifiedEmailController(view)

  "Calling the show action of the AgentVerifiedEmailController" should {
    implicit lazy val testGetRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/verified-your-email")
    implicit lazy val messages: Messages = mockVatControllerComponents.controllerComponents.messagesApi.preferred(testGetRequest)

    "go to the Agent email Verified page with a link to the Contact Preferences page" in {
      mockAuthRetrieveAgentEnrolment()
      val result = TestAgentVerifiedEmailController.show(testGetRequest)

      status(result) shouldBe Status.OK
      contentAsString(result) shouldBe view(routes.ContactPreferenceController.show().url).body
    }
  }
}
