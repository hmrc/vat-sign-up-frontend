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

package uk.gov.hmrc.vatsignupfrontend.controllers.agent.error

import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.i18n.Messages
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockVatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.controllers.agent.{routes => agentRoutes}
import uk.gov.hmrc.vatsignupfrontend.utils.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.views.html.agent.deregistered_unenrolled

class DeregisteredVatNumberControllerSpec extends UnitSpec with GuiceOneAppPerSuite with MockVatControllerComponents {

  object TestController extends DeregisteredVatNumberController

  implicit lazy val testGetRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/client/error/deregistered-vat-number")

  "Calling the show action of the deregistered vat number controller" should {
    "show the deregistered page" in {
      mockAuthRetrieveAgentEnrolment()
      implicit val messages: Messages = mockVatControllerComponents.controllerComponents.messagesApi.preferred(testGetRequest)

      val result = TestController.show(testGetRequest)

      status(result) shouldBe Status.OK
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
      contentAsString(result) shouldBe deregistered_unenrolled(agentRoutes.CaptureVatNumberController.show().url).body
    }
  }

}
