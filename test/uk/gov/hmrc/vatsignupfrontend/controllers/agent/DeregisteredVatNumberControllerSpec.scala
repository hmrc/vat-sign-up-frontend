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

package uk.gov.hmrc.vatsignupfrontend.controllers.agent

import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.play.test.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockControllerComponents
import uk.gov.hmrc.vatsignupfrontend.views.html.agent.deregistered_unenrolled
import play.api.i18n.Messages.Implicits._
import play.api.mvc.AnyContentAsEmpty

class DeregisteredVatNumberControllerSpec extends UnitSpec with GuiceOneAppPerSuite with MockControllerComponents {

  object TestController extends DeregisteredVatNumberController(mockControllerComponents)

  implicit lazy val testGetRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/error/deregistered-vat-number")

  "Calling the show action of the deregistered vat number controller" should {
    "show the deregistered page" in {

      mockAuthRetrieveAgentEnrolment()

      val result = TestController.show(testGetRequest)

      status(result) shouldBe Status.OK
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
      contentAsString(result) shouldBe deregistered_unenrolled(routes.CaptureVatNumberController.show().url).body
    }
  }

}
