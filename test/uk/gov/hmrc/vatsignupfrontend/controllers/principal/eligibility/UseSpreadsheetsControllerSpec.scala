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

package uk.gov.hmrc.vatsignupfrontend.controllers.principal.eligibility

import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.i18n.Messages
import play.api.mvc.{AnyContent, Request}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.play.test.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockControllerComponents
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.eligibility

class UseSpreadsheetsControllerSpec extends UnitSpec with GuiceOneAppPerSuite with MockControllerComponents {

  class Setup {
    val controller = new UseSpreadsheetsController(mockControllerComponents)
  }

  "show" should {
    "render the view successfully" in new Setup {
      implicit val req: Request[AnyContent] = FakeRequest()
      implicit val messages: Messages = mockMessagesApi.preferred(req)
      val res = controller.show(req)
      contentAsString(res) shouldBe eligibility.use_spreadsheets(routes.ResolveVatNumberController.resolve()).body
      status(res) shouldBe Status.OK
    }
  }

}
