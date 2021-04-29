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

package uk.gov.hmrc.vatsignupfrontend.controllers.agent.resignup

import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.i18n.MessagesApi
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.api.test.Helpers._
import play.twirl.api.Html
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.AppConfig
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockVatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.controllers.agent.routes.SignUpAnotherClientController
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants.testVatNumber
import uk.gov.hmrc.vatsignupfrontend.models.SoleTrader
import uk.gov.hmrc.vatsignupfrontend.utils.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.views.html.agent.resignup.sign_up_complete

class SignUpCompleteControllerSpec extends UnitSpec with GuiceOneAppPerSuite with MockVatControllerComponents {

  val view = app.injector.instanceOf[sign_up_complete]

  object TestSignUpCompleteController extends SignUpCompleteController(view)

  lazy val testGetRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/client/sign-up-complete").withSession(
    SessionKeys.businessEntityKey -> SoleTrader.toString,
    SessionKeys.vatNumberKey -> testVatNumber
  )

  val appConfig: AppConfig = mockVatControllerComponents.appConfig
  lazy val messagesApi: MessagesApi = mockVatControllerComponents.controllerComponents.messagesApi

  lazy val page: Html =
    view(SoleTrader, testVatNumber, SignUpAnotherClientController.submit())(
      testGetRequest,
      messagesApi.preferred(testGetRequest),
      appConfig
    )

  "Calling the show action of the sign up complete controller" should {
    "show the sign up complete page" in {
      mockAuthRetrieveAgentEnrolment()
      val request = testGetRequest

      val result = TestSignUpCompleteController.show(request)

      status(result) shouldBe Status.OK
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
      contentAsString(result) shouldBe page.body
    }
  }

}
