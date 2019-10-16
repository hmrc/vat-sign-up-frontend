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

package uk.gov.hmrc.vatsignupfrontend.controllers.principal.resignup

import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.i18n.Messages.Implicits.applicationMessages
import play.api.i18n.MessagesApi
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.api.test.Helpers._
import play.api.{Configuration, Environment}
import uk.gov.hmrc.play.test.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.AppConfig
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockControllerComponents
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants.testVatNumber
import uk.gov.hmrc.vatsignupfrontend.models.SoleTrader

class SignUpCompleteControllerSpec extends UnitSpec with GuiceOneAppPerSuite with MockControllerComponents {

  object TestSignUpCompleteController extends SignUpCompleteController(mockControllerComponents)

  lazy val testGetRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/sign-up-complete").withSession(
    SessionKeys.businessEntityKey -> SoleTrader.toString,
    SessionKeys.vatNumberKey -> testVatNumber
  )

  val env = Environment.simple()
  val configuration = Configuration.load(env)
  val appConfig = new AppConfig(configuration, env)

  lazy val messagesApi = app.injector.instanceOf[MessagesApi]

  lazy val page = uk.gov.hmrc.vatsignupfrontend.views.html.principal.resignup.sign_up_complete(SoleTrader, testVatNumber)(
    FakeRequest("GET", "/sign-up-complete"),
    applicationMessages,
    appConfig
  )

  "Calling the show action of the sign up complete controller" should {
    "show the sign up complete page" in {
      mockAuthAdminRole()
      val request = testGetRequest

      val result = await(TestSignUpCompleteController.show(request))
      status(result) shouldBe Status.OK
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
      contentAsString(result) shouldBe page.body
    }
  }

}
