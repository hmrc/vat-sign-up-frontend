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

package uk.gov.hmrc.vatsignupfrontend.views.agent.partnerships

import play.api.i18n.MessagesApi
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.vatsignupfrontend.assets.MessageLookup.{AgentConfirmPartnership => messages}
import uk.gov.hmrc.vatsignupfrontend.config.AppConfig
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.views.ViewSpec

class ConfirmPartnershipSpec extends ViewSpec {

  lazy val appConfig: AppConfig = app.injector.instanceOf[AppConfig]
  lazy val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]
  val request: FakeRequest[AnyContentAsEmpty.type] = FakeRequest()

  lazy val page: HtmlFormat.Appendable = uk.gov.hmrc.vatsignupfrontend.views.html.agent.partnerships.confirm_partnership(
    companyName = testCompanyName,
    postAction = testCall,
    changeLink = testUri)(
    request,
    messagesApi.preferred(request),
    appConfig
  )

  "The Confirm Partnership view" should {

    val testPage = TestView(
      name = "Confirm Partnership View",
      title = messages.title,
      heading = messages.heading,
      page = page
    )

    testPage.shouldHavePara(testCompanyName)

    testPage.shouldHaveForm("Confirm Partnership Form")(actionCall = testCall)

    testPage.shouldHaveConfirmAndContinueButton()

    testPage.shouldHaveALink(
      id = "changeLink",
      text = messages.link,
      href = testUri
    )
  }

}

