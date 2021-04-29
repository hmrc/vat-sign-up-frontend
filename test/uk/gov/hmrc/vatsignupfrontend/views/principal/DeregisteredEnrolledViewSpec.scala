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

package uk.gov.hmrc.vatsignupfrontend.views.principal

import play.api.i18n.MessagesApi
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.vatsignupfrontend.assets.MessageLookup.{PrincipalDeregisteredEnrolled => messages}
import uk.gov.hmrc.vatsignupfrontend.config.AppConfig
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants.testVatNumber
import uk.gov.hmrc.vatsignupfrontend.views.ViewSpec
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.deregistered_enrolled

class DeregisteredEnrolledViewSpec extends ViewSpec {

  val testLink: String = "test/test"
  val view = app.injector.instanceOf[deregistered_enrolled]
  lazy val appConfig: AppConfig = app.injector.instanceOf[AppConfig]
  lazy val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]
  val request: FakeRequest[AnyContentAsEmpty.type] = FakeRequest()

  lazy val page: HtmlFormat.Appendable = view(
    testVatNumber)(
    request,
    messagesApi.preferred(request),
    appConfig
  )

  "The Dissolved Company view" should {

    val testPage = TestView(
      name = "Deregistered Enrolled View",
      title = messages.title,
      heading = messages.heading,
      page = page
    )

    testPage.shouldHaveALink("vatRegistration", messages.link, appConfig.vatRegistrationUrl)

    testPage.shouldHavePara(messages.paragraph1(testVatNumber))
    testPage.shouldHavePara(messages.paragraph2)

    testPage.shouldHaveBulletSeq(messages.bullet1, messages.bullet2)

    testPage.shouldHaveSignOutButton(isAgent = false)
  }
}
