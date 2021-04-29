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
import uk.gov.hmrc.vatsignupfrontend.assets.MessageLookup.{PrincipalCaptureEmail => messages}
import uk.gov.hmrc.vatsignupfrontend.config.AppConfig
import uk.gov.hmrc.vatsignupfrontend.forms.EmailForm._
import uk.gov.hmrc.vatsignupfrontend.views.ViewSpec
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.capture_email

class CaptureEmailSpec extends ViewSpec {

  val view = app.injector.instanceOf[capture_email]
  lazy val appConfig: AppConfig = app.injector.instanceOf[AppConfig]
  lazy val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]
  val request: FakeRequest[AnyContentAsEmpty.type] = FakeRequest()

  lazy val DDpage: HtmlFormat.Appendable = view(
    hasDirectDebit = true,
    emailForm = emailForm(isAgent = false).form,
    postAction = testCall)(
    request,
    messagesApi.preferred(request),
    appConfig
  )

  "The Direct Debit Capture Email view" should {

    val testPage = TestView(
      name = "Capture Email View",
      title = messages.title,
      heading = messages.heading,
      page = DDpage
    )

    testPage.shouldHaveParaSeq(
      messages.line1,
      messages.line2,
      messages.line3,
      messages.disclaimer
    )

    testPage.shouldHaveHint(
      messages.hint
    )

    testPage.shouldHaveForm("Email Form")(actionCall = testCall)

    testPage.shouldHaveTextField(email, messages.heading, hideLabel = false)

    testPage.shouldHaveContinueButton()

    testPage.shouldHaveALink("disclaimer", messages.link, appConfig.disclaimer)
  }

  lazy val page: HtmlFormat.Appendable = view(
    hasDirectDebit = false,
    emailForm = emailForm(isAgent = false).form,
    postAction = testCall)(
    request,
    messagesApi.preferred(request),
    appConfig
  )

  "The standard Capture Email view" should {

    val testPage = TestView(
      name = "Capture Email View",
      title = messages.title,
      heading = messages.heading,
      page = page
    )

    testPage.shouldHaveParaSeq(
      messages.line1,
      messages.disclaimer
    )

    testPage.shouldHaveHint(
      messages.hint
    )

    testPage.shouldHaveForm("Email Form")(actionCall = testCall)

    testPage.shouldHaveTextField(email, messages.heading, hideLabel = false)

    testPage.shouldHaveContinueButton()

    testPage.shouldHaveALink("disclaimer", messages.link, appConfig.disclaimer)
  }

}

