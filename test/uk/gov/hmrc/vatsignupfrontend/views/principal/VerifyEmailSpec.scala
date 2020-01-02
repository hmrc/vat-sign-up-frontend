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

package uk.gov.hmrc.vatsignupfrontend.views.principal

import play.api.i18n.Messages.Implicits._
import play.api.i18n.MessagesApi
import play.api.test.FakeRequest
import play.api.{Configuration, Environment}
import uk.gov.hmrc.vatsignupfrontend.assets.MessageLookup.{PrincipalVerifyEmail => messages}
import uk.gov.hmrc.vatsignupfrontend.config.AppConfig
import uk.gov.hmrc.vatsignupfrontend.controllers.principal.routes
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.views.ViewSpec

class VerifyEmailSpec extends ViewSpec {

  val env = Environment.simple()
  val configuration = Configuration.load(env)

  lazy val messagesApi = app.injector.instanceOf[MessagesApi]

  lazy val page = uk.gov.hmrc.vatsignupfrontend.views.html.principal.verify_email(
    email = testEmail)(
    FakeRequest(),
    applicationMessages,
    new AppConfig(configuration, env)
  )

  "The Principal Verify email view" should {

    val testPage = TestView(
      name = "Principal Verify Email View",
      title = messages.title,
      heading = messages.heading,
      page = page
    )

    testPage.shouldHaveParaSeq(
      messages.line1(testEmail)
    )

    testPage.shouldHaveParaSeq(
      messages.line1(testEmail),
      messages.line2
    )

    testPage.shouldHaveAccordion(
      heading = messages.accordionHeading,
      text = messages.accordionText
    )

    testPage.shouldHaveALink("change", messages.linkText1, routes.CaptureEmailController.show().url)

    testPage.shouldHaveALink("resend", messages.linkText2, routes.ConfirmEmailController.show().url)

  }

}

