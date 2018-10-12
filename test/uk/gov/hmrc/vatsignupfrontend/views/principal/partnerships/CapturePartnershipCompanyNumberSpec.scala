/*
 * Copyright 2018 HM Revenue & Customs
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

package uk.gov.hmrc.vatsignupfrontend.views.principal.partnerships

import play.api.i18n.Messages.Implicits._
import play.api.i18n.MessagesApi
import play.api.test.FakeRequest
import play.api.{Configuration, Environment}
import uk.gov.hmrc.vatsignupfrontend.assets.MessageLookup.{PrincipalCapturePartnershipCompanyNumber => messages}
import uk.gov.hmrc.vatsignupfrontend.config.AppConfig
import uk.gov.hmrc.vatsignupfrontend.forms.CompanyNumberForm._
import uk.gov.hmrc.vatsignupfrontend.views.ViewSpec


class CapturePartnershipCompanyNumberSpec extends ViewSpec {

  val env = Environment.simple()
  val configuration = Configuration.load(env)

  lazy val messagesApi = app.injector.instanceOf[MessagesApi]
  lazy val appConfig = app.injector.instanceOf[AppConfig]

  lazy val page = uk.gov.hmrc.vatsignupfrontend.views.html.principal.partnerships.capture_partnership_company_number(
    companyNumberForm = companyNumberForm(isAgent = false).form,
    postAction = testCall)(
    FakeRequest(),
    applicationMessages,
    new AppConfig(configuration, env)
  )

  "The Capture Partnership Company Number view" should {

    val testPage = TestView(
      name = "Capture Partnership Company Number View",
      title = messages.title,
      heading = messages.heading,
      page = page
    )

    testPage.shouldHaveForm("Company Number Form")(actionCall = testCall)

    testPage.shouldHaveTextField(companyNumber, messages.heading)

    testPage.shouldHavePara(messages.line1)

    testPage.shouldHaveALink("companiesHouse", messages.linkText, appConfig.companiesHouse)

    testPage.shouldHaveContinueButton()
  }

}

