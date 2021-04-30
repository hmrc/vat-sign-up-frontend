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
import uk.gov.hmrc.vatsignupfrontend.assets.MessageLookup.{PrincipalDirectDebitTermsAndConditions => messages}
import uk.gov.hmrc.vatsignupfrontend.config.AppConfig
import uk.gov.hmrc.vatsignupfrontend.controllers.principal.routes
import uk.gov.hmrc.vatsignupfrontend.views.ViewSpec
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.direct_debit_terms_and_conditions

class DirectDebitTermsAndConditionsSpec extends ViewSpec {

  val view = app.injector.instanceOf[direct_debit_terms_and_conditions]
  lazy val appConfig: AppConfig = app.injector.instanceOf[AppConfig]
  lazy val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]
  val request: FakeRequest[AnyContentAsEmpty.type] = FakeRequest()

  lazy val page: HtmlFormat.Appendable = view(postAction = testCall)(
    request,
    messagesApi.preferred(request),
    appConfig
  )

  "The Terms and Conditions view" should {

    val testPage = TestView(
      name = "Terms and Conditions View",
      title = messages.title,
      heading = messages.heading,
      page = page
    )

    testPage.shouldHavePara(messages.line)

    testPage.shouldHaveAgreeAndContinueButton()

    testPage.shouldHaveForm("Terms and Conditions Form")(actionCall = testCall)

    testPage.shouldHaveALink(
      id = "termsAndConditionsLink",
      text = messages.link1,
      href = "https://www.tax.service.gov.uk/direct-debit/vat/terms-and-conditions"
    )

    testPage.shouldHaveALink(
      id = "notAgreeLink",
      text = messages.link2,
      href = routes.CancelDirectDebitController.show().url
    )
  }
}

