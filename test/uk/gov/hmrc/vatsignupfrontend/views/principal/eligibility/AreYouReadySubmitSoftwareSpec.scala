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

package uk.gov.hmrc.vatsignupfrontend.views.principal.eligibility

import play.api.data.Form
import play.api.i18n.MessagesApi
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.vatsignupfrontend.assets.MessageLookup
import uk.gov.hmrc.vatsignupfrontend.assets.MessageLookup.{AreYouReadySubmitSoftware => messages}
import uk.gov.hmrc.vatsignupfrontend.config.AppConfig
import uk.gov.hmrc.vatsignupfrontend.forms.eligibility.AreYouReadySubmitSoftwareForm
import uk.gov.hmrc.vatsignupfrontend.models.YesNo
import uk.gov.hmrc.vatsignupfrontend.views.ViewSpec
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.eligibility.are_you_ready_submit_software

class AreYouReadySubmitSoftwareSpec extends ViewSpec {

  val view = app.injector.instanceOf[are_you_ready_submit_software]
  lazy val appConfig: AppConfig = app.injector.instanceOf[AppConfig]
  lazy val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]
  val request: FakeRequest[AnyContentAsEmpty.type] = FakeRequest()

  val page: Form[YesNo] => HtmlFormat.Appendable = (form: Form[YesNo]) =>
    view(
      form,
      postAction = testCall)(
      request,
      messagesApi.preferred(request),
      appConfig
    )

  "Are you ready submit software view render correctly with a form thats empty with no errors" should {
    val testPage = TestView(
      name = "Are you Ready to Submit Software",
      title = messages.title,
      heading = messages.heading,
      page = page(AreYouReadySubmitSoftwareForm.areYouReadySubmitSoftwareForm),
      haveSignOutInBanner = false
    )
    testPage.shouldHavePara(messages.line1)
    testPage.shouldHavePara(messages.line2)
    testPage.shouldHaveForm(formName = s"$AreYouReadySubmitSoftwareForm")(actionCall = testCall)

    testPage.shouldHaveContinueButton()
  }

  "Are you ready submit software view render correctly with a form that has errors" should {
    val form = AreYouReadySubmitSoftwareForm.areYouReadySubmitSoftwareForm.bind(
      Map(AreYouReadySubmitSoftwareForm.yesNo -> ""
      )
    )

    val testPage = TestView(
      name = "Are you Ready to Submit Software",
      title = s"${MessageLookup.Base.errPrefix} ${messages.title}",
      heading = messages.heading,
      page = page(form),
      haveSignOutInBanner = false
    )

    testPage.shouldHaveErrorSummary(messages.errorMessage)
    testPage.shouldHaveFieldError(AreYouReadySubmitSoftwareForm.yesNo, messages.errorMessage)
    testPage.shouldHavePara(messages.line1)
    testPage.shouldHavePara(messages.line2)
    testPage.shouldHaveForm(formName = s"$AreYouReadySubmitSoftwareForm")(actionCall = testCall)

    testPage.shouldHaveContinueButton()
  }
}