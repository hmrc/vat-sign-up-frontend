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

package uk.gov.hmrc.vatsignupfrontend.views.principal.partnerships

import play.api.i18n.MessagesApi
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.vatsignupfrontend.assets.MessageLookup.{CapturePartnershipUtr => messages}
import uk.gov.hmrc.vatsignupfrontend.config.AppConfig
import uk.gov.hmrc.vatsignupfrontend.controllers.principal.partnerships.routes
import uk.gov.hmrc.vatsignupfrontend.forms.PartnershipUtrForm._
import uk.gov.hmrc.vatsignupfrontend.views.ViewSpec
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.partnerships.capture_partnership_utr

class CapturePartnershipUtrSpec extends ViewSpec {

  val view = app.injector.instanceOf[capture_partnership_utr]
  lazy val appConfig: AppConfig = app.injector.instanceOf[AppConfig]
  lazy val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]
  val request: FakeRequest[AnyContentAsEmpty.type] = FakeRequest()

  lazy val page: Boolean => HtmlFormat.Appendable = (displayGeneralPartnershipAccordion: Boolean) => {
    view(
      partnershipUtrForm = partnershipUtrForm.form,
      postAction = testCall,
      displayGeneralPartnershipAccordion = displayGeneralPartnershipAccordion
    )(
      request,
      messagesApi.preferred(request),
      appConfig
    )
  }

  s"The Capture Partnership Utr view with showGeneralPartnershipSpecific true" should {
    val testPage = TestView(
      name = "Capture Partnership Utr View",
      title = messages.title,
      heading = messages.heading,
      page = page(true)
    )

    testPage.shouldHaveForm("Partnership Utr Form")(actionCall = testCall)

    testPage.shouldHaveAccordion(messages.accordionHeading, messages.accordionText)

    specificGeneralPartnershipNoSAUTRDrivenElementsShouldChangeForTrue(testPage)

    testPage.shouldHaveTextField(partnershipUtr, messages.heading, hideLabel = false)

    testPage.shouldHavePara(messages.line1)

    testPage.shouldHaveContinueButton()
  }

  s"The Capture Partnership Utr view with showGeneralPartnershipSpecific false" should {
    val testPage = TestView(
      name = "Capture Partnership Utr View",
      title = messages.title,
      heading = messages.heading,
      page = page(false)
    )

    testPage.shouldHaveForm("Partnership Utr Form")(actionCall = testCall)

    testPage.shouldHaveTextField(partnershipUtr, messages.heading, hideLabel = false)

    testPage.shouldHavePara(messages.line1)

    testPage.shouldHaveALink("partnershipUtr-limited-link", messages.cannotFind, appConfig.findLostUtrNumberUrl)

    testPage.shouldHaveContinueButton()

  }

  private def specificGeneralPartnershipNoSAUTRDrivenElementsShouldChangeForTrue(testPage: TestView): Unit = {
    s"hint exists, hrefs for links in summary are correct, line 1 no longer exists in view" in {
      testPage.document.getElementById("partnershipUtr-accordion-link1").attr("href") shouldBe appConfig.findLostUtrNumberUrl
      testPage.document.getElementById("partnershipUtr-accordion-link2").attr("href") shouldBe routes.CapturePartnershipUtrController.noUtrSelected().url
    }
  }
}
