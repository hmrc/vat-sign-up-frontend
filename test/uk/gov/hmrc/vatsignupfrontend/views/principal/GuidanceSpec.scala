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

import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import play.api.i18n.MessagesApi
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.vatsignupfrontend.assets.MessageLookup.{Base, PrincipalGuidance => messages}
import uk.gov.hmrc.vatsignupfrontend.config.AppConfig
import uk.gov.hmrc.vatsignupfrontend.controllers.principal.routes
import uk.gov.hmrc.vatsignupfrontend.views.ViewSpec
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.guidance

class GuidanceSpec extends ViewSpec {

  val view = app.injector.instanceOf[guidance]
  lazy val appConfig: AppConfig = app.injector.instanceOf[AppConfig]
  lazy val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]
  val request: FakeRequest[AnyContentAsEmpty.type] = FakeRequest()

  lazy val page: HtmlFormat.Appendable = view()(
    request,
    messagesApi.preferred(request),
    appConfig
  )

  lazy val document: Document = Jsoup.parse(page.body)

  "The Guidance view" should {

    val testPage = TestView(
      name = "Guidance View",
      title = messages.title,
      heading = messages.heading,
      page = page,
      haveSignOutInBanner = false
    )

    testPage.shouldHaveParaSeq(
      messages.line1,
      messages.line2,
      messages.line3,
      messages.line4
    )

    "have a 'How it works' section" which {

      s"has the section heading '${messages.Section1.heading}'" in {
        document.select("#how-it-works h2").text() shouldBe messages.Section1.heading
      }

      s"has a paragraph stating sign up service details '${messages.Section1.line1}'" in {
        document.select("#how-it-works p").text() should include(messages.Section1.line1)
      }

      s"has a 1st numeric point '${messages.Section1.number1}'" in {
        document.select("#how-it-works li").text() should include(messages.Section1.number1)
      }

      s"has a 2nd numeric point '${messages.Section1.number2}'" in {
        document.select("#how-it-works li").text() should include(messages.Section1.number2)
      }

      s"has a 3rd numeric point '${messages.Section1.number3}'" in {
        document.select("#how-it-works li").text() should include(messages.Section1.number3)
      }

      s"has a final numeric point '${messages.Section1.number4}'" in {
        document.select("#how-it-works li").text() should include(messages.Section1.number4)
      }

      s"has a paragraph stating you can also choose to '${messages.Section1.line2}'" in {
        document.select("#how-it-works p").text() should include(messages.Section1.line2)
      }

      s"has a 1st bullet point '${messages.Section1.bullet1}'" in {
        document.select("#how-it-works li").text() should include(messages.Section1.bullet1)
      }

      s"has a 2nd bullet point '${messages.Section1.bullet2}'" in {
        document.select("#how-it-works li").text() should include(messages.Section1.bullet2)
      }

      s"has a 3rd bullet point '${messages.Section1.bullet3}'" in {
        document.select("#how-it-works li").text() should include(messages.Section1.bullet3)
      }
    }

    "have a 'Sign up' section" which {

      s"has the section heading '${messages.Section2.heading}'" in {
        document.select("#sign-up h2").text() shouldBe messages.Section2.heading
      }

      s"has a paragraph stating sign up details '${messages.Section2.line1}'" in {
        document.select("#sign-up p").text() should include(messages.Section2.line1)
      }

      s"has a paragraph stating submit VAT return '${messages.Section2.line2}'" in {
        document.select("#sign-up p").text() should include(messages.Section2.line2)
      }

    }

    testPage.shouldHaveALink("start-now", Base.startNow, routes.IndexResolverController.resolve().url)
  }

}

