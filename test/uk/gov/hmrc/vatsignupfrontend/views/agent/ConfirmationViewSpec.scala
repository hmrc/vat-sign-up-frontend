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

package uk.gov.hmrc.vatsignupfrontend.views.agent

import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import play.api.i18n.MessagesApi
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.vatsignupfrontend.assets.MessageLookup.AgentInformationReceived.Section3
import uk.gov.hmrc.vatsignupfrontend.assets.MessageLookup.{Base, AgentInformationReceived => messages}
import uk.gov.hmrc.vatsignupfrontend.config.AppConfig
import uk.gov.hmrc.vatsignupfrontend.models.SoleTrader
import uk.gov.hmrc.vatsignupfrontend.views.ViewSpec
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants
import uk.gov.hmrc.vatsignupfrontend.views.html.agent.confirmation

class ConfirmationViewSpec extends ViewSpec {

  val view = app.injector.instanceOf[confirmation]
  lazy val appConfig: AppConfig = app.injector.instanceOf[AppConfig]
  lazy val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]
  val request: FakeRequest[AnyContentAsEmpty.type] = FakeRequest()

  lazy val page: HtmlFormat.Appendable = view(
    SoleTrader,
    vatRegistrationNumber = TestConstants.testVatNumber,
    postAction = testCall)(
    request,
    messagesApi.preferred(request),
    appConfig
  )

  lazy val document: Document = Jsoup.parse(page.body)

  "The Confirmation view" should {

    s"have the title '${messages.title}'" in {
      document.title() should be(messages.title)
    }

    "have the business entity type on a data attribute embedded exactly once" in {
      document.select(s"""[data-entity-type="${SoleTrader.toString}"]""").size() shouldBe 1
    }

    "have a confirmation banner" which {

      s"has a heading (H1)" which {

        s"has the text '${messages.heading}'" in {
          document.select("h1").text() shouldBe messages.heading
        }
      }

    }

    "have a 'What happens next' section" which {

      s"has the section heading '${messages.Section1.heading}'" in {
        document.select("#what-happens-next h2").text() shouldBe messages.Section1.heading
      }

      s"has a paragraph stating HMRC process '${messages.Section1.line1}'" in {
        document.select("#what-happens-next p").text() should include(messages.Section1.line1)
      }

      s"has a bullet point '${messages.Section1.bullet1}'" in {
        document.select("#what-happens-next li").text() should include(messages.Section1.bullet1)
      }

      s"has a bullet point '${messages.Section1.bullet2}'" in {
        document.select("#what-happens-next li").text() should include(messages.Section1.bullet2)
      }

      s"has a bullet point '${messages.Section1.bullet3}'" in {
        document.select("#what-happens-next li").text() should include(messages.Section1.bullet3)
      }

    }

    " Have a 'what you must do next' section" which {

      s"has the section heading '${messages.Section2.heading}'" in {
        document.select("#what-you-must-do-next h2").text() shouldBe messages.Section2.heading
      }

      s"has a bullet point '${messages.Section2.bullet1}'" in {
        document.select("#what-you-must-do-next li").text() should include(messages.Section2.bullet1)
      }

      s"has a bullet point '${messages.Section2.bullet2}'" in {
        document.select("#what-you-must-do-next li").text() should include(messages.Section2.bullet2)
      }

      s"has a bullet point '${messages.Section2.bullet3}'" in {
        document.select("#what-you-must-do-next li").text() should include(messages.Section2.bullet3)
      }

      s"has a bullet point '${messages.Section2.bullet4}'" in {
        document.select("#what-you-must-do-next li").text() should include(messages.Section2.bullet4)
      }

    }

    "have a add another client button" in {
      val b = document.getElementById("add-another-button")
      b.attr("value") shouldBe Base.signUpAnotherClient
    }

  }
}
