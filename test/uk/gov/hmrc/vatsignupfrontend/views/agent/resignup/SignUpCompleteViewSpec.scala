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

package uk.gov.hmrc.vatsignupfrontend.views.agent.resignup

import org.jsoup.Jsoup
import play.api.i18n.MessagesApi
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import uk.gov.hmrc.vatsignupfrontend.assets.MessageLookup.{AgentResignupInformationReceived => messages}
import uk.gov.hmrc.vatsignupfrontend.config.AppConfig
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants.testVatNumber
import uk.gov.hmrc.vatsignupfrontend.models.SoleTrader
import uk.gov.hmrc.vatsignupfrontend.views.ViewSpec
import uk.gov.hmrc.vatsignupfrontend.views.html.agent.resignup.sign_up_complete


class SignUpCompleteViewSpec extends ViewSpec {

  val view = app.injector.instanceOf[sign_up_complete]
  lazy val appConfig: AppConfig = app.injector.instanceOf[AppConfig]
  lazy val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]
  val request: FakeRequest[AnyContentAsEmpty.type] = FakeRequest()

  lazy val page = view(SoleTrader, testVatNumber, testCall)(
    request,
    messagesApi.preferred(request),
    appConfig
  )

  lazy val document = Jsoup.parse(page.body)

  "The sign up complete view" should {

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

    "display the user's VAT number" in {
      document.select("#vat-registration-number").text() shouldBe messages.vatNumber + " " + testVatNumber
    }

    s"have a paragraph '${messages.Section.line}'" in {
      document.select("p").text() should include(messages.Section.line)
    }

    "have a 'What happens next' section" which {

      s"has the section heading '${messages.Section.heading}'" in {
        document.select("#what-happens-next h2").text() shouldBe messages.Section.heading
      }

      s"has a bullet point '${messages.Section.bullet1}'" in {
        document.select("#what-happens-next li").text() should include(messages.Section.bullet1)
      }

      s"has a bullet point '${messages.Section.bullet2}'" in {
        document.select("#what-happens-next li").text() should include(messages.Section.bullet2)
      }

      s"has a bullet point '${messages.Section.bullet3}'" in {
        document.select("#what-happens-next li").text() should include(messages.Section.bullet3)
      }

      s"has a bullet point '${messages.Section.s2bullet1}'" in {
        document.select("#what-you-must-do-next li").text() should include(messages.Section.s2bullet1)
      }

      s"has a bullet point '${messages.Section.s2bullet2}'" in {
        document.select("#what-you-must-do-next li").text() should include(messages.Section.s2bullet2)
      }

      s"has a bullet point '${messages.Section.s2bullet3}'" in {
        document.select("#what-you-must-do-next li").text() should include(messages.Section.s2bullet3)
      }
      s"has a bullet point '${messages.Section.s2bullet4}'" in {
        document.select("#what-you-must-do-next li").text() should include(messages.Section.s2bullet4)
      }

    }
    s"have a continue button '${messages.buttonText}' with " in {
      val continueButton = document.getElementById("add-another-button")
      continueButton.attr("value") shouldBe messages.buttonText
    }
  }
}
