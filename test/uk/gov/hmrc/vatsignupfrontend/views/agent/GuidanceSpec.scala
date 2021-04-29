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
import uk.gov.hmrc.vatsignupfrontend.assets.MessageLookup.{Base, AgentGuidance => messages}
import uk.gov.hmrc.vatsignupfrontend.config.AppConfig
import uk.gov.hmrc.vatsignupfrontend.controllers.agent.routes
import uk.gov.hmrc.vatsignupfrontend.views.ViewSpec
import uk.gov.hmrc.vatsignupfrontend.views.html.agent.guidance

class GuidanceSpec extends ViewSpec {

  val view = app.injector.instanceOf[guidance]
  lazy val appConfig: AppConfig = app.injector.instanceOf[AppConfig]
  lazy val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]
  val request: FakeRequest[AnyContentAsEmpty.type] = FakeRequest()

  lazy val page: HtmlFormat.Appendable = view(
  )(
    request,
    messagesApi.preferred(request),
    appConfig
  )

  object ExternalUrls {
    val govUkUrl = "https://www.gov.uk/government/organisations/hm-revenue-customs/contact/vat-enquiries"
    val agentServicesUrl = "https://www.tax.service.gov.uk/agent-subscription/start"
  }

  lazy val document: Document = Jsoup.parse(page.body)

  "The guidance view" should {

    import ExternalUrls._

    s"have the title '${messages.title}'" in {
      document.title() should be(messages.title)
    }

    s"has a paragraph stating what you need before you start'${messages.line1}'" in {
      document.select("#desc-one").text() should include(messages.line1)
    }

    s"has a paragraph stating what you need before you start'${messages.line2}'" in {
      document.select("#desc-two").text() should include(messages.line2)
    }

    s"has a bullet point '${messages.bullet1}'" in {
      document.select("#bullet-one").text() should include(messages.bullet1)
    }

    s"has a bullet point '${messages.bullet2}'" in {
      document.select("#bullet-two").text() should include(messages.bullet2)
    }

    s"has a bullet point '${messages.bullet3}'" in {
      document.select("#bullet-three").text() should include(messages.bullet3)
    }

    "have a 'Before you start' section" which {

      s"has the section heading '${messages.subHeadingBeforeYouStart}'" in {
        document.select("#heading-before-you-start").text() shouldBe messages.subHeadingBeforeYouStart
      }

      s"has a paragraph stating what you need before you start'${messages.beforeYouStartLine1}'" in {
        document.select("#before-you-start-desc").text() should include(messages.beforeYouStartLine1)
      }

      s"has a bullet point '${messages.beforeYouStartBullet1}'" in {
        document.select("#before-you-start-bullet-one").text() should include(messages.beforeYouStartBullet1)
      }

      s"has a bullet point '${messages.beforeYouStartBullet2}'" in {
        document.select("#before-you-start-bullet-two").text() should include(messages.beforeYouStartBullet2)
      }

    }

    "have a 'Sign up your clients' section" which {

      s"has the section heading '${messages.subHeadingSignUpYourClients}'" in {
        document.select("#heading-sign-up-your-clients").text() shouldBe messages.subHeadingSignUpYourClients
      }

      s"has a paragraph describing signing up a client'${messages.signUpYourClientsLine1}'" in {
        document.select("#sign-up-your-clients-desc-one").text() should include(messages.signUpYourClientsLine1)
      }

      s"has a paragraph describing signing up a client'${messages.signUpYourClientsLine2}'" in {
        document.select("#sign-up-your-clients-desc-two").text() should include(messages.signUpYourClientsLine2)
      }
    }


    "have a 'Software' section" which {

      s"has the section heading '${messages.subHeadingSoftware}'" in {
        document.select("#heading-software").text() shouldBe messages.subHeadingSoftware
      }

      s"has a paragraph describing software'${messages.softwareLine1}'" in {
        document.select("#software-desc-one").text() should include(messages.softwareLine1)
      }

      s"has a paragraph describing software'${messages.softwareLine2}'" in {
        document.select("#software-desc-two").text() should include(messages.softwareLine2)
      }
    }

    "have a 'Get help' section" which {

      s"has the section heading '${messages.subHeadingGetHelp}'" in {
        document.select("#heading-get-help").text() shouldBe messages.subHeadingGetHelp
      }

      s"has a paragraph describing software'${messages.getHelpLine1}'" in {
        document.select("#get-help-desc-one").text() should include(messages.getHelpLine1)
      }

      s"has a paragraph describing software'${messages.getHelpLine2}'" in {
        document.select("#get-help-desc-two").text() should include(messages.getHelpLine2)
      }
    }

    s"have a button with text 'Start now' pointed to the first page in the agent journey" in {
      val link = document.getElementById("start-now")
      link.attr("href") shouldBe routes.ConfirmVatNumberController.show().url
      link.text() shouldBe Base.startNow
    }

    s"have a link with text 'HMRC' pointed to 'Gov Uk'" in {
      val link = document.getElementById("hmrc")
      link.attr("href") shouldBe govUkUrl
      link.text() shouldBe "HMRC"
    }

    s"have a link with text 'set up an agent services account' pointed to 'Agent Services'" in {
      val link = document.getElementById("agent_services")
      link.attr("href") shouldBe agentServicesUrl
      link.text() shouldBe "set up an agent services account"
    }

  }
}
