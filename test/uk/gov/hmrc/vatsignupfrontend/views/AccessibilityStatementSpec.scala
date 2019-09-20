/*
 * Copyright 2019 HM Revenue & Customs
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

package uk.gov.hmrc.vatsignupfrontend.views

import org.jsoup.Jsoup
import play.api.{Configuration, Environment}
import play.api.i18n.Messages.Implicits.applicationMessages
import play.api.i18n.{DefaultMessagesApi, Messages, MessagesApi}
import play.api.test.FakeRequest
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.vatsignupfrontend.config.AppConfig
import uk.gov.hmrc.vatsignupfrontend.views.html.help.accessibility_statement

class AccessibilityStatementSpec extends ViewSpec {

  object Messages {
    val title = "Accessibility statement for signing up to Making Tax Digital for VAT"
    val heading = "Accessibility statement for signing up to Making Tax Digital for VAT"
    val introP1 = "This accessibility statement explains how accessible this service is, what to do if you have difficulty using it, and how to report accessibility problems with the service."
    val introP2 = "This service is part of the wider GOV.UK website. There is a separate accessibility statement for the main GOV.UK website."
    val introP3 = "This page only contains information about signing up for the Making Tax digital for VAT service, available at https://www.tax.service.gov.uk/vat-through-software/sign-up."
    val serviceH2 = "Using this service"
    val serviceP1 = "This service enables VAT registered businesses, and their agents, to sign up to Making Tax Digital for VAT so they can use software compatible with HMRC to manage and submit their VAT Returns."
    val serviceP2 = "This service is run by HM Revenue and Customs (HMRC). We want as many people as possible to be able to use this service. This means you should be able to:"
    val serviceLi1 = "change colours, contrast levels and fonts"
    val serviceLi2 = "zoom in up to 300% without the text spilling off the screen"
    val serviceLi3 = "get from the start of the service to the end using just a keyboard"
    val serviceLi4 = "get from the start of the service to the end using speech recognition software"
    val serviceLi5 = "listen to the service using a screen reader (including the most recent versions of JAWS, NVDA and VoiceOver)"
    val serviceP3 = "We have also made the text in the service as simple as possible to understand."
    val serviceP4 = "AbilityNet has advice on making your device easier to use if you have a disability."
    val howAccessH2 = "How accessible this service is"
    val howAccessP1 = "This service is fully compliant with the Web Content Accessibility Guidelines version 2.1 AA standard."
    val howAccessP2 = "There are no known accessibility issues within this service."
    val reportProblemH2 = "Reporting accessibility problems with this service"
    val reportProblemP1 = "We are always looking to improve the accessibility of this service. If you find any problems that are not listed on this page or think we are not meeting accessibility requirements, contact hmrc-accessibility-problems@digital.hmrc.gov.uk."
    val howToDoH2 = "What to do if you are not happy with how we respond to your complaint"
    val whatToDoP1 = "The Equality and Human Rights Commission (EHRC) is responsible for enforcing the Public Sector Bodies (Websites and Mobile Applications) (No. 2) Accessibility Regulations 2018 (the 'accessibility regulations'). If you are not happy with how we respond to your complaint, contact the Equality Advisory and Support Service (EASS), or the Equality Commission for Northern Ireland (ECNI) if you live in Northern Ireland."
    val contactUsH2 = "Contacting us by phone or getting a visit from us in person"
    val contactUsP1 = "We provide a text relay service if you are deaf, hearing impaired or have a speech impediment."
    val contactUsP2 = "We can provide a British Sign Language (BSL) interpreter, or you can arrange a visit from an HMRC advisor to help you complete the service."
    val contactUsP3 = "Find out how to contact us."
    val technicalInfoH2 = "Technical information about this service's accessibility"
    val technicalInfoP1 = "HMRC is committed to making this service accessible, in accordance with the Public Sector Bodies (Websites and Mobile Applications) (No. 2) Accessibility Regulations 2018."
    val technicalInfoP2 = "This service is fully compliant with the Web Content Accessibility Guidelines version 2.1 AA standard."
    val howWeTestH2 = "How we tested this service"
    val howWeTestP1 = "The service was last tested on 11 September 2019 and was checked for compliance with WCAG 2.1 AA."
    val howWeTestP2 = "The service was built using parts that were tested by the Digital Accessibility Centre. The full service was tested by HMRC and included disabled users."
    val howWeTestP3 = "This page was prepared on 19 September 2019. It was last updated on 19 September 2019."


    val accessibilityStatementLinkText = "accessibility statement"
    val serviceLinkText = "https://www.tax.service.gov.uk/vat-through-software/sign-up"
    val abilityNetLinkText = "AbilityNet"
    val wcagLinkText = "Web Content Accessibility Guidelines version 2.1 AA standard"
    val contactUsLinkText = "contact us"
    val eassLinkText = "contact the Equality Advisory and Support Service"
    val ecniLinkText = "Equality Commission for Northern Ireland"
    val dacLinkText = "Digital Accessibility Centre"
    val accessibilityLink = "Accessibility"
  }


  val env: Environment = Environment.simple()
  val configuration: Configuration = Configuration.load(env)

  lazy val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]

  lazy val page: HtmlFormat.Appendable = accessibility_statement(
  )(
    FakeRequest(),
    applicationMessages,
    new AppConfig(configuration, env)
  )


  "The accessibility page" should {
    lazy val parsePage = Jsoup.parse(page.body)
    lazy val pageBody = parsePage.getElementById("content")


    "have a heading" in {
      pageBody.select("h1").text shouldBe Messages.heading
    }

    "have a title" in {
      parsePage.title shouldBe Messages.title
    }

    "have multiple h2" in {
      pageBody.select("h2").text() shouldBe Seq(
        Messages.serviceH2,
        Messages.howAccessH2,
        Messages.reportProblemH2,
        Messages.howToDoH2,
        Messages.contactUsH2,
        Messages.technicalInfoH2,
        Messages.howWeTestH2
      ).mkString(" ")
    }

    "have multiple bullet points" in {
      pageBody.select("li").text() shouldBe Seq(
        Messages.serviceLi1,
        Messages.serviceLi2,
        Messages.serviceLi3,
        Messages.serviceLi4,
        Messages.serviceLi5
      ).mkString(" ")
    }

    "have multiple paragraphs" in {
      pageBody.select("p").text() shouldBe Seq(
        Messages.introP1,
        Messages.introP2,
        Messages.introP3,
        Messages.serviceP1,
        Messages.serviceP2,
        Messages.serviceP3,
        Messages.serviceP4,
        Messages.howAccessP1,
        Messages.howAccessP2,
        Messages.reportProblemP1,
        Messages.whatToDoP1,
        Messages.contactUsP1,
        Messages.contactUsP2,
        Messages.contactUsP3,
        Messages.technicalInfoP1,
        Messages.technicalInfoP2,
        Messages.howWeTestP1,
        Messages.howWeTestP2,
        Messages.howWeTestP3
      ).mkString(" ")
    }

    "have a link to the govuk accessibility statement" in {
      pageBody.select(s"a[href=https://www.gov.uk/help/accessibility]").text shouldBe Messages.accessibilityStatementLinkText
    }

    "have a link to the service" in {
      pageBody.select(s"a[href=https://www.tax.service.gov.uk/vat-through-software/sign-up]").text shouldBe Messages.serviceLinkText
    }

    "have a link to ability net" in {
      pageBody.select(s"a[href=https://mcmw.abilitynet.org.uk/]").text shouldBe Messages.abilityNetLinkText
    }

    "have two links to the accessibility guidelines" in {
      pageBody.select(s"a[href=https://www.w3.org/TR/WCAG21/]").text shouldBe Seq(
        Messages.wcagLinkText,
        Messages.wcagLinkText
      ).mkString(" ")
    }

    "have a link to contact us" in {
      pageBody.select(s"a[href=https://www.gov.uk/dealing-hmrc-additional-needs]").text shouldBe Messages.contactUsLinkText
    }

    "have a link to EASS" in {
      pageBody.select(s"a[href=https://www.equalityadvisoryservice.com/]").text shouldBe Messages.eassLinkText
    }

    "have a link to ECNI" in {
      pageBody.select(s"a[href=https://www.equalityni.org/Home]").text shouldBe Messages.ecniLinkText
    }

    "have a link to DAC" in {
      pageBody.select(s"a[href=http://www.digitalaccessibilitycentre.org/]").text shouldBe Messages.dacLinkText
    }

    "have the correct footer link" in {
      parsePage.select(s"a[href=/vat-through-software/sign-up/accessibility-statement]").text shouldBe Messages.accessibilityLink
    }
  }
}
