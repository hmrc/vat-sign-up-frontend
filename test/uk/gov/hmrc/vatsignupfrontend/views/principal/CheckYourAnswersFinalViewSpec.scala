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

package uk.gov.hmrc.vatsignupfrontend.views.principal

import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import play.api.i18n.Messages.Implicits.applicationMessages
import play.api.test.FakeRequest
import play.api.{Configuration, Environment}
import play.twirl.api.Html
import uk.gov.hmrc.vatsignupfrontend.assets.MessageLookup.{PrincipalCheckYourAnswersFinal => messages}
import uk.gov.hmrc.vatsignupfrontend.config.AppConfig
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants.{testCompanyName, testCompanyNumber, testCompanyUtr, testEmail, testNino, testVatNumber}
import uk.gov.hmrc.vatsignupfrontend.models._
import uk.gov.hmrc.vatsignupfrontend.views.ViewSpec
import uk.gov.hmrc.vatsignupfrontend.views.helpers.CheckYourAnswersIdConstants._

class CheckYourAnswersFinalViewSpec extends ViewSpec {

  val env = Environment.simple()
  val configuration = Configuration.load(env)


  def page(optBusinessEntity: BusinessEntity = Trust,
           optNino: Option[String] = None,
           optPartnershipUtr: Option[String] = None,
           optCompanyNumber: Option[String] = None,
           optCompanyName: Option[String] = None,
           emailAddress: String = testEmail,
           contactPreference: ContactPreference = Digital
          ): Html = uk.gov.hmrc.vatsignupfrontend.views.html.principal.check_your_answers_final(
    vatNumber = testVatNumber,
    businessEntity = optBusinessEntity,
    optNino = optNino,
    optPartnershipUtr = optPartnershipUtr,
    optCompanyNumber = optCompanyNumber,
    optCompanyName = optCompanyName,
    emailAddress = emailAddress,
    contactPreference = contactPreference,
    postAction = testCall
  )(FakeRequest(), applicationMessages, new AppConfig(configuration, env))

  lazy val pageDefault = page()
  lazy val pageSoleTrader = page(optBusinessEntity = SoleTrader, optNino = Some(testNino))
  lazy val pageLimitedCompany = page(
    optBusinessEntity = LimitedCompany,
    optCompanyNumber = Some(testCompanyNumber),
    optCompanyName = Some(testCompanyName)
  )
  lazy val pageLimitedPartnership = page(
    optBusinessEntity = LimitedPartnership,
    optPartnershipUtr = Some(testCompanyUtr),
    optCompanyNumber = Some(testCompanyNumber),
    optCompanyName = Some(testCompanyName)
  )
  lazy val pageGeneralParnership = page(
    optBusinessEntity = GeneralPartnership,
    optPartnershipUtr = Some(testCompanyUtr)
  )
  lazy val pageRegisteredSociety = page(
    optBusinessEntity = RegisteredSociety,
    optCompanyNumber = Some(testCompanyNumber),
    optCompanyName= Some(testCompanyName)
  )
  lazy val pageLetters = page(contactPreference = Paper)

  val questionId: String => String = (sectionId: String) => s"$sectionId-question"
  val answerId: String => String = (sectionId: String) => s"$sectionId-answer"
  val editLinkId: String => String = (sectionId: String) => s"$sectionId-edit"


  def questionStyleCorrectness(section: Element): Unit = {
    section.attr("class") shouldBe "tabular-data__heading tabular-data__heading--label"
  }

  def answerStyleCorrectness(section: Element): Unit = {
    section.attr("class") shouldBe "tabular-data__data-1"
  }

  def editLinkStyleCorrectness(section: Element): Unit = {
    section.attr("class") shouldBe "tabular-data__data-2"
  }

  def sectionTest(page: Html, sectionId: String, expectedQuestion: String, expectedAnswer: String, expectedEditLink: Option[String]): Unit = {
    lazy val doc = Jsoup.parse(page.body)
    val accountingPeriod = doc.getElementById(sectionId)
    val question = doc.getElementById(questionId(sectionId))
    val answer = doc.getElementById(answerId(sectionId))
    val editLink = doc.getElementById(editLinkId(sectionId))

    questionStyleCorrectness(question)
    answerStyleCorrectness(answer)
    if (expectedEditLink.nonEmpty) editLinkStyleCorrectness(editLink)

    question.text() shouldBe expectedQuestion
    answer.text() shouldBe expectedAnswer
    if (expectedEditLink.nonEmpty) {
      editLink.attr("href") shouldBe expectedEditLink.get
      editLink.select("span").text() shouldBe expectedQuestion
      editLink.select("span").hasClass("visuallyhidden") shouldBe true
    }
  }

  "Check your answers final view" should {
    val testPage = TestView(
      name = "Check your answers final view",
      title = messages.title,
      heading = messages.heading,
      page = pageDefault
    )
    testPage.shouldHaveForm("Check your answers final Form")(actionCall = testCall)
    testPage.shouldHaveAcceptAndSendButton()
  }

  "display VAT number" in {
    val expectedEditLink = uk.gov.hmrc.vatsignupfrontend.controllers.principal.routes.CaptureVatNumberController.show().url
    sectionTest(
      page = pageDefault,
      sectionId = VatNumberId,
      expectedQuestion = messages.vat_number,
      expectedAnswer = testVatNumber,
      expectedEditLink = Some(expectedEditLink)
    )
  }
  "display business entity" in {
    val expectedBusinessType = "Trust"
    val expectedEditLink = uk.gov.hmrc.vatsignupfrontend.controllers.principal.routes.CaptureBusinessEntityController.show().url
    sectionTest(
      page = pageDefault,
      sectionId = BusinessEntityId,
      expectedQuestion = messages.business_entity,
      expectedAnswer = expectedBusinessType,
      expectedEditLink = Some(expectedEditLink)
    )
  }
  "display national insurance number" when {
    "business entity is Sole Trader" in {
      val expectedEditLink = uk.gov.hmrc.vatsignupfrontend.controllers.principal.soletrader.routes.CaptureNinoController.show().url
      sectionTest(
        page = pageSoleTrader,
        sectionId = NinoId,
        expectedQuestion = messages.nino,
        expectedAnswer = testNino,
        expectedEditLink = Some(expectedEditLink)
      )
    }
  }
  "display partnership utr" when {
    "business entity is Limited Partnership" in {
      val expectedEditLink = uk.gov.hmrc.vatsignupfrontend.controllers.principal.partnerships.routes.CapturePartnershipUtrController.show().url
      sectionTest(
        page = pageLimitedPartnership,
        sectionId = UtrId,
        expectedQuestion = messages.partnership_utr,
        expectedAnswer = testCompanyUtr,
        expectedEditLink = Some(expectedEditLink)
      )
    }
    "business entity is General Partnership" in {
      val expectedEditLink = uk.gov.hmrc.vatsignupfrontend.controllers.principal.partnerships.routes.CapturePartnershipUtrController.show().url
      sectionTest(
        page = pageGeneralParnership,
        sectionId = UtrId,
        expectedQuestion = messages.partnership_utr,
        expectedAnswer = testCompanyUtr,
        expectedEditLink = Some(expectedEditLink)
      )
    }
  }

  "display company number" when {
    "business entity is Limited Company" in {
      val expectedEditLink = uk.gov.hmrc.vatsignupfrontend.controllers.principal.routes.CaptureCompanyNumberController.show().url
      sectionTest(
        page = pageLimitedCompany,
        sectionId = CrnId,
        expectedQuestion = messages.company_number,
        expectedAnswer = testCompanyNumber,
        expectedEditLink = Some(expectedEditLink)
      )
    }
    "business entity is Limited Partnership" in {
      val expectedEditLink = uk.gov.hmrc.vatsignupfrontend.controllers.principal.partnerships.routes.CapturePartnershipCompanyNumberController.show().url
      sectionTest(
        page = pageLimitedPartnership,
        sectionId = CrnId,
        expectedQuestion = messages.partnership_number,
        expectedAnswer = testCompanyNumber,
        expectedEditLink = Some(expectedEditLink)
      )
    }
    "business entity is Registered Society" in {
      val expectedEditLink = uk.gov.hmrc.vatsignupfrontend.controllers.principal.routes.CaptureRegisteredSocietyCompanyNumberController.show().url
      sectionTest(
        page = pageRegisteredSociety,
        sectionId = CrnId,
        expectedQuestion = messages.company_number,
        expectedAnswer = testCompanyNumber,
        expectedEditLink = Some(expectedEditLink)
      )
    }
  }

  "display company name" when {
    "business entity is Limited Company" in {
      sectionTest(
        page = pageLimitedCompany,
        sectionId = CompanyNameId,
        expectedQuestion = messages.company_name,
        expectedAnswer = testCompanyName,
        expectedEditLink = None
      )
    }
    "business entity is Limited Partnership" in {
      sectionTest(
        page = pageLimitedPartnership,
        sectionId = CompanyNameId,
        expectedQuestion = messages.partnership_name,
        expectedAnswer = testCompanyName,
        expectedEditLink = None
      )
    }
    "business entity is Registered Society" in {
      sectionTest(
        page = pageRegisteredSociety,
        sectionId = CompanyNameId,
        expectedQuestion = messages.registered_society_name,
        expectedAnswer = testCompanyName,
        expectedEditLink = None
      )
    }
  }
  "display email address" in {
    val expectedEditLink = uk.gov.hmrc.vatsignupfrontend.controllers.principal.routes.CaptureEmailController.show().url
    sectionTest(
      page = pageDefault,
      sectionId = EmailAddressId,
      expectedQuestion = messages.email_address,
      expectedAnswer = testEmail,
      expectedEditLink = Some(expectedEditLink)
    )
  }
  "display contact preference" when {
    "answer is Digital" in {
      val expectedEditLink = uk.gov.hmrc.vatsignupfrontend.controllers.principal.routes.ReceiveEmailNotificationsController.show().url
      sectionTest(
        page = pageDefault,
        sectionId = ContactPreferenceId,
        expectedQuestion = messages.contact_preference,
        expectedAnswer = messages.digital,
        expectedEditLink = Some(expectedEditLink)
      )
    }
    "answer is Paper" in {
      val expectedEditLink = uk.gov.hmrc.vatsignupfrontend.controllers.principal.routes.ReceiveEmailNotificationsController.show().url
      sectionTest(
        page = pageLetters,
        sectionId = ContactPreferenceId,
        expectedQuestion = messages.contact_preference,
        expectedAnswer = messages.letter,
        expectedEditLink = Some(expectedEditLink)
      )
    }
  }
}
