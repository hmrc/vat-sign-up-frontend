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
import play.twirl.api.Html
import uk.gov.hmrc.vatsignupfrontend.assets.MessageLookup.{PrincipalCheckYourAnswersFinal => messages}
import uk.gov.hmrc.vatsignupfrontend.config.AppConfig
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.models._
import uk.gov.hmrc.vatsignupfrontend.utils.SummarySectionTesting
import uk.gov.hmrc.vatsignupfrontend.views.ViewSpec
import uk.gov.hmrc.vatsignupfrontend.views.helpers.CheckYourAnswersIdConstants._
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.check_your_answers_final

class CheckYourAnswersFinalViewSpec extends ViewSpec with SummarySectionTesting {

  val view = app.injector.instanceOf[check_your_answers_final]
  lazy val appConfig: AppConfig = app.injector.instanceOf[AppConfig]
  lazy val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]
  val request: FakeRequest[AnyContentAsEmpty.type] = FakeRequest()

  def page(optBusinessEntity: BusinessEntity = Trust,
           optNino: Option[String] = None,
           optPartnershipUtr: Option[String] = None,
           optCompanyNumber: Option[String] = None,
           optCompanyName: Option[String] = None,
           emailAddress: String = testEmail,
           contactPreference: ContactPreference = Digital,
           isAdministrativeDivision: Boolean = false
          ): Html = view(
    vatNumber = testVatNumber,
    businessEntity = optBusinessEntity,
    optNino = optNino,
    optPartnershipUtr = optPartnershipUtr,
    optCompanyNumber = optCompanyNumber,
    optCompanyName = optCompanyName,
    emailAddress = emailAddress,
    contactPreference = contactPreference,
    postAction = testCall,
    isAdministrativeDivision
  )(
    request,
    messagesApi.preferred(request),
    appConfig
  )

  lazy val pageDefault: Html = page()
  lazy val pageSoleTrader: Html = page(optBusinessEntity = SoleTrader, optNino = Some(testNino))
  lazy val pageLimitedCompany: Html = page(
    optBusinessEntity = LimitedCompany,
    optCompanyNumber = Some(testCompanyNumber),
    optCompanyName = Some(testCompanyName)
  )
  lazy val pageLimitedPartnership: Html = page(
    optBusinessEntity = LimitedPartnership,
    optPartnershipUtr = Some(testCompanyUtr),
    optCompanyNumber = Some(testCompanyNumber),
    optCompanyName = Some(testCompanyName)
  )
  lazy val pageGeneralParnership: Html = page(
    optBusinessEntity = GeneralPartnership,
    optPartnershipUtr = Some(testCompanyUtr)
  )
  lazy val pageRegisteredSociety: Html = page(
    optBusinessEntity = RegisteredSociety,
    optCompanyNumber = Some(testCompanyNumber),
    optCompanyName = Some(testCompanyName)
  )
  lazy val pageLetters: Html = page(contactPreference = Paper)


  def sectionExists(page: Html, sectionId: String): Boolean = {
    lazy val doc = Jsoup.parse(page.body)
    val result = Option(doc.getElementById(sectionId))

    result.isDefined
  }


  "Check your answers final view" should {
    lazy val doc: Document = Jsoup.parse(page().body)
    "display VAT number" in {
      doc.sectionTest(
        VatNumberId,
        messages.vat_number,
        testVatNumber,
        Some(uk.gov.hmrc.vatsignupfrontend.controllers.principal.routes.CaptureVatNumberController.show().url)
      )
    }
    "display business entity" in {
      doc.sectionTest(
        BusinessEntityId,
        messages.business_entity,
        expectedAnswer = "Trust",
        Some(uk.gov.hmrc.vatsignupfrontend.controllers.principal.routes.CaptureBusinessEntityController.show().url)
      )
    }
    "not display business entity" when {
      "business entity is overseas" in {
        sectionExists(page(optBusinessEntity = Overseas), BusinessEntityId) shouldBe false
      }

      "isAdministrativeDivision is true" in {
        sectionExists(page(), BusinessEntityId) shouldBe false
      }
    }

    "display national insurance number" when {
      lazy val doc: Document = Jsoup.parse(pageSoleTrader.body)
      "business entity is Sole Trader" in {
        doc.sectionTest(
          NinoId,
          messages.nino,
          testNino,
          Some(uk.gov.hmrc.vatsignupfrontend.controllers.principal.soletrader.routes.SoleTraderResolverController.resolve().url)
        )
      }
    }
    "display partnership utr" when {
      "business entity is Limited Partnership" in {
        lazy val doc: Document = Jsoup.parse(pageGeneralParnership.body)
        doc.sectionTest(
          UtrId,
          messages.partnership_utr,
          testCompanyUtr,
          Some(uk.gov.hmrc.vatsignupfrontend.controllers.principal.partnerships.routes.ResolvePartnershipUtrController.resolve().url)
        )
      }
      "business entity is General Partnership" in {
        lazy val doc: Document = Jsoup.parse(pageGeneralParnership.body)
        doc.sectionTest(
          UtrId,
          messages.partnership_utr,
          testCompanyUtr,
          Some(uk.gov.hmrc.vatsignupfrontend.controllers.principal.partnerships.routes.ResolvePartnershipUtrController.resolve().url)
        )
      }
    }

    "display company number" when {
      "business entity is Limited Company" in {
        lazy val doc: Document = Jsoup.parse(pageLimitedCompany.body)
        doc.sectionTest(
          CrnId,
          messages.company_number,
          testCompanyNumber,
          Some(uk.gov.hmrc.vatsignupfrontend.controllers.principal.routes.CaptureCompanyNumberController.show().url)
        )
      }
      "business entity is Limited Partnership" in {
        lazy val doc: Document = Jsoup.parse(pageLimitedPartnership.body)
        doc.sectionTest(
          CrnId,
          messages.partnership_company_number,
          testCompanyNumber,
          Some(uk.gov.hmrc.vatsignupfrontend.controllers.principal.partnerships.routes.CapturePartnershipCompanyNumberController.show().url)
        )
      }
      "business entity is Registered Society" in {
        lazy val doc: Document = Jsoup.parse(pageRegisteredSociety.body)
        doc.sectionTest(
          CrnId,
          messages.company_number,
          testCompanyNumber,
          Some(uk.gov.hmrc.vatsignupfrontend.controllers.principal.routes.CaptureRegisteredSocietyCompanyNumberController.show().url)
        )
      }
    }

    "display company name" when {
      "business entity is Limited Company" in {
        lazy val doc: Document = Jsoup.parse(pageLimitedCompany.body)
        doc.sectionTest(
          CompanyNameId,
          messages.company_name,
          testCompanyName,
          None
        )
      }
      "business entity is Limited Partnership" in {
        lazy val doc: Document = Jsoup.parse(pageLimitedPartnership.body)
        doc.sectionTest(
          CompanyNameId,
          messages.partnership_name,
          testCompanyName,
          None
        )
      }
      "business entity is Registered Society" in {
        lazy val doc: Document = Jsoup.parse(pageRegisteredSociety.body)
        doc.sectionTest(
          CompanyNameId,
          messages.registered_society_name,
          testCompanyName,
          None
        )
      }
    }
    "display email address" in {
      doc.sectionTest(
        EmailAddressId,
        messages.email_address,
        testEmail,
        Some(uk.gov.hmrc.vatsignupfrontend.controllers.principal.routes.CaptureEmailController.show().url)
      )
    }
    "display contact preference" when {
      "answer is Digital" in {
        doc.sectionTest(
          ContactPreferenceId,
          messages.contact_preference,
          messages.digital,
          Some(uk.gov.hmrc.vatsignupfrontend.controllers.principal.routes.ReceiveEmailNotificationsController.show().url)
        )
      }
      "answer is Paper" in {
        lazy val doc: Document = Jsoup.parse(pageLetters.body)
        doc.sectionTest(
          ContactPreferenceId,
          messages.contact_preference,
          messages.letter,
          Some(uk.gov.hmrc.vatsignupfrontend.controllers.principal.routes.ReceiveEmailNotificationsController.show().url)
        )
      }
    }
  }
}
