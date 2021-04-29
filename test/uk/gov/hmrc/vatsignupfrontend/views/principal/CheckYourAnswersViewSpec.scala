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

import java.time.LocalDate

import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import play.api.i18n.MessagesApi
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.twirl.api.Html
import uk.gov.hmrc.vatsignupfrontend.assets.MessageLookup.{PrincipalCheckYourAnswers => messages}
import uk.gov.hmrc.vatsignupfrontend.config.AppConfig
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.models._
import uk.gov.hmrc.vatsignupfrontend.utils.SummarySectionTesting
import uk.gov.hmrc.vatsignupfrontend.views.ViewSpec
import uk.gov.hmrc.vatsignupfrontend.views.helpers.CheckYourAnswersIdConstants._
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.check_your_answers

class CheckYourAnswersViewSpec extends ViewSpec with SummarySectionTesting {

  val testRegistrationDate: DateModel = DateModel.dateConvert(LocalDate.now())
  val testEntity: BusinessEntity = SoleTrader

  val view = app.injector.instanceOf[check_your_answers]
  lazy val appConfig: AppConfig = app.injector.instanceOf[AppConfig]
  lazy val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]
  val request: FakeRequest[AnyContentAsEmpty.type] = FakeRequest()

  def page(optBox5Figure: Option[String] = None,
           optLastReturnMonthPeriod: Option[String] = None,
           optPreviousVatReturn: Option[String] = None,
           optPostCode: Option[PostCode] = Some(testBusinessPostcode)
          ): Html = view(
    vatNumber = testVatNumber,
    registrationDate = testRegistrationDate,
    optPostCode = optPostCode,
    optPreviousVatReturn = optPreviousVatReturn,
    optBox5Figure = optBox5Figure,
    optLastReturnMonthPeriod = optLastReturnMonthPeriod,
    postAction = testCall
  )(
    request,
    messagesApi.preferred(request),
    appConfig
  )

  lazy val doc: Document = Jsoup.parse(page().body)

  lazy val pageDefault: Html = page()

  lazy val pageWithAdditionalKnownFacts: Html = page(Some(testBox5Figure), Some(testLastReturnMonthPeriod), Some(Yes.stringValue))

  lazy val pageWithoutPostCode: Html = page(Some(testBox5Figure), Some(testLastReturnMonthPeriod), Some(Yes.stringValue), None)

  "Check your answers view" should {
    lazy val pageWithAdditionalKnownFacts: Html = page(Some(testBox5Figure), Some(testLastReturnMonthPeriod), Some(Yes.stringValue))

    lazy val doc: Document = Jsoup.parse(pageWithAdditionalKnownFacts.body)


    "display the correct info for VatNumber" in {
      doc.sectionTest(
        VatNumberId,
        messages.yourVatNumber,
        testVatNumber,
        Some(uk.gov.hmrc.vatsignupfrontend.controllers.principal.routes.CaptureVatNumberController.show().url)
      )
    }

    "display the correct info for VatRegistrationDate" in {
      doc.sectionTest(
        VatRegistrationDateId,
        messages.vatRegistrationDate,
        testRegistrationDate.toCheckYourAnswersDateFormat,
        Some(uk.gov.hmrc.vatsignupfrontend.controllers.principal.routes.CaptureVatRegistrationDateController.show().url)
      )
    }

    "display the correct info for BusinessPostCode" in {
      doc.sectionTest(
        BusinessPostCodeId,
        messages.businessPostCode,
        testBusinessPostcode.checkYourAnswersFormat,
        Some(uk.gov.hmrc.vatsignupfrontend.controllers.principal.routes.BusinessPostCodeController.show().url)
      )
    }

    "Check your answers view with additional known facts" should {
      lazy val pageWithAdditionalKnownFacts: Html = page(Some(testBox5Figure), Some(testLastReturnMonthPeriod), Some(Yes.stringValue))

      lazy val doc: Document = Jsoup.parse(pageWithAdditionalKnownFacts.body)
      "display the correct info for VatNumber" in {
        doc.sectionTest(
          VatNumberId,
          messages.yourVatNumber,
          testVatNumber,
          Some(uk.gov.hmrc.vatsignupfrontend.controllers.principal.routes.CaptureVatNumberController.show().url)
        )
      }

      "display the correct info for VatRegistrationDate" in {
        doc.sectionTest(
          VatRegistrationDateId,
          messages.vatRegistrationDate,
          testRegistrationDate.toCheckYourAnswersDateFormat,
          Some(uk.gov.hmrc.vatsignupfrontend.controllers.principal.routes.CaptureVatRegistrationDateController.show().url)
        )
      }

      "display the correct info for BusinessPostCode" in {
        doc.sectionTest(
          BusinessPostCodeId,
          messages.businessPostCode,
          testBusinessPostcode.checkYourAnswersFormat,
          Some(uk.gov.hmrc.vatsignupfrontend.controllers.principal.routes.BusinessPostCodeController.show().url)
        )
      }
    }

    "display the correct answer for Previous VAT return" in {
      doc.sectionTest(
        PreviousVatReturnId,
        messages.previousVatReturn,
        Yes.stringValue,
        Some(uk.gov.hmrc.vatsignupfrontend.controllers.principal.routes.PreviousVatReturnController.show().url)
      )
    }

    "display the correct answer for Box5Value" in {
      doc.sectionTest(
        VatBox5FigureId,
        messages.box5Figure,
        expectedAnswer = s"£99,999,999,999.99",
        Some(uk.gov.hmrc.vatsignupfrontend.controllers.principal.routes.CaptureBox5FigureController.show().url)
      )
    }

    "display the correct answer for LastReturnMonth" in {
      doc.sectionTest(
        VatLastReturnMonthId,
        messages.lastReturnMonth,
        testLastReturnMonthPeriod,
        Some(uk.gov.hmrc.vatsignupfrontend.controllers.principal.routes.CaptureLastReturnMonthPeriodController.show().url)
      )
    }
  }

  "Check your answers view without overseas postcode" should {
    lazy val pageWithoutPostCode: Html = page(Some(testBox5Figure), Some(testLastReturnMonthPeriod), Some(Yes.stringValue), None)

    lazy val doc: Document = Jsoup.parse(pageWithoutPostCode.body)


    val testPage = TestView(
      name = "Check your answers View",
      title = messages.title,
      heading = messages.heading,
      page = pageWithoutPostCode
    )

    testPage.shouldHaveH2(messages.subHeading)

    testPage.shouldHaveForm("Check your answers Form")(actionCall = testCall)

    "display the correct info for VatNumber" in {
      doc.sectionTest(
        VatNumberId,
        messages.yourVatNumber,
        testVatNumber,
        Some(uk.gov.hmrc.vatsignupfrontend.controllers.principal.routes.CaptureVatNumberController.show().url)
      )
    }

    "display the correct info for VatRegistrationDate" in {
      doc.sectionTest(
        VatRegistrationDateId,
        messages.vatRegistrationDate,
        testRegistrationDate.toCheckYourAnswersDateFormat,
        Some(uk.gov.hmrc.vatsignupfrontend.controllers.principal.routes.CaptureVatRegistrationDateController.show().url)
      )
    }

    "display the correct answer for Box5Value" in {
      doc.sectionTest(
        VatBox5FigureId,
        messages.box5Figure,
        expectedAnswer = s"£99,999,999,999.99",
        Some(uk.gov.hmrc.vatsignupfrontend.controllers.principal.routes.CaptureBox5FigureController.show().url)
      )
    }

    "display the correct answer for Previous VAT return" in {
      doc.sectionTest(
        PreviousVatReturnId,
        messages.previousVatReturn,
        Yes.stringValue,
        Some(uk.gov.hmrc.vatsignupfrontend.controllers.principal.routes.PreviousVatReturnController.show().url)
      )
    }

    "display the correct answer for LastReturnMonth" in {
      doc.sectionTest(
        VatLastReturnMonthId,
        messages.lastReturnMonth,
        testLastReturnMonthPeriod,
        Some(uk.gov.hmrc.vatsignupfrontend.controllers.principal.routes.CaptureLastReturnMonthPeriodController.show().url)
      )
    }
  }
}
