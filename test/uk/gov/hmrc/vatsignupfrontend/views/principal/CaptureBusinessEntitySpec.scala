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
import play.api.i18n.MessagesApi
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.vatsignupfrontend.assets.MessageLookup.{CaptureBusinessEntity => messages}
import uk.gov.hmrc.vatsignupfrontend.config.AppConfig
import uk.gov.hmrc.vatsignupfrontend.forms.BusinessEntityForm._
import uk.gov.hmrc.vatsignupfrontend.views.ViewSpec
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.capture_business_entity

class CaptureBusinessEntitySpec extends ViewSpec {

  val view = app.injector.instanceOf[capture_business_entity]
  lazy val appConfig: AppConfig = app.injector.instanceOf[AppConfig]
  lazy val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]
  val request: FakeRequest[AnyContentAsEmpty.type] = FakeRequest()

  lazy val page: HtmlFormat.Appendable = view(
    businessEntityForm,
    postAction = testCall
  )(
    request,
    messagesApi.preferred(request),
    appConfig
  )

  "The Principal Business Entity view" should {

    val testPage = TestView(
      name = "Principal Capture Business Entity View",
      title = messages.principalTitle,
      heading = messages.principalHeading,
      page = page
    )

    testPage.shouldHaveForm("Business Entity Form")(actionCall = testCall)

    soleTrader -> messages.radioSoleTrader
    limitedCompany -> messages.radioLimitedCompany
    generalPartnership -> messages.radioGeneralPartnership
    other -> messages.radioOther

    "have a set of radio inputs" which {
      lazy val doc = Jsoup.parse(page.body)

      "for the option 'Sole Trader'" should {

        "have the text 'Sole trader'" in {
          doc.select("label[for=sole-trader]").text() shouldEqual messages.radioSoleTrader
        }

        "have an input under the label that" should {

          lazy val optionLabel = doc.select("#sole-trader")

          "have the id 'sole-trader'" in {
            optionLabel.attr("id") shouldEqual "sole-trader"
          }

          "be of type radio" in {
            optionLabel.attr("type") shouldEqual "radio"
          }
        }
      }

      "for the option 'Limited company'" should {

        "have the text 'Limited company'" in {
          doc.select("label[for=limited-company]").text() shouldEqual messages.radioLimitedCompany
        }

        "have an input under the label that" should {

          lazy val optionLabel = doc.select("#limited-company")

          "have the id 'limited-company'" in {
            optionLabel.attr("id") shouldEqual "limited-company"
          }

          "be of type radio" in {
            optionLabel.attr("type") shouldEqual "radio"
          }
        }
      }

      "for the option 'General partnership'" should {

        "have the text 'General partnership'" in {
          doc.select("label[for=general-partnership]").text() shouldEqual messages.radioGeneralPartnership
        }

        "have an input under the label that" should {

          lazy val optionLabel = doc.select("#general-partnership")

          "have the id 'limited-company'" in {
            optionLabel.attr("id") shouldEqual "general-partnership"
          }

          "be of type radio" in {
            optionLabel.attr("type") shouldEqual "radio"
          }
        }
      }

      "for the option 'Limited partnership'" should {

        "have the text 'Limited partnership'" in {
          doc.select("label[for=limited-partnership]").text() shouldEqual messages.radioLimitedPartnership
        }

        "have an input under the label that" should {

          lazy val optionLabel = doc.select("#limited-partnership")

          "have the id 'limited-company'" in {
            optionLabel.attr("id") shouldEqual "limited-partnership"
          }

          "be of type radio" in {
            optionLabel.attr("type") shouldEqual "radio"
          }
        }
      }

      "for the option 'Other'" should {

        "have the text 'Other'" in {
          doc.select("label[for=other]").text() shouldEqual messages.radioOrOther
        }

        "have an input under the label that" should {

          lazy val optionLabel = doc.select("#other")

          "have the id 'other'" in {
            optionLabel.attr("id") shouldEqual "other"
          }

          "be of type radio" in {
            optionLabel.attr("type") shouldEqual "radio"
          }
        }
      }
    }

    testPage.shouldHaveContinueButton()
  }
}
