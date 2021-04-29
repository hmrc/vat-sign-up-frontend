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
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.capture_business_entity_other

class CaptureBusinessEntityOtherSpec extends ViewSpec {

  val view = app.injector.instanceOf[capture_business_entity_other]
  lazy val appConfig: AppConfig = app.injector.instanceOf[AppConfig]
  lazy val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]
  val request: FakeRequest[AnyContentAsEmpty.type] = FakeRequest()

  def page: HtmlFormat.Appendable = view(
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

    "have a set of radio inputs" which {
      lazy val doc = Jsoup.parse(page.body)

      "for the option 'VAT group'" should {
        "have the text 'VAT group'" in {
          doc.select("label[for=vat-group]").text() shouldEqual messages.radioVatGroup
        }
        "have an input under the label that" should {
          lazy val optionLabel = doc.select("#vat-group")

          "have the id 'vat-group'" in {
            optionLabel.attr("id") shouldEqual "vat-group"
          }
          "be of type radio" in {
            optionLabel.attr("type") shouldEqual "radio"
          }
        }
      }
      "for the option 'Unincorporated Association'" should {
        "have the text 'Unincorporated Association'" in {
          doc.select("label[for=unincorporated-association]").text() shouldEqual messages.radioUnincorporatedAssociation
        }

        "have an input under the label that" should {
          lazy val optionLabel = doc.select("#unincorporated-association")

          "have the id 'unincorporated-association'" in {
            optionLabel.attr("id") shouldEqual "unincorporated-association"
          }
          "be of type radio" in {
            optionLabel.attr("type") shouldEqual "radio"
          }
        }
      }

      "for the option 'Trust'" should {
        "have the text 'Trust'" in {
          doc.select("label[for=trust]").text() shouldEqual messages.radioTrust
        }
        "have an input under the label that" should {
          lazy val optionLabel = doc.select("#trust")

          "have the id 'trust'" in {
            optionLabel.attr("id") shouldEqual "trust"
          }
          "be of type radio" in {
            optionLabel.attr("type") shouldEqual "radio"
          }
        }
      }

      "for the option 'Registered Society'" should {
        "have the text 'Registered society (including community benefit societies and co-operative societies)'" in {
          doc.select("label[for=registered-society]").text() shouldEqual messages.radioRegisteredSociety
        }

        "have an input under the label that" should {
          lazy val optionLabel = doc.select("#registered-society")

          "have the id 'registered-society'" in {
            optionLabel.attr("id") shouldEqual "registered-society"
          }
          "be of type radio" in {
            optionLabel.attr("type") shouldEqual "radio"
          }
        }
      }

      "for the option 'Charity'" should {
        "have the text 'CIO (charity)'" in {
          doc.select("label[for=charity]").text() shouldEqual messages.radioCharity
        }
        "have an input under the label that" should {

          lazy val optionLabel = doc.select("#charity")

          "have the id 'charity'" in {
            optionLabel.attr("id") shouldEqual "charity"
          }
          "be of type radio" in {
            optionLabel.attr("type") shouldEqual "radio"
          }
        }
      }

      "for the option 'Government Organisation'" should {
        "have the text 'Government Organisation'" in {
          doc.select("label[for=government-organisation]").text() shouldEqual messages.radioGovernmentOrganisation
        }

        "have an input under the label that" should {
          lazy val optionLabel = doc.select("#government-organisation")

          "have the id 'government-organisation'" in {
            optionLabel.attr("id") shouldEqual "government-organisation"
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
