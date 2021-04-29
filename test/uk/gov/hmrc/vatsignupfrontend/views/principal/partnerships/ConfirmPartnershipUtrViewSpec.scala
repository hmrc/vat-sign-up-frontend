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

package uk.gov.hmrc.vatsignupfrontend.views.principal.partnerships

import play.api.i18n.MessagesApi
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.vatsignupfrontend.assets.MessageLookup.{Base => baseMessages, ConfirmPartnershipUtr => messages}
import uk.gov.hmrc.vatsignupfrontend.config.AppConfig
import uk.gov.hmrc.vatsignupfrontend.forms.ConfirmGeneralPartnershipForm._
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.views.ViewSpec
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.partnerships.confirm_partnership_utr

class ConfirmPartnershipUtrViewSpec extends ViewSpec {

  val view = app.injector.instanceOf[confirm_partnership_utr]
  lazy val appConfig: AppConfig = app.injector.instanceOf[AppConfig]
  lazy val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]
  val request: FakeRequest[AnyContentAsEmpty.type] = FakeRequest()

  def page(name: Option[String]): HtmlFormat.Appendable =
    view(
      testSaUtr,
      name,
      confirmPartnershipForm,
      postAction = testCall)(
      request,
      messagesApi.preferred(request),
      appConfig
    )

  "The Confirm General Partnership utr view" should {

    val testPage = TestView(
      name = "Confirm Partnership utr View",
      title = messages.title,
      heading = messages.headingGeneralPartnership,
      page = page(name = None)
    )
    testPage.shouldHaveForm("Yes No Form")(actionCall = testCall)

    testPage.shouldHavePara(testSaUtr)

    testPage.shouldHavePara(messages.line1)

    testPage.shouldHavePara(messages.line2)

    "have a set of radio inputs" which {
      lazy val doc = testPage.document

      "for the option 'Yes'" should {

        "have the text 'yes'" in {
          baseMessages.yes shouldEqual "Yes"
        }

        "have an input under the label that" should {

          lazy val optionLabel = doc.select("#yes")

          "have the id 'yes'" in {
            optionLabel.attr("id") shouldEqual "yes"
          }

          "be of type radio" in {
            optionLabel.attr("type") shouldEqual "radio"
          }
        }
      }

      "for the option 'No'" should {

        "have the text 'no'" in {
          baseMessages.no shouldEqual "No"
        }

        "have an input under the label that" should {

          lazy val optionLabel = doc.select("#no")

          "have the id 'no" in {
            optionLabel.attr("id") shouldEqual "no"
          }

          "be of type radio" in {
            optionLabel.attr("type") shouldEqual "radio"
          }
        }
      }
    }

    testPage.shouldHaveContinueButton()
  }

  "The Confirm Limited Partnership utr view" should {

    val testName = "test name"

    val testPage = TestView(
      name = "Confirm Partnership utr View",
      title = messages.title,
      heading = messages.headingLimitedPartnership(testName),
      page = page(name = Some(testName))
    )
  }

}
