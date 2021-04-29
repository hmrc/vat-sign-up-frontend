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
import play.api.i18n.MessagesApi
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import uk.gov.hmrc.vatsignupfrontend.assets.MessageLookup
import uk.gov.hmrc.vatsignupfrontend.assets.MessageLookup.{AgentReceiveEmailNotifications => messages}
import uk.gov.hmrc.vatsignupfrontend.config.AppConfig
import uk.gov.hmrc.vatsignupfrontend.forms.ContactPreferencesForm
import uk.gov.hmrc.vatsignupfrontend.forms.ContactPreferencesForm._
import uk.gov.hmrc.vatsignupfrontend.views.ViewSpec
import uk.gov.hmrc.vatsignupfrontend.views.html.agent.receive_email_notifications


class ReceiveEmailNotificationsSpec extends ViewSpec {

  val view = app.injector.instanceOf[receive_email_notifications]
  lazy val appConfig: AppConfig = app.injector.instanceOf[AppConfig]
  lazy val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]
  val request: FakeRequest[AnyContentAsEmpty.type] = FakeRequest()

  "The Receive Email Notifications view" when {

    "There are no form errors" should {

      lazy val page = view(
        contactPreferencesForm(isAgent = true),
        postAction = testCall)(
        request,
        messagesApi.preferred(request),
        appConfig
      )

      val testPage = TestView(
        name = "Receive Email Notifcations",
        title = messages.title,
        heading = messages.heading,
        page = page
      )

      testPage.shouldHaveForm("Contact Preference Form")(actionCall = testCall)

      "have a set of radio inputs" which {
        lazy val doc = Jsoup.parse(page.body)

        "for the option 'Digital'" should {

          s"have the text '${messages.digital}'" in {
            doc.select(s"label[for=${ContactPreferencesForm.digital}]").text() shouldEqual messages.digital
          }

          "have an input under the label that" should {

            lazy val optionLabel = doc.select("#digital")

            "have the id 'digital'" in {
              optionLabel.attr("id") shouldEqual "digital"
            }

            "be of type radio" in {
              optionLabel.attr("type") shouldEqual "radio"
            }
          }
        }

        "for the option 'Paper'" should {

          s"have the text '${messages.paper}'" in {
            doc.select(s"label[for=${ContactPreferencesForm.paper}]").text() shouldEqual messages.paper
          }

          "have an input under the label that" should {

            lazy val optionLabel = doc.select("#paper")

            "have the id 'paper" in {
              optionLabel.attr("id") shouldEqual "paper"
            }

            "be of type radio" in {
              optionLabel.attr("type") shouldEqual "radio"
            }
          }
        }
      }

      testPage.shouldHaveParaSeq(
        messages.line1,
        messages.line2
      )

      testPage.shouldHaveContinueButton()
    }
  }

  "There are form errors as a preference hasn't been selected" should {

    lazy val page = view(
      contactPreferencesForm(isAgent = true).bind(Map(ContactPreferencesForm.contactPreference -> "")),
      postAction = testCall)(
      request,
      messagesApi.preferred(request),
      appConfig
    )

    val testPage = TestView(
      name = "Receive Email Notifcations",
      title = messages.title,
      heading = messages.heading,
      page = page,
      hasErrors = true
    )

    testPage.shouldHaveErrorSummary(MessageLookup.AgentReceiveEmailNotifications.error)
    testPage.shouldHaveFieldError(ContactPreferencesForm.contactPreference, MessageLookup.AgentReceiveEmailNotifications.error)

  }
}
