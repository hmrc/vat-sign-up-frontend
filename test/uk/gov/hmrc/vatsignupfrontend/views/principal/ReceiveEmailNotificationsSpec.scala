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
import uk.gov.hmrc.vatsignupfrontend.assets.MessageLookup
import uk.gov.hmrc.vatsignupfrontend.assets.MessageLookup.{ReceiveEmailNotifications => messages}
import uk.gov.hmrc.vatsignupfrontend.config.AppConfig
import uk.gov.hmrc.vatsignupfrontend.forms.ContactPreferencesForm
import uk.gov.hmrc.vatsignupfrontend.forms.ContactPreferencesForm._
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.views.ViewSpec
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.receive_email_notifications

class ReceiveEmailNotificationsSpec extends ViewSpec {

  val error = "error.principal.receive_email_notifications"
  val view = app.injector.instanceOf[receive_email_notifications]
  lazy val appConfig: AppConfig = app.injector.instanceOf[AppConfig]
  lazy val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]
  val request: FakeRequest[AnyContentAsEmpty.type] = FakeRequest()

  "The Receive Email Notifications view" when {

    "the form has no errors" should {

      lazy val page = view(
        testEmail,
        contactPreferencesForm(isAgent = false),
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
        haveSignOutInBanner = true
      )

      "have a set of radio inputs" which {
        lazy val doc = Jsoup.parse(page.body)

        "for the option 'Digital'" should {

          "have the text 'Send email to'" in {
            doc.select(s"label[for=${messages.radioDigital}]").text() shouldEqual messages.radioButtonEmail(testEmail)
          }

          "have an input under the label that" should {

            lazy val optionLabel = doc.select("#digital")

            "have the id 'yes'" in {
              optionLabel.attr("id") shouldEqual "digital"
            }

            "be of type radio" in {
              optionLabel.attr("type") shouldEqual "radio"
            }
          }
        }

        "for the option 'No'" should {

          "have the text 'no'" in {
            doc.select(s"label[for=${messages.radioPaper}]").text() shouldEqual messages.paper
          }

          "have an input under the label that" should {

            lazy val optionLabel = doc.select("#paper")

            "have the id 'no" in {
              optionLabel.attr("id") shouldEqual "paper"
            }

            "be of type radio" in {
              optionLabel.attr("type") shouldEqual "radio"
            }
          }
        }
      }

      testPage.shouldHaveForm("Receive Ready Form")(actionCall = testCall)

      testPage.shouldHaveParaSeq(
        messages.line1,
        messages.line2
      )

      testPage.shouldHaveContinueButton()
    }

    "the form has errors due to no radio option" should {

      lazy val page = view(
        testEmail,
        contactPreferencesForm(isAgent = false).bind(Map(ContactPreferencesForm.contactPreference -> "")),
        postAction = testCall)(
        request,
        messagesApi.preferred(request),
        appConfig
      )

      val testPage = TestView(
        name = "Receive Email Notifications Error",
        title = messages.title,
        heading = messages.heading,
        page = page,
        hasErrors = true
      )

      testPage.shouldHaveErrorSummary(MessageLookup.ReceiveEmailNotifications.error)
      testPage.shouldHaveFieldError(ContactPreferencesForm.contactPreference, MessageLookup.ReceiveEmailNotifications.error)
    }

  }
}
