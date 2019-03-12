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
import play.api.i18n.Messages.Implicits._
import play.api.i18n.MessagesApi
import play.api.test.FakeRequest
import play.api.{Configuration, Environment}
import uk.gov.hmrc.vatsignupfrontend.assets.MessageLookup.{ReceiveEmailNotifications => messages}
import uk.gov.hmrc.vatsignupfrontend.config.AppConfig
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.views.ViewSpec
import uk.gov.hmrc.vatsignupfrontend.forms.ContactPreferencesForm._


class ReceiveEmailNotificationsSpec extends ViewSpec {

  val env = Environment.simple()
  val configuration = Configuration.load(env)
  val error = "error.principal.receive_email_notifications"

  lazy val messagesApi = app.injector.instanceOf[MessagesApi]

  lazy val page = uk.gov.hmrc.vatsignupfrontend.views.html.principal.receive_email_notifications(
    testEmail,
    contactPreferencesForm(isAgent = false),
    postAction = testCall)(
    FakeRequest(),
    applicationMessages,
    new AppConfig(configuration, env)
  )

  "The Receive Email Notifications view" should {

    val testPage = TestView(
      name = "Receive Email Notifcations",
      title = messages.title,
      heading = messages.heading,
      page = page,
      haveSignOutInBanner = true
    )
    testPage.shouldHaveForm("Yes No Form")(actionCall = testCall)

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
          doc.select(s"label[for=${messages.radioPaper}]").text()  shouldEqual messages.paper
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
}
