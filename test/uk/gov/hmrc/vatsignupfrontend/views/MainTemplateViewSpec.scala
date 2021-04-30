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

package uk.gov.hmrc.vatsignupfrontend.views

import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import play.api.i18n.MessagesApi
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.api.{Configuration, Environment}
import play.twirl.api.{Html, HtmlFormat}
import uk.gov.hmrc.http.SessionKeys
import uk.gov.hmrc.play.bootstrap.config.{RunMode, ServicesConfig}
import uk.gov.hmrc.vatsignupfrontend.config.AppConfig
import uk.gov.hmrc.vatsignupfrontend.views.html.main_template

class MainTemplateViewSpec extends ViewSpec {

  val testCountdown = "1234"
  val testTimeOut = "87913"

  val configuration: Configuration = Configuration.load(Environment.simple())
  lazy val runMode: RunMode = app.injector.instanceOf[RunMode]
  lazy val appConfig: AppConfig = new AppConfig(new ServicesConfig(configuration, runMode)) {
    override lazy val timeoutLength: String = testTimeOut
    override lazy val countdownLength: String = testCountdown
  }

  val view = app.injector.instanceOf[main_template]

  lazy val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]
  val request: FakeRequest[AnyContentAsEmpty.type] = FakeRequest()

  def page(enabledTimeout: Boolean = true,
           showSignOutLink: Boolean = true,
           hasAuthToken: Boolean = false,
           scriptElem: Option[Html] = None
          ): Html = view(
    navTitle = None,
    title = "testTitle",
    sidebarLinks = None,
    contentHeader = None,
    bodyClasses = None,
    mainClass = None,
    scriptElem = scriptElem,
    showSignOutLink = showSignOutLink,
    isAgent = false,
    enableTimeout = enabledTimeout)(HtmlFormat.empty)(
    if (hasAuthToken) FakeRequest().withSession(SessionKeys.authToken -> "")
    else request,
    messagesApi.preferred(request),
    appConfig
  )

  val testScript = "<script type=\"text/javascript\" src=\"test-javascript.js\"></script>"

  class Setup(enabledTimeOut: Boolean = true, showSignOutLink: Boolean = true, hasAuthToken: Boolean = false, scriptElem: Option[Html] = None) {
    val doc: Document = Jsoup.parse(page(enabledTimeOut, showSignOutLink, hasAuthToken, scriptElem).body)
  }

  "main template view" should {
    "display the title" in new Setup {
      doc.title shouldBe "testTitle"
    }

    "show timeout" in new Setup(true) {
      val timeoutScriptText: String = doc.getElementById("timeoutScript").html()
      timeoutScriptText.contains(testCountdown) shouldBe true
      timeoutScriptText.contains(testTimeOut) shouldBe true
    }

    "not show timeout" in new Setup(false) {
      Option(doc.getElementById("timeoutScript")) shouldBe None
    }

    "not show the sign out link when the user is logged out" in new Setup(showSignOutLink = false, hasAuthToken = false) {
      doc.getElementById("logOutNavHref") should be(null)
    }

    "show the sign out link when the user is logged out but the page has specified to always show it" in new Setup(showSignOutLink = true, hasAuthToken = false) {
      doc.getElementById("logOutNavHref") shouldNot be(null)
    }

    "show the sign out link when the user is logged in" in new Setup(showSignOutLink = false, hasAuthToken = true) {
      doc.getElementById("logOutNavHref") shouldNot be(null)
    }

    "have the mtd vat custom javascript file in the html" in new Setup() {
      doc.getElementById("mtd-vat-custom-js") shouldNot be(null)
    }

    "have the script element that is passed into the view" in new Setup(scriptElem = Some(Html(testScript))) {
      doc.getElementsByTag("script").toArray.map(_.toString) should contain(testScript)
    }
  }

}
