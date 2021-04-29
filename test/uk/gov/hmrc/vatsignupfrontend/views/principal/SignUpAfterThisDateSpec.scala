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

import play.api.i18n.MessagesApi
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.vatsignupfrontend.assets.MessageLookup.{SignUpAfterThisDate => messages}
import uk.gov.hmrc.vatsignupfrontend.config.AppConfig
import uk.gov.hmrc.vatsignupfrontend.models.DateModel
import uk.gov.hmrc.vatsignupfrontend.views.ViewSpec
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.sign_up_after_this_date

class SignUpAfterThisDateSpec extends ViewSpec {

  val view = app.injector.instanceOf[sign_up_after_this_date]

  lazy val appConfig: AppConfig = app.injector.instanceOf[AppConfig]
  lazy val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]
  val request: FakeRequest[AnyContentAsEmpty.type] = FakeRequest()

  val testDate: LocalDate = LocalDate.now()
  val expectedFormattedDate: String = DateModel.dateConvert(testDate).toOutputDateFormat

  lazy val page: HtmlFormat.Appendable = view(
    date = testDate)(
    request,
    messagesApi.preferred(request),
    appConfig
  )

  "The Sign Up After This Date view" should {

    val testPage = TestView(
      name = "Sign Up After This Date view",
      title = messages.title,
      heading = messages.heading,
      page = page,
      haveSignOutInBanner = false
    )

    testPage.shouldHaveParaSeq(
      messages.line1,
      messages.line2,
      messages.line3,
      messages.line4(expectedFormattedDate),
      messages.line5
    )

    testPage.shouldHaveBulletSeq(
      messages.bullet1,
      messages.bullet2
    )

    testPage.shouldHaveSignOutButton(isAgent = false)

  }

}
