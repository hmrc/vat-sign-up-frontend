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
import uk.gov.hmrc.vatsignupfrontend.assets.MessageLookup.{PrincipalCheckYourAnswersRegisteredSociety => messages}
import uk.gov.hmrc.vatsignupfrontend.config.AppConfig
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.models.{BusinessEntity, RegisteredSociety}
import uk.gov.hmrc.vatsignupfrontend.utils.SummarySectionTesting
import uk.gov.hmrc.vatsignupfrontend.views.ViewSpec
import uk.gov.hmrc.vatsignupfrontend.views.helpers.CheckYourAnswersIdConstants._
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.check_your_answers_registered_society

class CheckYourAnswersRegisteredSocietySpec extends ViewSpec with SummarySectionTesting {

  val testEntity: BusinessEntity = RegisteredSociety
  val view = app.injector.instanceOf[check_your_answers_registered_society]
  lazy val appConfig: AppConfig = app.injector.instanceOf[AppConfig]
  lazy val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]
  val request: FakeRequest[AnyContentAsEmpty.type] = FakeRequest()

  def page(): Html = view(
    companyNumber = testCompanyNumber,
    ctReference = testCompanyUtr,
    entityType = testEntity,
    postAction = testCall
  )(
    request,
    messagesApi.preferred(request),
    appConfig
  )

  lazy val pageDefault: Html = page()

  lazy val doc: Document = Jsoup.parse(page().body)

  "Check your answers view" should {

    val testPage = TestView(
      name = "Check your answers View",
      title = messages.title,
      heading = messages.heading,
      page = page
    )

    testPage.shouldHaveH2(messages.subHeading)

    testPage.shouldHaveForm("Check your answers Form")(actionCall = testCall)
  }


  "display the correct info for CompanyNumber" in {
    doc.sectionTest(
      CrnId,
      messages.companyNumber,
      testCompanyNumber,
      Some(uk.gov.hmrc.vatsignupfrontend.controllers.principal.routes.CaptureRegisteredSocietyCompanyNumberController.show().url)
    )
  }

  "display the correct info for CompanyUtr" in {
    doc.sectionTest(
      UtrId,
      messages.companyUtr,
      testCompanyUtr,
      Some(uk.gov.hmrc.vatsignupfrontend.controllers.principal.routes.CaptureRegisteredSocietyUtrController.show().url)
    )
  }

  "display the correct info for BusinessEntity" in {
    doc.sectionTest(
      BusinessEntityId,
      messages.businessEntity,
      messages.registeredSociety,
      Some(uk.gov.hmrc.vatsignupfrontend.controllers.principal.routes.CaptureBusinessEntityController.show().url)
    )
  }
}
