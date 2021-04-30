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
import uk.gov.hmrc.vatsignupfrontend.assets.MessageLookup.{CaptureBusinessEntity, PrincipalCheckYourAnswersCompanySummary => messages}
import uk.gov.hmrc.vatsignupfrontend.config.AppConfig
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.models.{BusinessEntity, SoleTrader}
import uk.gov.hmrc.vatsignupfrontend.utils.SummarySectionTesting
import uk.gov.hmrc.vatsignupfrontend.views.ViewSpec
import uk.gov.hmrc.vatsignupfrontend.views.helpers.CheckYourAnswersCompanyConstants._
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.check_your_answers_company

class CheckYourAnswersCompanyViewSpec extends ViewSpec with SummarySectionTesting {

  val testEntity: BusinessEntity = SoleTrader
  val view = app.injector.instanceOf[check_your_answers_company]
  lazy val appConfig: AppConfig = app.injector.instanceOf[AppConfig]
  lazy val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]
  val request: FakeRequest[AnyContentAsEmpty.type] = FakeRequest()

  def page(): Html = view(
    companyNumber = testCompanyNumber,
    companyUtr = testCompanyUtr,
    businessEntity = testEntity,
    postAction = testCall
  )(
    request,
    messagesApi.preferred(request),
    appConfig
  )

  lazy val doc: Document = Jsoup.parse(page().body)

  lazy val pageDefault: Html = page()


  "Check your answers view" should {
    "display the correct info for CompanyNumber" in {
      doc.sectionTest(
        CompanyNumberId,
        messages.companyNumber,
        testCompanyNumber,
        Some(uk.gov.hmrc.vatsignupfrontend.controllers.principal.routes.CaptureCompanyNumberController.show().url)
      )
    }

    "display the correct info for CompanyUtr" in {
      doc.sectionTest(
        CompanyUtrId,
        messages.companyUtr,
        testCompanyUtr,
        Some(uk.gov.hmrc.vatsignupfrontend.controllers.principal.routes.CaptureCompanyUtrController.show().url)
      )
    }

    "display the correct info for BusinessEntity" in {
      doc.sectionTest(
        BusinessEntityId,
        messages.businessEntity,
        CaptureBusinessEntity.radioSoleTrader,
        Some(uk.gov.hmrc.vatsignupfrontend.controllers.principal.routes.CaptureBusinessEntityController.show().url)
      )
    }
  }
}
