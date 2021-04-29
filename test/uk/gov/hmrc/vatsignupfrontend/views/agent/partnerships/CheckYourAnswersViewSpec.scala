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

package uk.gov.hmrc.vatsignupfrontend.views.agent.partnerships

import _root_.uk.gov.hmrc.vatsignupfrontend.views.helpers.CheckYourAnswersPartnershipsIdConstants.CompanyNumberId
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import play.api.i18n.MessagesApi
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.twirl.api.Html
import uk.gov.hmrc.vatsignupfrontend.assets.MessageLookup.{AgentCheckYourAnswers => messages}
import uk.gov.hmrc.vatsignupfrontend.config.AppConfig
import uk.gov.hmrc.vatsignupfrontend.controllers.agent.partnerships.{routes => partnershipRoutes}
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.models.{GeneralPartnership, LimitedPartnership}
import uk.gov.hmrc.vatsignupfrontend.utils.SummarySectionTesting
import uk.gov.hmrc.vatsignupfrontend.views.ViewSpec
import uk.gov.hmrc.vatsignupfrontend.views.helpers.CheckYourAnswersIdConstants._
import uk.gov.hmrc.vatsignupfrontend.views.html.agent.partnerships.check_your_answers


class CheckYourAnswersViewSpec extends ViewSpec with SummarySectionTesting {

  val view = app.injector.instanceOf[check_your_answers]
  lazy val appConfig: AppConfig = app.injector.instanceOf[AppConfig]
  lazy val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]
  val request: FakeRequest[AnyContentAsEmpty.type] = FakeRequest()


  "the Check Your Answers View" when {
    val utrAnswer = "utr-answer"
    val businessEntityAnswer = "business-entity-answer"
    val pobAnswer = "business-post-code-answer"
    lazy val expectedUrlUtr = partnershipRoutes.CapturePartnershipUtrController.show().url
    lazy val expectedUrlPostCode = partnershipRoutes.PartnershipPostCodeController.show().url
    lazy val expectedUrlCompanyNumber = partnershipRoutes.AgentCapturePartnershipCompanyNumberController.show().url

    val testPageGeneralPartnership = TestView(
      name = "Check your answers View",
      title = messages.title,
      heading = messages.heading,
      page = view(
        utr = Some(testSaUtr),
        entityType = GeneralPartnership,
        Some(testBusinessPostcode),
        companyNumber = None,
        postAction = testCall
      )(
        request,
        messagesApi.preferred(request),
        appConfig
      )
    )

    "the saUtr and the post code are given for a general partnership" should {
      "render the page correctly" in {
        lazy val page: Html = view(
          utr = Some(testSaUtr),
          entityType = GeneralPartnership,
          Some(testBusinessPostcode),
          companyNumber = None,
          postAction = testCall
        )(
          request,
          messagesApi.preferred(request),
          appConfig
        )

        lazy val doc: Document = Jsoup.parse(page.body)

        doc.sectionTest(UtrId, messages.yourUtr, testSaUtr, Some(expectedUrlUtr))
        doc.sectionTest(
          BusinessPostCodeId,
          messages.yourBusinessPostCode,
          testBusinessPostcode.checkYourAnswersFormat,
          Some(expectedUrlPostCode)
        )

        testPageGeneralPartnership.document.getElementById(utrAnswer).text shouldBe testSaUtr
        testPageGeneralPartnership.document.getElementById(businessEntityAnswer).text shouldBe messages.generalPartnership
        testPageGeneralPartnership.document.getElementById(pobAnswer).text shouldBe testBusinessPostcode.checkYourAnswersFormat
      }
    }


      "the General Partnership has an optional SA UTR" should {
        "render the page correctly" in {
          lazy val page: Html = view(
            utr = None,
            entityType = GeneralPartnership,
            postCode = None,
            companyNumber = None,
            postAction = testCall
          )(
            request,
            messagesApi.preferred(request),
            appConfig
          )

          lazy val doc = Jsoup.parse(page.body)

          doc.sectionTest(
            sectionId = UtrId,
            expectedQuestion = messages.yourUtr,
            expectedAnswer = messages.noSAUTR,
            expectedEditLink = Some(expectedUrlUtr)
          )
        }


        "the saUtr, company number and the post code are given for a limited partnership" should {
          "render the page correctly" in {
            lazy val page: Html = view(
              utr = Some(testSaUtr),
              entityType = LimitedPartnership,
              Some(testBusinessPostcode),
              companyNumber = Some(testCompanyNumber),
              postAction = testCall
            )(
              request,
              messagesApi.preferred(request),
              appConfig
            )

            lazy val doc = Jsoup.parse(page.body)

            doc.sectionTest(UtrId, messages.yourUtr, testSaUtr, Some(expectedUrlUtr))
            doc.sectionTest(BusinessPostCodeId, messages.yourBusinessPostCode, testBusinessPostcode.checkYourAnswersFormat, Some(expectedUrlPostCode))
            doc.sectionTest(CompanyNumberId, messages.yourCompanyNumber, testCompanyNumber, Some(expectedUrlCompanyNumber))

            doc.getElementById(utrAnswer).text shouldBe testSaUtr
            doc.getElementById(businessEntityAnswer).text shouldBe messages.limitedPartnership
            doc.getElementById(pobAnswer).text shouldBe testBusinessPostcode.checkYourAnswersFormat
          }
        }
      }
    }
}
