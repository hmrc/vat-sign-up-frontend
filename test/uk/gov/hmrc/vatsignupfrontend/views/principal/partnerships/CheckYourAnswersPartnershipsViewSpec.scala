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


import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import play.api.i18n.MessagesApi
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.twirl.api.Html
import uk.gov.hmrc.vatsignupfrontend.assets.MessageLookup.{CaptureBusinessEntity, PartnershipsCYA => messages}
import uk.gov.hmrc.vatsignupfrontend.config.AppConfig
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.models.{GeneralPartnership, LimitedPartnership}
import uk.gov.hmrc.vatsignupfrontend.utils.SummarySectionTesting
import uk.gov.hmrc.vatsignupfrontend.views.ViewSpec
import uk.gov.hmrc.vatsignupfrontend.views.helpers.CheckYourAnswersPartnershipsIdConstants._
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.partnerships.check_your_answers_partnerships

class CheckYourAnswersPartnershipsViewSpec extends ViewSpec with SummarySectionTesting {

  val view = app.injector.instanceOf[check_your_answers_partnerships]
  lazy val appConfig: AppConfig = app.injector.instanceOf[AppConfig]
  lazy val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]
  val request: FakeRequest[AnyContentAsEmpty.type] = FakeRequest()


  "Limited Partnership Check Your Answers View" should {

    lazy val limitedPartnershipPage: Html = view(
      entityType = LimitedPartnership,
      partnershipUtr = Some(testCompanyUtr),
      companyNumber = Some(testCompanyNumber),
      postCode = Some(testBusinessPostcode),
      postAction = testCall
    )(
      request,
      messagesApi.preferred(request),
      appConfig
    )

    val testPage = TestView(
      name = "Limited Partnership Check Your Answers View",
      title = messages.title,
      heading = messages.heading,
      page = limitedPartnershipPage
    )

    lazy val doc: Document = Jsoup.parse(limitedPartnershipPage.body)

    testPage.shouldHaveForm("Check Your Answers Form")(actionCall = testCall)

    "display the correct info for business entity" in {
      doc.sectionTest(
        BusinessEntityId,
        messages.businessEntity,
        CaptureBusinessEntity.radioLimitedPartnership,
        Some(uk.gov.hmrc.vatsignupfrontend.controllers.principal.routes.CaptureBusinessEntityController.show().url)
      )
    }

    "display the correct info for company number" in {
      doc.sectionTest(
        CompanyNumberId,
        messages.companyNumber,
        testCompanyNumber,
        Some(uk.gov.hmrc.vatsignupfrontend.controllers.principal.partnerships.routes.CapturePartnershipCompanyNumberController.show().url)
      )
    }

    "display the correct info for partnership utr" in {
      doc.sectionTest(
        CompanyUtrId,
        messages.companyUtr,
        testCompanyUtr,
        expectedEditLink = Some(uk.gov.hmrc.vatsignupfrontend.controllers.principal.partnerships.routes.ResolvePartnershipUtrController.resolve().url)
      )
    }

    "display the correct info for post code" in {
      doc.sectionTest(
        PartnershipPostCodeId,
        messages.postCode,
        testBusinessPostcode.checkYourAnswersFormat,
        Some(uk.gov.hmrc.vatsignupfrontend.controllers.principal.partnerships.routes.PrincipalPlacePostCodeController.show().url)
      )
    }

    "check your answers for limited partnership" should {
      "display the correct information" when {
        "a limited partnership user has a SaUTR displaying there SaUTR" in {
          doc.sectionTest(
            CompanyUtrId,
            messages.companyUtr,
            testCompanyUtr,
            Some(uk.gov.hmrc.vatsignupfrontend.controllers.principal.partnerships.routes.ResolvePartnershipUtrController.resolve().url)
          )
        }
      }
    }

  }

  "General Partnership Check Your Answers View" when {
    "the user has an Sautr" should {

      lazy val generalPartnershipPage: Html = view(
        entityType = GeneralPartnership,
        partnershipUtr = Some(testCompanyUtr),
        companyNumber = None,
        postCode = Some(testBusinessPostcode),
        postAction = testCall
      )(
        request,
        messagesApi.preferred(request),
        appConfig
      )

      val testPage = TestView(
        name = "General Partnership without Sautr Check Your Answers",
        title = messages.title,
        heading = messages.heading,
        page = generalPartnershipPage
      )

      lazy val doc: Document = Jsoup.parse(generalPartnershipPage.body)

      testPage.shouldHaveForm("Check Your Answers Form")(actionCall = testCall)

      "display the correct info for business entity" in {
        doc.sectionTest(
          BusinessEntityId,
          messages.businessEntity,
          CaptureBusinessEntity.radioGeneralPartnership,
          Some(uk.gov.hmrc.vatsignupfrontend.controllers.principal.routes.CaptureBusinessEntityController.show().url)
        )
      }

      "display the correct info for partnership utr" in {
        doc.sectionTest(
          CompanyUtrId,
          messages.companyUtr,
          testCompanyUtr,
          Some(uk.gov.hmrc.vatsignupfrontend.controllers.principal.partnerships.routes.ResolvePartnershipUtrController.resolve().url)
        )
      }

      "display the correct info for post code" in {
        doc.sectionTest(
          PartnershipPostCodeId,
          expectedQuestion = messages.postCode,
          expectedAnswer = testBusinessPostcode.checkYourAnswersFormat,
          expectedEditLink = Some(uk.gov.hmrc.vatsignupfrontend.controllers.principal.partnerships.routes.PrincipalPlacePostCodeController.show().url)
        )
      }


      "the user does not have an Sautr" should {
        val testPage = TestView(
          name = "General Partnership with Sautr Check Your Answers View",
          title = messages.title,
          heading = messages.heading,
          page = generalPartnershipPage
        )

        testPage.shouldHaveForm("Check Your Answers Form")(actionCall = testCall)

        "display the correct info for business entity" in {
          doc.sectionTest(
            BusinessEntityId,
            messages.businessEntity,
            CaptureBusinessEntity.radioGeneralPartnership,
            expectedEditLink = Some(uk.gov.hmrc.vatsignupfrontend.controllers.principal.routes.CaptureBusinessEntityController.show().url)
          )
        }
      }
    }
  }
}
