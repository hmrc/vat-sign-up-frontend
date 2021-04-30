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
import play.api.i18n.{Messages, MessagesApi}
import play.twirl.api.Html
import uk.gov.hmrc.vatsignupfrontend.assets.MessageLookup.{AgentCheckYourAnswersFinal => msg}
import uk.gov.hmrc.vatsignupfrontend.config.AppConfig
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch.FeatureSwitching
import uk.gov.hmrc.vatsignupfrontend.controllers.agent.partnerships.{routes => partnershipRoutes}
import uk.gov.hmrc.vatsignupfrontend.controllers.agent.routes._
import uk.gov.hmrc.vatsignupfrontend.controllers.agent.soletrader.{routes => soletraderRoutes}
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.models._
import uk.gov.hmrc.vatsignupfrontend.utils.SummarySectionTesting
import uk.gov.hmrc.vatsignupfrontend.views.ViewSpec
import uk.gov.hmrc.vatsignupfrontend.views.helpers.BusinessEntityHelper.getBusinessEntityName
import uk.gov.hmrc.vatsignupfrontend.views.helpers.CheckYourAnswersIdConstants._
import uk.gov.hmrc.vatsignupfrontend.views.helpers.ContactPreferenceHelper
import uk.gov.hmrc.vatsignupfrontend.views.html.agent.check_your_answers_final

class CheckYourAnswersFinalSpec extends ViewSpec with SummarySectionTesting with FeatureSwitching {

  lazy val appConfig: AppConfig = app.injector.instanceOf[AppConfig]
  lazy val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]

  def testSubmissionRequestSummary(vrn: String = testVatNumber,
                                   businessEntity: BusinessEntity,
                                   optNino: Option[String] = None,
                                   optCrn: Option[String] = None,
                                   optUtr: Option[String] = None,
                                   agentEmail: String = testAgentEmail,
                                   optClientEmail: Option[String] = Some(testEmail),
                                   contactPreference: ContactPreference = Digital
                                  ): SubscriptionRequestSummary =
    SubscriptionRequestSummary(
      vatNumber = vrn,
      businessEntity = businessEntity,
      optNino = optNino,
      optCompanyNumber = optCrn,
      optSautr = optUtr,
      optSignUpEmail = optClientEmail,
      transactionEmail = agentEmail,
      contactPreference = contactPreference
    )

  implicit val messages: Messages = messagesApi.preferred(viewTestRequest)
  val view = app.injector.instanceOf[check_your_answers_final]

  def testView(summary: SubscriptionRequestSummary,
               optBusinessEntity: Option[BusinessEntity],
               optCompanyName: Option[String] = None
              ): Html = view(
    subSummary = summary,
    optBusinessEntity = optBusinessEntity,
    optCompanyName = optCompanyName,
    postAction = testCall
  )(
    viewTestRequest,
    messages,
    appConfig
  )

  "The final check your answers page" when {
    "The business is a Sole trader" when {
      val summary = testSubmissionRequestSummary(businessEntity = SoleTrader, optNino = Some(testNino))
      lazy val page = testView(summary = summary, optBusinessEntity = Some(SoleTrader))
      lazy val doc = Jsoup.parse(page.body)

      val testPage = TestView(
        name = "Final check your answers - Sole trader",
        title = msg.title,
        heading = msg.heading,
        page = page
      )

      testPage.shouldHaveAcceptAndSendButton()

      "have rows for VRN, NINO, agent email, client email and contact preference" in {
        doc.sectionTest(VatNumberId, msg.vrn, testVatNumber, Some(CaptureVatNumberController.show().url))
        doc.sectionTest(BusinessEntityId, msg.businessEntity, getBusinessEntityName(SoleTrader), Some(CaptureBusinessEntityController.show().url))
        doc.sectionTest(NinoId, msg.nino, testNino, Some(soletraderRoutes.CaptureNinoController.show().url))
        doc.sectionTest(AgentEmailId, msg.agentEmail, testAgentEmail, Some(CaptureAgentEmailController.show().url))
        doc.sectionTest(ContactPreferenceId, msg.contactPreference, ContactPreferenceHelper.getContactPreferenceName(Digital), Some(ContactPreferenceController.show().url))
        doc.sectionTest(ClientEmailId, msg.clientEmail, testEmail, Some(CaptureClientEmailController.show().url))
      }
    }
    "The business is a Limited company or overseas trader with UK establishment" should {
      val summary = testSubmissionRequestSummary(businessEntity = LimitedCompany, optCrn = Some(testCompanyNumber))
      lazy val page = testView(summary = summary, optBusinessEntity = Some(LimitedCompany), optCompanyName = Some(testCompanyName))
      lazy val doc = Jsoup.parse(page.body)

      val testPage = TestView(
        name = "Final check your answers - Limited company",
        title = msg.title,
        heading = msg.heading,
        page = page
      )

      testPage.shouldHaveAcceptAndSendButton()

      "have rows for VRN, CRN, Company name, agent email, client email and contact preference" in {
        doc.sectionTest(VatNumberId, msg.vrn, testVatNumber, Some(CaptureVatNumberController.show().url))
        doc.sectionTest(BusinessEntityId, msg.businessEntity, getBusinessEntityName(LimitedCompany), Some(CaptureBusinessEntityController.show().url))
        doc.sectionTest(CrnId, msg.companyNumber, testCompanyNumber, Some(CaptureCompanyNumberController.show().url))
        doc.sectionTest(AgentEmailId, msg.agentEmail, testAgentEmail, Some(CaptureAgentEmailController.show().url))
        doc.sectionTest(ContactPreferenceId, msg.contactPreference, ContactPreferenceHelper.getContactPreferenceName(Digital), Some(ContactPreferenceController.show().url))
        doc.sectionTest(ClientEmailId, msg.clientEmail, testEmail, Some(CaptureClientEmailController.show().url))
      }
    }
    "The business is a General Partnership" should {
      val summary = testSubmissionRequestSummary(businessEntity = GeneralPartnership, optUtr = Some(testCompanyUtr))
      lazy val page = testView(summary = summary, optBusinessEntity = Some(GeneralPartnership))
      lazy val doc = Jsoup.parse(page.body)

      val testPage = TestView(
        name = "Final check your answers - General partnership",
        title = msg.title,
        heading = msg.heading,
        page = page
      )

      testPage.shouldHaveAcceptAndSendButton()

      "have rows for VRN, Partnership UTR, agent email, client email and contact preference" in {
        doc.sectionTest(VatNumberId, msg.vrn, testVatNumber, Some(CaptureVatNumberController.show().url))
        doc.sectionTest(BusinessEntityId, msg.businessEntity, getBusinessEntityName(GeneralPartnership), Some(CaptureBusinessEntityController.show().url))
        doc.sectionTest(PartnershipUtrId, msg.partnershipUtr, testCompanyUtr, Some(partnershipRoutes.CapturePartnershipUtrController.show().url))
        doc.sectionTest(AgentEmailId, msg.agentEmail, testAgentEmail, Some(CaptureAgentEmailController.show().url))
        doc.sectionTest(ContactPreferenceId, msg.contactPreference, ContactPreferenceHelper.getContactPreferenceName(Digital), Some(ContactPreferenceController.show().url))
        doc.sectionTest(ClientEmailId, msg.clientEmail, testEmail, Some(CaptureClientEmailController.show().url))
      }
    }
    "The business is a Limited Partnership" should {
      val summary = testSubmissionRequestSummary(businessEntity = LimitedPartnership,
        optUtr = Some(testCompanyUtr),
        optCrn = Some(testCompanyNumber)
      )

      lazy val page = testView(
        summary = summary,
        optBusinessEntity = Some(LimitedPartnership),
        optCompanyName = Some(testCompanyName)
      )

      lazy val doc = Jsoup.parse(page.body)

      val testPage = TestView(
        name = "Final check your answers - General partnership",
        title = msg.title,
        heading = msg.heading,
        page = page
      )

      testPage.shouldHaveAcceptAndSendButton()

      "have rows for VRN, CRN, Company name, Partnership UTR, agent email, client email and contact preference" in {
        doc.sectionTest(VatNumberId, msg.vrn, testVatNumber, Some(CaptureVatNumberController.show().url))
        doc.sectionTest(BusinessEntityId, msg.businessEntity, getBusinessEntityName(LimitedPartnership), Some(CaptureBusinessEntityController.show().url))
        doc.sectionTest(CrnId, msg.partnershipCompanyNumber, testCompanyNumber, Some(partnershipRoutes.AgentCapturePartnershipCompanyNumberController.show().url))
        doc.sectionTest(CompanyNameId, msg.partnershipCompanyName, testCompanyName, None)
        doc.sectionTest(PartnershipUtrId, msg.partnershipUtr, testCompanyUtr, Some(partnershipRoutes.CapturePartnershipUtrController.show().url))
        doc.sectionTest(AgentEmailId, msg.agentEmail, testAgentEmail, Some(CaptureAgentEmailController.show()
          .url))
        doc.sectionTest(ContactPreferenceId, msg.contactPreference, ContactPreferenceHelper.getContactPreferenceName(Digital), Some(ContactPreferenceController.show().url))
        doc.sectionTest(ClientEmailId, msg.clientEmail, testEmail, Some(CaptureClientEmailController.show()
          .url))
      }
    }
    "The business is a Scottish Partnership" should {
      val summary = testSubmissionRequestSummary(
        businessEntity = ScottishLimitedPartnership,
        optCrn = Some(testCompanyNumber),
        optUtr = Some(testCompanyUtr)
      )

      lazy val page = testView(
        summary = summary,
        optBusinessEntity = Some(ScottishLimitedPartnership),
        optCompanyName = Some(testCompanyName)
      )

      lazy val doc = Jsoup.parse(page.body)

      val testPage = TestView(
        name = "Final check your answers - Scottish partnership",
        title = msg.title,
        heading = msg.heading,
        page = page
      )

      testPage.shouldHaveAcceptAndSendButton()

      "have rows for VRN, CRN, Company name, Partnership UTR, agent email, client email and contact preference" in {
        doc.sectionTest(VatNumberId, msg.vrn, testVatNumber, Some(CaptureVatNumberController.show().url))
        doc.sectionTest(BusinessEntityId, msg.businessEntity, getBusinessEntityName(ScottishLimitedPartnership), Some(CaptureBusinessEntityController.show().url))
        doc.sectionTest(CrnId, msg.partnershipCompanyNumber, testCompanyNumber, Some(partnershipRoutes.AgentCapturePartnershipCompanyNumberController.show().url))
        doc.sectionTest(CompanyNameId, msg.partnershipCompanyName, testCompanyName, None)
        doc.sectionTest(PartnershipUtrId, msg.partnershipUtr, testCompanyUtr, Some(partnershipRoutes.CapturePartnershipUtrController.show().url))
        doc.sectionTest(AgentEmailId, msg.agentEmail, testAgentEmail, Some(CaptureAgentEmailController.show()
          .url))
        doc.sectionTest(ContactPreferenceId, msg.contactPreference, ContactPreferenceHelper.getContactPreferenceName(Digital), Some(ContactPreferenceController.show().url))
        doc.sectionTest(ClientEmailId, msg.clientEmail, testEmail, Some(CaptureClientEmailController.show()
          .url))
      }
    }
    "The business is a Limited Liability Partnership" should {
      val summary = testSubmissionRequestSummary(
        businessEntity = LimitedLiabilityPartnership,
        optCrn = Some(testCompanyNumber),
        optUtr = Some(testCompanyUtr)
      )

      lazy val page = testView(
        summary = summary,
        optBusinessEntity = Some(LimitedLiabilityPartnership),
        optCompanyName = Some(testCompanyName)
      )

      lazy val doc = Jsoup.parse(page.body)

      val testPage = TestView(
        name = "Final check your answers - Limited liability partnership",
        title = msg.title,
        heading = msg.heading,
        page = page
      )

      testPage.shouldHaveAcceptAndSendButton()

      "have rows for VRN, CRN, Company name, Partnership UTR, agent email, client email and contact preference" in {
        doc.sectionTest(VatNumberId, msg.vrn, testVatNumber, Some(CaptureVatNumberController.show().url))
        doc.sectionTest(BusinessEntityId, msg.businessEntity, getBusinessEntityName(LimitedLiabilityPartnership), Some(CaptureBusinessEntityController.show().url))
        doc.sectionTest(CrnId, msg.partnershipCompanyNumber, testCompanyNumber, Some(partnershipRoutes.AgentCapturePartnershipCompanyNumberController.show().url))
        doc.sectionTest(CompanyNameId, msg.partnershipCompanyName, testCompanyName, None)
        doc.sectionTest(PartnershipUtrId, msg.partnershipUtr, testCompanyUtr, Some(partnershipRoutes.CapturePartnershipUtrController.show().url))
        doc.sectionTest(AgentEmailId, msg.agentEmail, testAgentEmail, Some(CaptureAgentEmailController.show()
          .url))
        doc.sectionTest(ContactPreferenceId, msg.contactPreference, ContactPreferenceHelper.getContactPreferenceName(Digital), Some(ContactPreferenceController.show().url))
        doc.sectionTest(ClientEmailId, msg.clientEmail, testEmail, Some(CaptureClientEmailController.show()
          .url))
      }
    }
    "The business is a VAT group" should {
      val summary = testSubmissionRequestSummary(businessEntity = VatGroup)
      lazy val page = testView(summary = summary, optBusinessEntity = Some(VatGroup))
      lazy val doc = Jsoup.parse(page.body)

      val testPage = TestView(
        name = "Final check your answers - VAT group",
        title = msg.title,
        heading = msg.heading,
        page = page
      )

      testPage.shouldHaveAcceptAndSendButton()

      "have rows for VRN, agent email, client email and contact preference" in {
        doc.sectionTest(VatNumberId, msg.vrn, testVatNumber, Some(CaptureVatNumberController.show().url))
        doc.sectionTest(BusinessEntityId, msg.businessEntity, getBusinessEntityName(VatGroup), Some(CaptureBusinessEntityController.show().url))
        doc.sectionTest(AgentEmailId, msg.agentEmail, testAgentEmail, Some(CaptureAgentEmailController.show()
          .url))
        doc.sectionTest(ContactPreferenceId, msg.contactPreference, ContactPreferenceHelper.getContactPreferenceName(Digital), Some(ContactPreferenceController.show().url))
        doc.sectionTest(ClientEmailId, msg.clientEmail, testEmail, Some(CaptureClientEmailController.show()
          .url))
      }
    }
    "The business is a Charity" should {
      val summary = testSubmissionRequestSummary(businessEntity = Charity)
      lazy val page = testView(summary = summary, optBusinessEntity = Some(Charity))
      lazy val doc = Jsoup.parse(page.body)

      val testPage = TestView(
        name = "Final check your answers - Charity",
        title = msg.title,
        heading = msg.heading,
        page = page
      )

      testPage.shouldHaveAcceptAndSendButton()

      "have rows for VRN, agent email, client email and contact preference" in {
        doc.sectionTest(VatNumberId, msg.vrn, testVatNumber, Some(CaptureVatNumberController.show().url))
        doc.sectionTest(BusinessEntityId, msg.businessEntity, getBusinessEntityName(Charity), Some(CaptureBusinessEntityController.show().url))
        doc.sectionTest(AgentEmailId, msg.agentEmail, testAgentEmail, Some(CaptureAgentEmailController.show()
          .url))
        doc.sectionTest(ContactPreferenceId, msg.contactPreference, ContactPreferenceHelper.getContactPreferenceName(Digital), Some(ContactPreferenceController.show().url))
        doc.sectionTest(ClientEmailId, msg.clientEmail, testEmail, Some(CaptureClientEmailController.show()
          .url))
      }
    }
    "The business is a Division" should {
      val summary = testSubmissionRequestSummary(businessEntity = Division)
      lazy val page = testView(summary = summary, optBusinessEntity = None)
      lazy val doc = Jsoup.parse(page.body)

      val testPage = TestView(
        name = "Final check your answers - Division",
        title = msg.title,
        heading = msg.heading,
        page = page
      )

      testPage.shouldHaveAcceptAndSendButton()

      "have rows for VRN, agent email, client email and contact preference but NOT business entity" in {
        doc.sectionTest(VatNumberId, msg.vrn, testVatNumber, Some(CaptureVatNumberController.show().url))
        doc.sectionTest(AgentEmailId, msg.agentEmail, testAgentEmail, Some(CaptureAgentEmailController.show()
          .url))
        doc.sectionTest(ContactPreferenceId, msg.contactPreference, ContactPreferenceHelper.getContactPreferenceName(Digital), Some(ContactPreferenceController.show().url))
        doc.sectionTest(ClientEmailId, msg.clientEmail, testEmail, Some(CaptureClientEmailController.show()
          .url))
        doc.select("#business-entity-row").isEmpty shouldBe true
      }
    }
    "The business is an Unincorporated Association" should {
      val summary = testSubmissionRequestSummary(businessEntity = UnincorporatedAssociation)
      lazy val page = testView(summary = summary, optBusinessEntity = Some(UnincorporatedAssociation))
      lazy val doc = Jsoup.parse(page.body)

      val testPage = TestView(
        name = "Final check your answers - Unincorporated Association",
        title = msg.title,
        heading = msg.heading,
        page = page
      )

      testPage.shouldHaveAcceptAndSendButton()

      "have rows for VRN, agent email, client email and contact preference" in {
        doc.sectionTest(VatNumberId, msg.vrn, testVatNumber, Some(CaptureVatNumberController.show().url))
        doc.sectionTest(BusinessEntityId, msg.businessEntity, getBusinessEntityName(UnincorporatedAssociation), Some(CaptureBusinessEntityController.show().url))
        doc.sectionTest(AgentEmailId, msg.agentEmail, testAgentEmail, Some(CaptureAgentEmailController.show()
          .url))
        doc.sectionTest(ContactPreferenceId, msg.contactPreference, ContactPreferenceHelper.getContactPreferenceName(Digital), Some(ContactPreferenceController.show().url))
        doc.sectionTest(ClientEmailId, msg.clientEmail, testEmail, Some(CaptureClientEmailController.show()
          .url))
      }
    }
    "The business is an Trust" should {
      val summary = testSubmissionRequestSummary(businessEntity = Trust)
      lazy val page = testView(summary = summary, optBusinessEntity = Some(Trust))
      lazy val doc = Jsoup.parse(page.body)

      val testPage = TestView(
        name = "Final check your answers - Trust",
        title = msg.title,
        heading = msg.heading,
        page = page
      )

      testPage.shouldHaveAcceptAndSendButton()

      "have rows for VRN, agent email, client email and contact preference" in {
        doc.sectionTest(VatNumberId, msg.vrn, testVatNumber, Some(CaptureVatNumberController.show().url))
        doc.sectionTest(BusinessEntityId, msg.businessEntity, getBusinessEntityName(Trust), Some(CaptureBusinessEntityController.show().url))
        doc.sectionTest(AgentEmailId, msg.agentEmail, testAgentEmail, Some(CaptureAgentEmailController.show()
          .url))
        doc.sectionTest(ContactPreferenceId, msg.contactPreference, ContactPreferenceHelper.getContactPreferenceName(Digital), Some(ContactPreferenceController.show().url))
        doc.sectionTest(ClientEmailId, msg.clientEmail, testEmail, Some(CaptureClientEmailController.show()
          .url))
      }
    }
    "The business is a Registered Society" should {
      val summary = testSubmissionRequestSummary(businessEntity = RegisteredSociety, optCrn = Some(testCompanyNumber))
      lazy val page = testView(summary = summary, optBusinessEntity = Some(RegisteredSociety), optCompanyName = Some(testCompanyName))
      lazy val doc = Jsoup.parse(page.body)

      val testPage = TestView(
        name = "Final check your answers - Registered Society",
        title = msg.title,
        heading = msg.heading,
        page = page
      )

      testPage.shouldHaveAcceptAndSendButton()

      "have rows for VRN, CRN, Company name, agent email, client email and contact preference" in {
        doc.sectionTest(VatNumberId, msg.vrn, testVatNumber, Some(CaptureVatNumberController.show().url))
        doc.sectionTest(BusinessEntityId, msg.businessEntity, getBusinessEntityName(RegisteredSociety), Some(CaptureBusinessEntityController.show().url))
        doc.sectionTest(CrnId, msg.registeredSocietyCompanyNumber, testCompanyNumber, Some(CaptureRegisteredSocietyCompanyNumberController.show().url))
        doc.sectionTest(CompanyNameId, msg.registeredSocietyCompanyName, testCompanyName, None)
        doc.sectionTest(AgentEmailId, msg.agentEmail, testAgentEmail, Some(CaptureAgentEmailController.show().url))
        doc.sectionTest(ContactPreferenceId, msg.contactPreference, ContactPreferenceHelper.getContactPreferenceName(Digital), Some(ContactPreferenceController.show().url))
        doc.sectionTest(ClientEmailId, msg.clientEmail, testEmail, Some(CaptureClientEmailController.show().url))
      }
    }
    "The business is an Government Organisation" should {
      val summary = testSubmissionRequestSummary(businessEntity = GovernmentOrganisation)

      lazy val page = testView(summary = summary, optBusinessEntity = Some(GovernmentOrganisation))
      lazy val doc = Jsoup.parse(page.body)

      val testPage = TestView(
        name = "Final check your answers - Government Organisation",
        title = msg.title,
        heading = msg.heading,
        page = page
      )

      testPage.shouldHaveAcceptAndSendButton()

      "have rows for VRN, agent email, client email and contact preference" in {
        doc.sectionTest(VatNumberId, msg.vrn, testVatNumber, Some(CaptureVatNumberController.show().url))
        doc.sectionTest(BusinessEntityId, msg.businessEntity, getBusinessEntityName(GovernmentOrganisation), Some(CaptureBusinessEntityController.show().url))
        doc.sectionTest(AgentEmailId, msg.agentEmail, testAgentEmail, Some(CaptureAgentEmailController.show()
          .url))
        doc.sectionTest(ContactPreferenceId, msg.contactPreference, ContactPreferenceHelper.getContactPreferenceName(Digital), Some(ContactPreferenceController.show().url))
        doc.sectionTest(ClientEmailId, msg.clientEmail, testEmail, Some(CaptureClientEmailController.show()
          .url))
      }
    }
    "The business is overseas without a UK establishment" should {
      val summary = testSubmissionRequestSummary(businessEntity = Overseas)
      lazy val page = testView(summary = summary, optBusinessEntity = None)
      lazy val doc = Jsoup.parse(page.body)

      val testPage = TestView(
        name = "Final check your answers - Overseas without a UK establishment",
        title = msg.title,
        heading = msg.heading,
        page = page
      )

      testPage.shouldHaveAcceptAndSendButton()

      "NOT have business entity and have rows for VRN, agent email, client email and contact preference" in {
        doc.sectionTest(VatNumberId, msg.vrn, testVatNumber, Some(CaptureVatNumberController.show().url))
        doc.sectionTest(AgentEmailId, msg.agentEmail, testAgentEmail, Some(CaptureAgentEmailController.show().url))
        doc.sectionTest(ContactPreferenceId, msg.contactPreference, ContactPreferenceHelper.getContactPreferenceName(Digital), Some(ContactPreferenceController.show().url))
        doc.sectionTest(ClientEmailId, msg.clientEmail, testEmail, Some(CaptureClientEmailController.show().url))
      }
    }
  }

}
