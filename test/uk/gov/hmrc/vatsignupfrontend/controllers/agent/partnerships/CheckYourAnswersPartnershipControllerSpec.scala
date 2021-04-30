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

package uk.gov.hmrc.vatsignupfrontend.controllers.agent.partnerships

import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.http.InternalServerException
import uk.gov.hmrc.vatsignupfrontend.utils.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.SessionKeys._
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockVatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.controllers.agent.error.{routes => errorRoutes}
import uk.gov.hmrc.vatsignupfrontend.controllers.agent.{routes => agentRoutes}
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.httpparsers.StorePartnershipInformationHttpParser._
import uk.gov.hmrc.vatsignupfrontend.models.BusinessEntity.BusinessEntitySessionFormatter
import uk.gov.hmrc.vatsignupfrontend.models.PartnershipEntityType.CompanyTypeSessionFormatter
import uk.gov.hmrc.vatsignupfrontend.models._
import uk.gov.hmrc.vatsignupfrontend.services.mocks.MockStorePartnershipInformationService
import uk.gov.hmrc.vatsignupfrontend.utils.SessionUtils.jsonSessionFormatter
import uk.gov.hmrc.vatsignupfrontend.views.html.agent.partnerships.check_your_answers

import scala.concurrent.Future

class CheckYourAnswersPartnershipControllerSpec extends UnitSpec with GuiceOneAppPerSuite with MockVatControllerComponents
  with MockStorePartnershipInformationService {

  val view = app.injector.instanceOf[check_your_answers]

  object TestCheckYourAnswersPartnershipController extends CheckYourAnswersPartnershipController(
    mockStorePartnershipInformationService,
    view
  )

  private def sessionValues(vatNumber: Option[String] = Some(testVatNumber),
                            sautr: Option[String] = Some(testSaUtr),
                            postCode: Option[PostCode] = Some(testBusinessPostcode),
                            entityType: Option[BusinessEntity] = None,
                            companyNumber: Option[String],
                            partnershipEntityType: Option[PartnershipEntityType]): Iterable[(String, String)] =
    ((vatNumber map (vatNumberKey -> _))
      ++ (sautr map (partnershipSautrKey -> _))
      ++ (postCode map jsonSessionFormatter[PostCode].toString map (partnershipPostCodeKey -> _))
      ++ (entityType map BusinessEntitySessionFormatter.toString map (businessEntityKey -> _))
      ++ (partnershipEntityType map CompanyTypeSessionFormatter.toString map (partnershipTypeKey -> _))
      ++ (companyNumber map (companyNumberKey -> _)))


  def testGetRequest(vatNumber: Option[String] = Some(testVatNumber),
                     sautr: Option[String] = None,
                     postCode: Option[PostCode] = None,
                     entityType: Option[BusinessEntity] = None,
                     companyNumber: Option[String] = None,
                     partnershipEntityType: Option[PartnershipEntityType] = None
                    ): FakeRequest[AnyContentAsEmpty.type] =
    FakeRequest("GET", "/check-your-answers").withSession(
      sessionValues(vatNumber, sautr, postCode, entityType, companyNumber, partnershipEntityType).toSeq: _*
    )

  def testPostRequest(vatNumber: Option[String] = Some(testVatNumber),
                      sautr: Option[String] = None,
                      postCode: Option[PostCode] = None,
                      entityType: Option[BusinessEntity] = None,
                      companyNumber: Option[String] = None,
                      partnershipEntityType: Option[PartnershipEntityType] = None
                     ): FakeRequest[AnyContentAsEmpty.type] =
    FakeRequest("POST", "/check-your-answers").withSession(
      sessionValues(vatNumber, sautr, postCode, entityType, companyNumber, partnershipEntityType).toSeq: _*
    )

  "Calling the show action of the Check your answers controller" when {
    "all prerequisite data are in session for general partnership" should {
      "go to the Check your answers page" in {
        mockAuthRetrieveAgentEnrolment()

        val result = TestCheckYourAnswersPartnershipController.show(testGetRequest(
          entityType = Some(GeneralPartnership),
          sautr = Some(testSaUtr),
          postCode = Some(testBusinessPostcode)
        ))

        status(result) shouldBe Status.OK
        contentType(result) shouldBe Some("text/html")
        charset(result) shouldBe Some("utf-8")
      }
    }
    "all prerequisite data are in session" should {
      "go to the Check your answers page" in {
        mockAuthRetrieveAgentEnrolment()

        val result = TestCheckYourAnswersPartnershipController.show(testGetRequest(
          entityType = Some(LimitedPartnership),
          partnershipEntityType = Some(PartnershipEntityType.LimitedPartnership),
          companyNumber = Some(testCompanyNumber),
          sautr = Some(testSaUtr),
          postCode = Some(testBusinessPostcode)
        ))

        status(result) shouldBe Status.OK
        contentType(result) shouldBe Some("text/html")
        charset(result) shouldBe Some("utf-8")
      }
    }
    "vat number is missing" should {
      "go to capture vat number page" in {
        mockAuthRetrieveAgentEnrolment()

        val result = TestCheckYourAnswersPartnershipController.show(testGetRequest(
          entityType = Some(GeneralPartnership),
          sautr = Some(testSaUtr),
          postCode = Some(testBusinessPostcode),
          vatNumber = None
        ))

        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(agentRoutes.CaptureVatNumberController.show().url)
      }
    }
    "business entity is missing" should {
      "go to capture business entity page" in {
        mockAuthRetrieveAgentEnrolment()

        val result = TestCheckYourAnswersPartnershipController.show(testGetRequest(
          entityType = None,
          sautr = Some(testSaUtr),
          postCode = Some(testBusinessPostcode)
        ))

        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(agentRoutes.CaptureBusinessEntityController.show().url)
      }
    }
    "sa utr is missing for a limited partnership" should {
      "go to capture business entity page" in {
        mockAuthRetrieveAgentEnrolment()

        val result = TestCheckYourAnswersPartnershipController.show(testGetRequest(
          entityType = Some(LimitedPartnership),
          sautr = None,
          companyNumber = Some(testCompanyNumber),
          postCode = Some(testBusinessPostcode)
        ))

        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(agentRoutes.CaptureBusinessEntityController.show().url)
      }
    }
  }

  "calling the submit action of the Check Your Answers Partnership controller" when {
    "all prerequisite data are in" when {
      "store partnership info returned StorePartnershipInformationSuccess" should {
        "go to the capture agent email age" in {
          mockAuthRetrieveAgentEnrolment()

          mockStorePartnershipInformation(
            testVatNumber,
            Some(testSaUtr),
            Some(testBusinessPostcode)
          )(Future.successful(Right(StorePartnershipInformationSuccess)))

          val result = TestCheckYourAnswersPartnershipController.submit(testPostRequest(
            entityType = Some(GeneralPartnership),
            sautr = Some(testSaUtr),
            postCode = Some(testBusinessPostcode)
          ))

          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(agentRoutes.CaptureAgentEmailController.show().url)
        }
      }

      "store partnership info returned StorePartnershipInformationSuccess for a limited partnership" should {
        "go to the capture agent email page" in {
          mockAuthRetrieveAgentEnrolment()

          mockStorePartnershipInformation(
            testVatNumber,
            Some(testSaUtr),
            testCompanyNumber,
            PartnershipEntityType.LimitedPartnership,
            Some(testBusinessPostcode)
          )(Future.successful(Right(StorePartnershipInformationSuccess)))

          val result = TestCheckYourAnswersPartnershipController.submit(testPostRequest(
            entityType = Some(LimitedPartnership),
            partnershipEntityType = Some(PartnershipEntityType.LimitedPartnership),
            companyNumber = Some(testCompanyNumber),
            sautr = Some(testSaUtr),
            postCode = Some(testBusinessPostcode)
          ))

          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(agentRoutes.CaptureAgentEmailController.show().url)
        }
      }
      "store partnership info returned KnownFactsMismatchFailure" should {
        "go to Could not confirm partnership Page" in {
          mockAuthRetrieveAgentEnrolment()

          mockStorePartnershipInformation(
            testVatNumber,
            Some(testSaUtr),
            Some(testBusinessPostcode)
          )(Future.successful(Left(StorePartnershipKnownFactsFailure)))

          val result = TestCheckYourAnswersPartnershipController.submit(testPostRequest(
            entityType = Some(GeneralPartnership),
            sautr = Some(testSaUtr),
            postCode = Some(testBusinessPostcode)
          ))

          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(errorRoutes.CouldNotConfirmPartnershipController.show().url)
        }
      }
      "store partnership info returned KnownFactsMismatchFailure for a limited partnership" should {
        "go to Could not confirm partnership Page" in {
          mockAuthRetrieveAgentEnrolment()
          mockStorePartnershipInformation(
            testVatNumber,
            Some(testSaUtr),
            testCompanyNumber,
            PartnershipEntityType.LimitedPartnership,
            Some(testBusinessPostcode)
          )(Future.successful(Left(StorePartnershipKnownFactsFailure)))

          val result = TestCheckYourAnswersPartnershipController.submit(testPostRequest(
            entityType = Some(LimitedPartnership),
            partnershipEntityType = Some(PartnershipEntityType.LimitedPartnership),
            companyNumber = Some(testCompanyNumber),
            sautr = Some(testSaUtr),
            postCode = Some(testBusinessPostcode)
          ))

          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(errorRoutes.CouldNotConfirmPartnershipController.show().url)
        }
      }
      "store partnership info returned StorePartnershipInformationFailureResponse" should {
        "throw Internal Server Exception" in {
          mockAuthRetrieveAgentEnrolment()
          mockStorePartnershipInformation(
            testVatNumber,
            Some(testSaUtr),
            Some(testBusinessPostcode)
          )(Future.successful(Left(StorePartnershipInformationFailureResponse(500))))

          intercept[InternalServerException](TestCheckYourAnswersPartnershipController.submit(testPostRequest(
            entityType = Some(GeneralPartnership),
            sautr = Some(testSaUtr),
            postCode = Some(testBusinessPostcode)
          )))
        }
      }
    }
    "store partnership info returned PartnershipUtrNotFound" should {
      "go to Could not confirm partnership Page" in {
        mockAuthRetrieveAgentEnrolment()
        mockStorePartnershipInformation(
          testVatNumber,
          Some(testSaUtr),
          Some(testBusinessPostcode)
        )(Future.successful(Left(PartnershipUtrNotFound)))

        val result = TestCheckYourAnswersPartnershipController.submit(testPostRequest(
          entityType = Some(GeneralPartnership),
          sautr = Some(testSaUtr),
          postCode = Some(testBusinessPostcode)
        ))

        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(errorRoutes.CouldNotConfirmPartnershipController.show().url)
      }
    }
    "store partnership info returned PartnershipUtrNotFound for a limited partnership" should {
      "go to Could not confirm partnership page" in {
        mockAuthRetrieveAgentEnrolment()
        mockStorePartnershipInformation(
          testVatNumber,
          Some(testSaUtr),
          testCompanyNumber,
          PartnershipEntityType.LimitedPartnership,
          Some(testBusinessPostcode)
        )(Future.successful(Left(PartnershipUtrNotFound)))

        val result = TestCheckYourAnswersPartnershipController.submit(testPostRequest(
          entityType = Some(LimitedPartnership),
          partnershipEntityType = Some(PartnershipEntityType.LimitedPartnership),
          companyNumber = Some(testCompanyNumber),
          sautr = Some(testSaUtr),
          postCode = Some(testBusinessPostcode)
        ))

        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(errorRoutes.CouldNotConfirmPartnershipController.show().url)

      }
    }
    "vat number is missing" should {
      "go to capture vat number page" in {
        mockAuthRetrieveAgentEnrolment()

        val result = TestCheckYourAnswersPartnershipController.submit(testPostRequest(
          vatNumber = None,
          entityType = Some(LimitedPartnership)
        ))

        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(agentRoutes.CaptureVatNumberController.show().url)
      }
    }
    "sa utr is missing" should {
      "go to capture business entity page" in {
        mockAuthRetrieveAgentEnrolment()

        val result = TestCheckYourAnswersPartnershipController.submit(testPostRequest(
          vatNumber = Some(testVatNumber),
          entityType = Some(LimitedPartnership),
          companyNumber = Some(testCompanyNumber),
          sautr = None
        ))

        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(agentRoutes.CaptureBusinessEntityController.show().url)
      }
    }
  }

}
