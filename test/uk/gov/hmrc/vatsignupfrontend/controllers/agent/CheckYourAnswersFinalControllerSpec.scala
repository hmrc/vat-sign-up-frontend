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

package uk.gov.hmrc.vatsignupfrontend.controllers.agent

import org.jsoup.Jsoup
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.http.InternalServerException
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockVatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.connectors.mocks.MockSubscriptionRequestSummaryConnector
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.httpparsers.GetCompanyNameHttpParser.CompanyClosed
import uk.gov.hmrc.vatsignupfrontend.httpparsers.SubscriptionRequestSummaryHttpParser.{SubscriptionRequestExistsButNotComplete, SubscriptionRequestUnexpectedError}
import uk.gov.hmrc.vatsignupfrontend.models._
import uk.gov.hmrc.vatsignupfrontend.models.companieshouse.{LimitedLiabilityPartnership, NonPartnershipEntity}
import uk.gov.hmrc.vatsignupfrontend.services.mocks.{MockAdministrativeDivisionLookupService, MockGetCompanyNameService, MockStoreVatNumberService, MockSubmissionService}
import uk.gov.hmrc.vatsignupfrontend.utils.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.views.html.agent.check_your_answers_final

import scala.concurrent.Future

class CheckYourAnswersFinalControllerSpec extends UnitSpec with GuiceOneAppPerSuite with MockAdministrativeDivisionLookupService
  with MockVatControllerComponents
  with MockStoreVatNumberService
  with MockSubmissionService
  with MockSubscriptionRequestSummaryConnector
  with MockGetCompanyNameService {

  val view = app.injector.instanceOf[check_your_answers_final]

  object TestCheckYourAnswersFinalController
    extends CheckYourAnswersFinalController(
      mockStoreVatNumberService,
      mockSubscriptionRequestSummaryConnector,
      mockSubmissionService,
      mockGetCompanyNameService,
      view
    )

  def testGetRequest(vatNumber: Option[String] = Some(testVatNumber)): FakeRequest[AnyContentAsEmpty.type] =
    FakeRequest("GET", "/client/check-your-answers-final").withSession(
      SessionKeys.vatNumberKey -> vatNumber.getOrElse("")
    )

  def testPostRequest(vatNumber: Option[String] = Some(testVatNumber)): FakeRequest[AnyContentAsEmpty.type] =
    FakeRequest("POST", "/client/check-your-answers-final").withSession(
      SessionKeys.vatNumberKey -> vatNumber.getOrElse("")
    )

  "Show action on the Check Your Answer Final Controller" when {
    "vat number is missing from session" should {
      "go to Capture Vat page" in {
        mockAuthRetrieveAgentEnrolment()

        val res = TestCheckYourAnswersFinalController.show(testGetRequest(vatNumber = None))
        status(res) shouldBe SEE_OTHER
        redirectLocation(res) shouldBe Some(routes.CaptureVatNumberController.show().url)
      }
    }
    "vat number is in session" should {
      "subscription request summary returns internal server error" should {
        "return Internal Server Error" in {
          mockAuthRetrieveAgentEnrolment()
          mockGetSubscriptionRequest(testVatNumber)(
            Future.successful(Left(SubscriptionRequestUnexpectedError(INTERNAL_SERVER_ERROR, "Unexpected status from Backend")))
          )
          intercept[InternalServerException](TestCheckYourAnswersFinalController.show(testGetRequest()))
        }
      }
      "subscription request summary returns other errors regarding data" should {
        "redirect to Capture VAT page - restart the journey" in {
          mockAuthRetrieveAgentEnrolment()
          mockGetSubscriptionRequest(testVatNumber)(
            Future.successful(Left(SubscriptionRequestExistsButNotComplete))
          )

          val res = TestCheckYourAnswersFinalController.show(testGetRequest())
          status(res) shouldBe SEE_OTHER
          redirectLocation(res) shouldBe Some(routes.CaptureVatNumberController.show().url)

        }
      }
      "subscription request summary returns data" should {
        "subscription request summary contains a company number" should {
          "get company name service returns NOT_FOUND error" should {
            "return Internal Server Error" in {
              mockAuthRetrieveAgentEnrolment()
              mockGetSubscriptionRequest(testVatNumber)(
                Future.successful(Right(
                  SubscriptionRequestSummary(testVatNumber, LimitedCompany, None, Some(testCompanyNumber), None, None, testEmail, Digital)))
              )
              mockGetCompanyNameNotFound(testCompanyNumber)
              intercept[InternalServerException](TestCheckYourAnswersFinalController.show(testGetRequest()))

            }
          }
          "get company name service returns INTERNAL_SERVER_ERROR" should {
            "return Internal Server Error" in {
              mockAuthRetrieveAgentEnrolment()
              mockGetSubscriptionRequest(testVatNumber)(
                Future.successful(Right(
                  SubscriptionRequestSummary(testVatNumber, LimitedCompany, None, Some(testCompanyNumber), None, None, testEmail, Digital)))
              )
              mockGetCompanyNameFailure(testCompanyNumber)
              intercept[InternalServerException](TestCheckYourAnswersFinalController.show(testGetRequest()))

            }
          }
          "get company name service returns CompanyClosed" should {
            "return Internal Server Error" in {
              mockAuthRetrieveAgentEnrolment()
              mockGetSubscriptionRequest(testVatNumber)(
                Future.successful(Right(
                  SubscriptionRequestSummary(testVatNumber, LimitedCompany, None, Some(testCompanyNumber), None, None, testEmail, Digital)))
              )
              mockGetCompanyName(testCompanyNumber)(Future.successful(Right(CompanyClosed(testCompanyName))))
              intercept[InternalServerException](TestCheckYourAnswersFinalController.show(testGetRequest()))

            }
          }
          "get company name service returns data" should {
            "display the Check Your Answers Final Page" should {
              "for Limited Company" in {
                mockAuthRetrieveAgentEnrolment()
                mockGetSubscriptionRequest(testVatNumber)(
                  Future.successful(Right(
                    SubscriptionRequestSummary(testVatNumber, LimitedCompany, None, Some(testCompanyNumber), None, None, testEmail, Digital)))
                )
                mockGetCompanyNameSuccess(testCompanyNumber, NonPartnershipEntity)

                val res = TestCheckYourAnswersFinalController.show(testGetRequest())
                status(res) shouldBe OK
              }
              "for Limited Partnership" in {
                mockAuthRetrieveAgentEnrolment()
                mockGetSubscriptionRequest(testVatNumber)(
                  Future.successful(Right(
                    SubscriptionRequestSummary(testVatNumber, LimitedPartnership, None, Some(testCompanyNumber), None, None, testEmail, Digital)))
                )
                mockGetCompanyNameSuccess(testCompanyNumber, LimitedLiabilityPartnership)

                val res = TestCheckYourAnswersFinalController.show(testGetRequest())
                status(res) shouldBe OK
              }
              "for Registered Society" in {
                mockAuthRetrieveAgentEnrolment()
                mockGetSubscriptionRequest(testVatNumber)(
                  Future.successful(Right(
                    SubscriptionRequestSummary(testVatNumber, RegisteredSociety, None, Some(testCompanyNumber), None, None, testEmail, Digital)))
                )
                mockGetCompanyNameSuccess(testCompanyNumber, NonPartnershipEntity)

                val res = TestCheckYourAnswersFinalController.show(testGetRequest())
                status(res) shouldBe OK
              }
            }
          }
        }
        "subscription request summary doesn't contain a company number" should {
          "display the Check Your Answers Final Page" should {
            "for Sole Trader" in {
              mockAuthRetrieveAgentEnrolment()
              mockGetSubscriptionRequest(testVatNumber)(
                Future.successful(Right(
                  SubscriptionRequestSummary(testVatNumber, SoleTrader, Some(testNino), None, None, None, testEmail, Digital)))
              )

              val res = TestCheckYourAnswersFinalController.show(testGetRequest())
              status(res) shouldBe OK
            }
            "for General Partnership" in {
              mockAuthRetrieveAgentEnrolment()
              mockGetSubscriptionRequest(testVatNumber)(
                Future.successful(Right(
                  SubscriptionRequestSummary(testVatNumber, GeneralPartnership, None, None, Some(testSaUtr), None, testEmail, Digital)))
              )
              val res = TestCheckYourAnswersFinalController.show(testGetRequest())
              status(res) shouldBe OK
            }
          }
        }
      }
    }
    "the business entity is division" should {
      "not contain a row for business entity" in {
        mockAuthRetrieveAgentEnrolment()
        mockGetSubscriptionRequest(testVatNumber)(
          Future.successful(Right(
            SubscriptionRequestSummary(testVatNumber, Division, None, None, None, None, testEmail, Digital)))
        )
        mockIsAdministrativeDivision(testVatNumber)(isAdministrativeDivision = true)

        val res = TestCheckYourAnswersFinalController.show(testGetRequest(vatNumber = Some(testVatNumber)))
        val doc = Jsoup.parse(contentAsString(res))
        status(res) shouldBe OK
        doc.select("#business-entity-row").isEmpty shouldBe true
      }
    }

  }
  "Submit action on the Check Your Answer Final Controller" when {
    "submission service returns a successful submission response" should {
      "redirect to the confirmation controller" in {
        mockAuthRetrieveAgentEnrolment()
        mockSubmitSuccess(testVatNumber)

        val res = TestCheckYourAnswersFinalController.submit(testPostRequest())
        status(res) shouldBe SEE_OTHER
        redirectLocation(res) shouldBe Some(routes.ConfirmationController.show().url)
      }
    }
    "submission service fails" should {
      "goto technical difficulties" in {
        mockAuthRetrieveAgentEnrolment()
        mockSubmitFailure(testVatNumber)

        intercept[InternalServerException] {
          TestCheckYourAnswersFinalController.submit(testPostRequest())
        }
      }
    }
  }

}
