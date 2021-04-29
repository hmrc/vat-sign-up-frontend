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

package uk.gov.hmrc.vatsignupfrontend.controllers.principal

import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
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
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.check_your_answers_final

import scala.concurrent.Future

class CheckYourAnswersFinalControllerSpec extends UnitSpec with GuiceOneAppPerSuite
  with MockVatControllerComponents
  with MockStoreVatNumberService
  with MockSubmissionService
  with MockSubscriptionRequestSummaryConnector
  with MockGetCompanyNameService
  with MockAdministrativeDivisionLookupService {

  val view = app.injector.instanceOf[check_your_answers_final]

  object TestCheckYourAnswersFinalController
    extends CheckYourAnswersFinalController(
      mockStoreVatNumberService, mockSubscriptionRequestSummaryConnector, mockSubmissionService,
      mockGetCompanyNameService, mockAdministrativeDivisionLookupService, view
    )

  def testGetRequest(vatNumber: Option[String] = Some(testVatNumber)
                    ): FakeRequest[AnyContentAsEmpty.type] =
    FakeRequest("GET", "/check-your-answers-final").withSession(
      SessionKeys.vatNumberKey -> vatNumber.getOrElse("")
    )

  def testPostRequest(vatNumber: Option[String] = Some(testVatNumber),
                      hasDirectDebit: Option[String] = Some("true"),
                      acceptedDirectDebit: Option[String] = Some("true")
                     ): FakeRequest[AnyContentAsEmpty.type] =
    FakeRequest("POST", "/check-your-answers-final").withSession(
      SessionKeys.vatNumberKey -> vatNumber.getOrElse(""),
      SessionKeys.hasDirectDebitKey -> hasDirectDebit.getOrElse(""),
      SessionKeys.acceptedDirectDebitTermsKey -> acceptedDirectDebit.getOrElse("")
    )

  "Show action on the Check Your Answer Final Controller" when {
    "vat number is missing from session" should {
      "go to Capture Vat page" in {
        mockAuthAdminRole()

        val res = TestCheckYourAnswersFinalController.show(testGetRequest(vatNumber = None))
        status(res) shouldBe SEE_OTHER
        redirectLocation(res) shouldBe Some(routes.CaptureVatNumberController.show().url)
      }
    }
    "vat number is in session" should {
      "subscription request summary returns internal server error" should {
        "return Internal Server Error" in {
          mockAuthAdminRole()
          mockGetSubscriptionRequest(testVatNumber)(
            Future.successful(Left(SubscriptionRequestUnexpectedError(INTERNAL_SERVER_ERROR, "Unexpected status from Backend")))
          )
          intercept[InternalServerException](TestCheckYourAnswersFinalController.show(testGetRequest()))
        }
      }
      "subscription request summary returns other errors regarding data" should {
        "redirect to Capture VAT page - restart the journey" in {
          mockAuthAdminRole()
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
              mockAuthAdminRole()
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
              mockAuthAdminRole()
              mockGetSubscriptionRequest(testVatNumber)(
                Future.successful(Right(
                  SubscriptionRequestSummary(testVatNumber, LimitedCompany, None, Some(testCompanyNumber), None, None, testEmail, Digital)))
              )
              mockGetCompanyNameFailure(testCompanyNumber)
              intercept[InternalServerException](TestCheckYourAnswersFinalController.show(testGetRequest()))

            }
          }
          "get company name service returns Company Closed" should {
            "return Internal Server Error" in {
              mockAuthAdminRole()
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
                mockAuthAdminRole()
                mockGetSubscriptionRequest(testVatNumber)(
                  Future.successful(Right(
                    SubscriptionRequestSummary(testVatNumber, LimitedCompany, None, Some(testCompanyNumber), None, None, testEmail, Digital)))
                )
                mockGetCompanyNameSuccess(testCompanyNumber, NonPartnershipEntity)

                val res = TestCheckYourAnswersFinalController.show(testGetRequest())
                status(res) shouldBe OK
              }
              "for Limited Partnership" in {
                mockAuthAdminRole()
                mockGetSubscriptionRequest(testVatNumber)(
                  Future.successful(Right(
                    SubscriptionRequestSummary(testVatNumber, LimitedPartnership, None, Some(testCompanyNumber), None, None, testEmail, Digital)))
                )
                mockGetCompanyNameSuccess(testCompanyNumber, LimitedLiabilityPartnership)

                val res = TestCheckYourAnswersFinalController.show(testGetRequest())
                status(res) shouldBe OK
              }
              "for Registered Society" in {
                mockAuthAdminRole()
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
              mockAuthAdminRole()
              mockGetSubscriptionRequest(testVatNumber)(
                Future.successful(Right(
                  SubscriptionRequestSummary(testVatNumber, SoleTrader, Some(testNino), None, None, None, testEmail, Digital)))
              )

              val res = TestCheckYourAnswersFinalController.show(testGetRequest())
              status(res) shouldBe OK
            }
            "for General Partnership" in {
              mockAuthAdminRole()
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

  }
  "Submit action on the Check Your Answer Final Controller" when {
    "the user does not have the direct debit attribute on the control list" should {
      "submit successfully and go to information received" in {
        mockAuthAdminRole()
        mockSubmitSuccess(testVatNumber)

        val result = TestCheckYourAnswersFinalController.submit(testPostRequest())

        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.InformationReceivedController.show().url)
      }

      "not submit" when {
        "the vat number is missing and go to resolve vat number" in {
          mockAuthAdminRole()
          val result = TestCheckYourAnswersFinalController.submit(testPostRequest(vatNumber = None))
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.ResolveVatNumberController.resolve().url)
        }
      }
    }

    "the user does have the direct debit attribute on the control list" should {
      "VAT number isn't in session but acceptedDirectDebit is in session" should {
        "goto resolve vat number" in {
          mockAuthAdminRole()
          val result = TestCheckYourAnswersFinalController.submit(testPostRequest(vatNumber = None))

          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.ResolveVatNumberController.resolve().url)
        }
      }

      "VAT number is in session and acceptedDirectDebit isn't in session" should {
        "goto accept direct debit terms and conditions" in {
          mockAuthAdminRole()
          val result = TestCheckYourAnswersFinalController.submit(testPostRequest(acceptedDirectDebit = Some("false")))

          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.DirectDebitTermsAndConditionsController.show().url)
        }
      }

      "VAT number is in session and acceptedDirectDebit is false" should {
        "goto accept direct debit terms and conditions" in {
          mockAuthAdminRole()
          val result = TestCheckYourAnswersFinalController.submit(testPostRequest(acceptedDirectDebit = Some("false")))

          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.DirectDebitTermsAndConditionsController.show().url)
        }
      }
    }

    "submission service fails" should {
      "goto technical difficulties" in {
        mockAuthAdminRole()
        mockSubmitFailure(testVatNumber)
        intercept[InternalServerException] {
          TestCheckYourAnswersFinalController.submit(testPostRequest())
        }
      }
    }
  }


}