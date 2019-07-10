/*
 * Copyright 2019 HM Revenue & Customs
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

import java.time.LocalDate

import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.libs.json.Json
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.http.InternalServerException
import uk.gov.hmrc.play.test.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch.{DirectDebitTermsJourney, FinalCheckYourAnswer}
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockControllerComponents
import uk.gov.hmrc.vatsignupfrontend.connectors.mocks.MockSubscriptionRequestSummaryConnector
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.httpparsers.SubscriptionRequestSummaryHttpParser.{SubscriptionRequestExistsButNotComplete, SubscriptionRequestUnexpectedError}
import uk.gov.hmrc.vatsignupfrontend.models._
import uk.gov.hmrc.vatsignupfrontend.services.mocks.{MockStoreVatNumberService, MockSubmissionService}

import scala.concurrent.Future

class CheckYourAnswersFinalControllerSpec extends UnitSpec with GuiceOneAppPerSuite
  with MockControllerComponents
  with MockStoreVatNumberService
  with MockSubmissionService
  with MockSubscriptionRequestSummaryConnector {

  object TestCheckYourAnswersFinalController
    extends CheckYourAnswersFinalController(mockControllerComponents, mockStoreVatNumberService, mockSubscriptionRequestSummaryConnector, mockSubmissionService)

  val testDate: DateModel = DateModel.dateConvert(LocalDate.now())

  override def beforeEach(): Unit = {
    enable(FinalCheckYourAnswer)
  }

  def testGetRequest(vatNumber: Option[String] = Some(testVatNumber),
                     optBusinessEntity: Option[String] = None,
                     optNino: Option[String] = Some(testNino),
                     optPartnershipUtr: Option[String] = Some(testCompanyUtr),
                     optCompanyNumber: Option[String] = Some(testCompanyNumber),
                     optCompanyName: Option[String] = Some(testCompanyName),
                     optRegisteredSocietyCompanyNumber: Option[String] = None,
                     optRegisteredSocietyCompanyName: Option[String] = None,
                     emailAddress: Option[String] = Some(testEmail),
                     contactPreference: Option[String] = Some(ContactPreference.DigitalKey)
                    ): FakeRequest[AnyContentAsEmpty.type] =
    FakeRequest("GET", "/check-your-answers-final").withSession(
      SessionKeys.vatNumberKey -> vatNumber.getOrElse(""),
      SessionKeys.businessEntityKey -> optBusinessEntity.map(Json.toJson(_).toString()).getOrElse(""),
      SessionKeys.ninoKey -> optNino.map(Json.toJson(_).toString()).getOrElse(""),
      SessionKeys.partnershipSautrKey -> optPartnershipUtr.getOrElse(""),
      SessionKeys.companyNumberKey -> optCompanyNumber.getOrElse(""),
      SessionKeys.companyNameKey -> optCompanyName.getOrElse(""),
      SessionKeys.registeredSocietyCompanyNumberKey -> optRegisteredSocietyCompanyNumber.getOrElse(""),
      SessionKeys.registeredSocietyNameKey -> optRegisteredSocietyCompanyName.getOrElse(""),
      SessionKeys.emailKey -> emailAddress.getOrElse(""),
      SessionKeys.contactPreferenceKey -> contactPreference.getOrElse("")
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

          intercept[InternalServerException](await(TestCheckYourAnswersFinalController.show(testGetRequest())))
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
        "the business entity is Sole trader" should {
          "nino is missing from the session" should {
            "go to Capture Nino page" in {
              mockAuthAdminRole()
              mockGetSubscriptionRequest(testVatNumber)(
                Future.successful(Right(SubscriptionRequestSummary(testVatNumber, SoleTrader, None, testEmail, Digital)))
              )

              val res = TestCheckYourAnswersFinalController.show(
                testGetRequest(
                  optBusinessEntity = Some(BusinessEntity.SoleTraderKey),
                  optNino = None
                ))
              status(res) shouldBe SEE_OTHER
              redirectLocation(res) shouldBe Some(soletrader.routes.CaptureNinoController.show().url)
            }
          }
          "nino is in session" should {
            "display the Check Your Answer Final page" in {
              mockAuthAdminRole()
              mockGetSubscriptionRequest(testVatNumber)(
                Future.successful(Right(SubscriptionRequestSummary(testVatNumber, SoleTrader, None, testEmail, Digital)))
              )

              val res = TestCheckYourAnswersFinalController.show(testGetRequest(optBusinessEntity = Some(BusinessEntity.SoleTraderKey)))
              status(res) shouldBe OK
            }
          }
        }
        "the business entity is Limited Company" should {
          "company number is missing from the session" should {
            "go to Capture Company Number page" in {
              mockAuthAdminRole()
              mockGetSubscriptionRequest(testVatNumber)(
                Future.successful(Right(SubscriptionRequestSummary(testVatNumber, LimitedCompany, None, testEmail, Digital))))

              val res = TestCheckYourAnswersFinalController.show(
                testGetRequest(
                  optBusinessEntity = Some(BusinessEntity.LimitedCompanyKey),
                  optCompanyNumber = None
                ))
              status(res) shouldBe SEE_OTHER
              redirectLocation(res) shouldBe Some(routes.CaptureCompanyNumberController.show().url)
            }
          }
          "all data are in the session" should {
            "display the Check Your Answer Final page" in {
              mockAuthAdminRole()
              mockGetSubscriptionRequest(testVatNumber)(
                Future.successful(Right(SubscriptionRequestSummary(testVatNumber, LimitedCompany, None, testEmail, Digital)))
              )

              val res = TestCheckYourAnswersFinalController.show(testGetRequest(optBusinessEntity = Some(BusinessEntity.LimitedCompanyKey)))
              status(res) shouldBe OK
            }
          }
        }
        "the business entity is Limited Partnership" should {
          "company number is missing from the session" should {
            "go to Capture Partnership Company Number page" in {
              mockAuthAdminRole()
              mockGetSubscriptionRequest(testVatNumber)(
                Future.successful(Right(SubscriptionRequestSummary(testVatNumber, LimitedPartnership, None, testEmail, Digital)))
              )

              val res = TestCheckYourAnswersFinalController.show(
                testGetRequest(
                  optBusinessEntity = Some(BusinessEntity.LimitedPartnershipKey),
                  optCompanyNumber = None
                ))
              status(res) shouldBe SEE_OTHER
              redirectLocation(res) shouldBe Some(partnerships.routes.CapturePartnershipCompanyNumberController.show().url)
            }
          }
          "partnership UTR is missing from the session" should {
            "go to Capture Partnership UTR page" in {
              mockAuthAdminRole()
              mockGetSubscriptionRequest(testVatNumber)(
                Future.successful(Right(SubscriptionRequestSummary(testVatNumber, LimitedPartnership, None, testEmail, Digital)))
              )

              val res = TestCheckYourAnswersFinalController.show(
                testGetRequest(
                  optBusinessEntity = Some(BusinessEntity.LimitedPartnershipKey),
                  optPartnershipUtr = None
                ))
              status(res) shouldBe SEE_OTHER
              redirectLocation(res) shouldBe Some(partnerships.routes.CapturePartnershipUtrController.show().url)
            }
          }
          "all data are in the session" should {
            "display the Check Your Answer Final page" in {
              mockAuthAdminRole()
              mockGetSubscriptionRequest(testVatNumber)(
                Future.successful(Right(SubscriptionRequestSummary(testVatNumber, LimitedPartnership, None, testEmail, Digital)))
              )

              val res = TestCheckYourAnswersFinalController.show(testGetRequest(optBusinessEntity = Some(BusinessEntity.LimitedPartnershipKey)))
              status(res) shouldBe OK
            }
          }
        }
        "the business entity is General Partnership" should {
          "partnership UTR is missing from the session" should {
            "go to Capture Partnership UTR page" in {
              mockAuthAdminRole()
              mockGetSubscriptionRequest(testVatNumber)(
                Future.successful(Right(SubscriptionRequestSummary(testVatNumber, GeneralPartnership, None, testEmail, Digital)))
              )

              val res = TestCheckYourAnswersFinalController.show(
                testGetRequest(
                  optBusinessEntity = Some(BusinessEntity.GeneralPartnershipKey),
                  optPartnershipUtr = None
                ))
              status(res) shouldBe SEE_OTHER
              redirectLocation(res) shouldBe Some(partnerships.routes.CapturePartnershipUtrController.show().url)
            }
          }
          "all data are in the session" should {
            "display the Check Your Answer Final page" in {
              mockAuthAdminRole()
              mockGetSubscriptionRequest(testVatNumber)(
                Future.successful(Right(SubscriptionRequestSummary(testVatNumber, GeneralPartnership, None, testEmail, Digital)))
              )

              val res = TestCheckYourAnswersFinalController.show(testGetRequest(optBusinessEntity = Some(BusinessEntity.GeneralPartnershipKey)))
              status(res) shouldBe OK

            }
          }
        }
        "the business entity is Registered Society" should {
          "company number is missing from the session" should {
            "go to Capture Registered Society Company Number page" in {
              mockAuthAdminRole()
              mockGetSubscriptionRequest(testVatNumber)(
                Future.successful(Right(SubscriptionRequestSummary(testVatNumber, RegisteredSociety, None, testEmail, Digital)))
              )

              val res = TestCheckYourAnswersFinalController.show(
                testGetRequest(
                  optBusinessEntity = Some(BusinessEntity.RegisteredSocietyKey),
                  optCompanyNumber = None
                ))
              status(res) shouldBe SEE_OTHER
              redirectLocation(res) shouldBe Some(routes.CaptureRegisteredSocietyCompanyNumberController.show().url)
            }
          }
          "all data are in the session" should {
            "display the Check Your Answer Final page" in {
              mockAuthAdminRole()
              mockGetSubscriptionRequest(testVatNumber)(
                Future.successful(Right(SubscriptionRequestSummary(testVatNumber, RegisteredSociety, None, testEmail, Digital)))
              )

              val res = TestCheckYourAnswersFinalController.show(
                testGetRequest(
                  optBusinessEntity = Some(BusinessEntity.RegisteredSocietyKey),
                  optRegisteredSocietyCompanyNumber = Some(testCompanyNumber),
                  optRegisteredSocietyCompanyName = Some(testCompanyName)
                ))
              status(res) shouldBe OK
            }
          }
        }
        "the business entity is other than the ones specified" should {
          "all data are in the session" should {
            "display the Check Your Answer Final page" in {
              mockAuthAdminRole()
              mockGetSubscriptionRequest(testVatNumber)(
                Future.successful(Right(SubscriptionRequestSummary(testVatNumber, Trust, None, testEmail, Digital)))
              )

              val res = TestCheckYourAnswersFinalController.show(testGetRequest(optBusinessEntity = Some(BusinessEntity.TrustKey)))
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
          enable(DirectDebitTermsJourney)

          mockAuthAdminRole()
          val result = TestCheckYourAnswersFinalController.submit(testPostRequest(acceptedDirectDebit = Some("false")))

          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.DirectDebitTermsAndConditionsController.show().url)
        }
      }

      "VAT number is in session and acceptedDirectDebit is false" should {
        "goto accept direct debit terms and conditions" in {
          enable(DirectDebitTermsJourney)

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
          await(TestCheckYourAnswersFinalController.submit(testPostRequest()))
        }
      }
    }
  }


}