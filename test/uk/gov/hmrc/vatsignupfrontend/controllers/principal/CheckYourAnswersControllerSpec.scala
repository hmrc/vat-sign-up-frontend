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

import java.time.LocalDate
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.libs.json.Json
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.http.InternalServerException
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockVatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.controllers.principal.error.{routes => errorRoutes}
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.httpparsers.{ClaimSubscriptionHttpParser, StoreMigratedVatNumberHttpParser}
import uk.gov.hmrc.vatsignupfrontend.models._
import uk.gov.hmrc.vatsignupfrontend.services.StoreVatNumberService.VatNumberStored
import uk.gov.hmrc.vatsignupfrontend.services.mocks.{MockClaimSubscriptionService, MockStoreMigratedVatNumberService, MockStoreVatNumberService}
import uk.gov.hmrc.vatsignupfrontend.utils.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.check_your_answers

import scala.concurrent.Future

class CheckYourAnswersControllerSpec extends UnitSpec with GuiceOneAppPerSuite
  with MockVatControllerComponents
  with MockStoreVatNumberService
  with MockStoreMigratedVatNumberService
  with MockClaimSubscriptionService {

  val view = app.injector.instanceOf[check_your_answers]

  object TestCheckYourAnswersController extends CheckYourAnswersController(
    mockStoreVatNumberService,
    mockStoreMigratedVatNumberService,
    mockClaimSubscriptionService,
    view
  )

  val testDate: DateModel = DateModel.dateConvert(LocalDate.now())

  def testGetRequest(vatNumber: Option[String] = Some(testVatNumber),
                     registrationDate: Option[DateModel] = Some(testDate),
                     postCode: Option[PostCode] = Some(testBusinessPostcode),
                     optBox5Figure: Option[String] = Some(testBox5Figure),
                     optLastReturnMonth: Option[String] = Some(testLastReturnMonthPeriod),
                     optPreviousVatReturn: Option[String] = Some(Yes.stringValue),
                     optBusinessEntity: Option[String] = None
                    ): FakeRequest[AnyContentAsEmpty.type] =
    FakeRequest("GET", "/check-your-answers").withSession(
      SessionKeys.vatNumberKey -> vatNumber.getOrElse(""),
      SessionKeys.vatRegistrationDateKey -> registrationDate.map(Json.toJson(_).toString()).getOrElse(""),
      SessionKeys.businessPostCodeKey -> postCode.map(Json.toJson(_).toString()).getOrElse(""),
      SessionKeys.box5FigureKey -> optBox5Figure.getOrElse(""),
      SessionKeys.lastReturnMonthPeriodKey -> optLastReturnMonth.getOrElse(""),
      SessionKeys.previousVatReturnKey -> optPreviousVatReturn.getOrElse(""),
      SessionKeys.businessEntityKey -> optBusinessEntity.getOrElse("")
    )

  def testPostRequest(vatNumber: Option[String] = Some(testVatNumber),
                      registrationDate: Option[DateModel] = Some(testDate),
                      postCode: Option[PostCode] = Some(testBusinessPostcode),
                      optBox5Figure: Option[String] = Some(testBox5Figure),
                      optLastReturnMonth: Option[String] = Some(testLastReturnMonthPeriod),
                      optPreviousVatReturn: Option[String] = Some(Yes.stringValue),
                      optBusinessEntity: Option[String] = None
                     ): FakeRequest[AnyContentAsEmpty.type] =
    FakeRequest("POST", "/check-your-answers").withSession(
      SessionKeys.vatNumberKey -> vatNumber.getOrElse(""),
      SessionKeys.vatRegistrationDateKey -> registrationDate.map(Json.toJson(_).toString()).getOrElse(""),
      SessionKeys.businessPostCodeKey -> postCode.map(Json.toJson(_).toString()).getOrElse(""),
      SessionKeys.box5FigureKey -> optBox5Figure.getOrElse(""),
      SessionKeys.lastReturnMonthPeriodKey -> optLastReturnMonth.getOrElse(""),
      SessionKeys.previousVatReturnKey -> optPreviousVatReturn.getOrElse(""),
      SessionKeys.businessEntityKey -> optBusinessEntity.getOrElse("")
    )

  "Calling the show action of the Check your answers controller" when {
    "all prerequisite data are in session" should {
      "go to the Check your answers page" in {
        mockAuthAdminRole()

        val result = TestCheckYourAnswersController.show(testGetRequest())
        status(result) shouldBe Status.OK
        contentType(result) shouldBe Some("text/html")
        charset(result) shouldBe Some("utf-8")
      }
    }

    "vat number is missing" should {
      "go to capture vat number page" in {
        mockAuthAdminRole()

        val result = TestCheckYourAnswersController.show(testGetRequest(vatNumber = None))
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.CaptureVatNumberController.show().url)
      }
    }

    "vat registration date is missing" should {
      "go to capture vat registration date page" in {
        mockAuthAdminRole()

        val result = TestCheckYourAnswersController.show(testGetRequest(registrationDate = None))
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.CaptureVatRegistrationDateController.show().url)
      }
    }

    "post code is missing" should {
      "go to business post code page" in {
        mockAuthAdminRole()

        val result = TestCheckYourAnswersController.show(testGetRequest(postCode = None))
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.BusinessPostCodeController.show().url)
      }
    }

    "Overseas entity is in session" should {
      "show page without postcode" in {
        mockAuthAdminRole()

        val result = TestCheckYourAnswersController.show(testGetRequest(optBusinessEntity = Some(Overseas.toString)))
        status(result) shouldBe Status.OK
        contentType(result) shouldBe Some("text/html")
        charset(result) shouldBe Some("utf-8")
      }
    }

    "all prerequisite data is in session" when {
      "vat number is missing" should {
        "go to capture vat number page" in {
          mockAuthAdminRole()

          val result = TestCheckYourAnswersController.show(testGetRequest(vatNumber = None))
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.CaptureVatNumberController.show().url)
        }
      }

      "vat registration date is missing" should {
        "go to capture vat registration date page" in {
          mockAuthAdminRole()

          val result = TestCheckYourAnswersController.show(testGetRequest(registrationDate = None))
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.CaptureVatRegistrationDateController.show().url)
        }
      }

      "post code is missing" should {
        "go to business post code page" in {
          mockAuthAdminRole()

          val result = TestCheckYourAnswersController.show(testGetRequest(postCode = None))
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.BusinessPostCodeController.show().url)
        }
      }

      "previous VAT return is missing" should {
        "go to Previous VAT return page" in {
          mockAuthAdminRole()

          val result = TestCheckYourAnswersController.show(testGetRequest(optPreviousVatReturn = None))
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.PreviousVatReturnController.show().url)
        }
      }

      "the box 5 figure is missing" should {
        "go to the capture box 5 figure page" in {
          mockAuthAdminRole()

          val result = TestCheckYourAnswersController.show(testGetRequest(optBox5Figure = None))
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.CaptureBox5FigureController.show().url)
        }
      }

      "the last return month is missing" should {
        "go to the capture last return month page" in {
          mockAuthAdminRole()

          val result = TestCheckYourAnswersController.show(testGetRequest(optLastReturnMonth = None))
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.CaptureLastReturnMonthPeriodController.show().url)
        }
      }

      "the user is migrated so only has two known facts" in {
        mockAuthAdminRole()

        val result = TestCheckYourAnswersController.show(
          testGetRequest(optBox5Figure = None, optLastReturnMonth = None, optPreviousVatReturn = None).withSession(
            SessionKeys.isMigratedKey -> "true")
        )

        status(result) shouldBe Status.OK
      }

      "the user is migrated and overseas so only has reg date" in {
        mockAuthAdminRole()

        val result = TestCheckYourAnswersController.show(
          testGetRequest(postCode = None, optBox5Figure = None, optLastReturnMonth = None, optPreviousVatReturn = None).withSession(
            SessionKeys.isMigratedKey -> "true",
            SessionKeys.businessEntityKey -> Overseas.toString
          )
        )

        status(result) shouldBe Status.OK
      }

      "the user is already subscribed and overseas" in {
        mockAuthAdminRole()

        val result = TestCheckYourAnswersController.show(
          testGetRequest(postCode = None, optBox5Figure = None, optLastReturnMonth = None, optPreviousVatReturn = None).withSession(
            SessionKeys.isAlreadySubscribedKey -> "true",
            SessionKeys.businessEntityKey -> Overseas.toString
          )
        )

        status(result) shouldBe Status.OK
      }
    }
  }

  "Calling the submit action of the Check your answers controller" when {
    "all prerequisite data are in" when {
      "store vat number returned VatNumberStored" should {
        "goto business entity controller" in {
          mockAuthRetrieveEmptyEnrolment()
          mockStoreVatNumberSuccess(
            vatNumber = testVatNumber,
            optPostCode = Some(testBusinessPostcode),
            registrationDate = testDate,
            optBox5Figure = Some(testBox5Figure),
            optLastReturnMonth = Some(testLastReturnMonthPeriod),
            isFromBta = false
          )

          val result = TestCheckYourAnswersController.submit(testPostRequest())
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) should contain(routes.CaptureBusinessEntityController.show().url)
          session(result) get SessionKeys.hasDirectDebitKey should contain("false")
        }
      }

      "store vat number returned VatNumberStored with direct debits" should {
        "goto business entity controller" in {
          mockAuthRetrieveEmptyEnrolment()
          mockStoreVatNumberDirectDebitSuccess(
            vatNumber = testVatNumber,
            optPostCode = Some(testBusinessPostcode),
            registrationDate = testDate,
            optBox5Figure = Some(testBox5Figure),
            optLastReturnMonth = Some(testLastReturnMonthPeriod),
            isFromBta = false
          )

          val result = TestCheckYourAnswersController.submit(testPostRequest())
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) should contain(routes.CaptureBusinessEntityController.show().url)
          session(result) get SessionKeys.hasDirectDebitKey should contain("true")
        }
      }

      "store vat number returned VatNumberStored with direct debits and isOverseas flag set to true" should {
        "goto business entity controller" in {
          mockAuthRetrieveEmptyEnrolment()

          mockStoreVatNumber(
            vatNumber = testVatNumber,
            optPostCode = None,
            registrationDate = testDate,
            optBox5Figure = Some(testBox5Figure),
            optLastReturnMonth = Some(testLastReturnMonthPeriod),
            isFromBta = false
          )(Future.successful(Right(VatNumberStored(isOverseas = true, isDirectDebit = false))))

          val result = TestCheckYourAnswersController.submit(
            testPostRequest(optBusinessEntity = Some(Overseas.toString))
          )

          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) should contain(routes.CaptureBusinessEntityController.show().url)
          session(result) get SessionKeys.hasDirectDebitKey should contain("false")
          session(result) get SessionKeys.businessEntityKey should contain("overseas")
        }
      }

      "store vat number returned SubscriptionClaimed" when {
        "goto sign up complete controller" in {
          mockAuthRetrieveEmptyEnrolment()
          mockStoreVatNumberSubscriptionClaimed(
            vatNumber = testVatNumber,
            optPostCode = Some(testBusinessPostcode),
            registrationDate = testDate,
            optBox5Figure = Some(testBox5Figure),
            optLastReturnMonth = Some(testLastReturnMonthPeriod),
            isFromBta = false
          )

          val result = TestCheckYourAnswersController.submit(testPostRequest())
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) should contain(routes.SignUpCompleteClientController.show().url)
        }
      }

      "store vat number returned KnownFactsMismatch" should {
        "go to the could not confirm business page" in {
          mockAuthRetrieveEmptyEnrolment()
          mockStoreVatNumberKnownFactsMismatch(
            vatNumber = testVatNumber,
            optPostCode = Some(testBusinessPostcode),
            registrationDate = testDate,
            optBox5Figure = Some(testBox5Figure),
            optLastReturnMonth = Some(testLastReturnMonthPeriod),
            isFromBta = false
          )

          val result = TestCheckYourAnswersController.submit(testPostRequest())
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(errorRoutes.VatCouldNotConfirmBusinessController.show().url)
        }
      }

      "store vat number returned InvalidVatNumber" should {
        "go to the invalid vat number page" in {
          mockAuthRetrieveEmptyEnrolment()
          mockStoreVatNumberInvalid(
            vatNumber = testVatNumber,
            optPostCode = Some(testBusinessPostcode),
            registrationDate = testDate,
            optBox5Figure = Some(testBox5Figure),
            optLastReturnMonth = Some(testLastReturnMonthPeriod),
            isFromBta = false
          )

          val result = TestCheckYourAnswersController.submit(testPostRequest())
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(errorRoutes.InvalidVatNumberController.show().url)
        }
      }

      "store vat number returned IneligibleVatNumber" should {
        "go to the could not use service page" in {
          mockAuthRetrieveEmptyEnrolment()
          mockStoreVatNumberIneligible(
            vatNumber = testVatNumber,
            optPostCode = Some(testBusinessPostcode),
            registrationDate = testDate,
            optBox5Figure = Some(testBox5Figure),
            optLastReturnMonth = Some(testLastReturnMonthPeriod),
            isFromBta = false
          )

          val result = TestCheckYourAnswersController.submit(testPostRequest())
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(errorRoutes.CannotUseServiceController.show().url)
        }
      }

      "store vat number returned VatMigrationInProgress" should {
        "go to the migration in progress error page" in {
          mockAuthRetrieveEmptyEnrolment()
          mockStoreVatNumberMigrationInProgress(
            vatNumber = testVatNumber,
            optPostCode = Some(testBusinessPostcode),
            registrationDate = testDate,
            optBox5Figure = Some(testBox5Figure),
            optLastReturnMonth = Some(testLastReturnMonthPeriod),
            isFromBta = false
          )

          val result = TestCheckYourAnswersController.submit(testPostRequest())
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(errorRoutes.MigrationInProgressErrorController.show().url)
        }
      }

      "store vat number returned VatNumberAlreadyEnrolled" should {
        "go to the business already signed up error page" in {
          mockAuthRetrieveEmptyEnrolment()
          mockStoreVatNumberAlreadyEnrolled(
            vatNumber = testVatNumber,
            optPostCode = Some(testBusinessPostcode),
            registrationDate = testDate,
            optBox5Figure = Some(testBox5Figure),
            optLastReturnMonth = Some(testLastReturnMonthPeriod),
            isFromBta = false
          )

          val result = TestCheckYourAnswersController.submit(testPostRequest())
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(errorRoutes.BusinessAlreadySignedUpController.show().url)
        }
      }

      "store vat number returned a failure" should {
        "throw internal server exception" in {
          mockAuthRetrieveEmptyEnrolment()
          mockStoreVatNumberFailure(
            vatNumber = testVatNumber,
            optPostCode = Some(testBusinessPostcode),
            registrationDate = testDate,
            optBox5Figure = Some(testBox5Figure),
            optLastReturnMonth = Some(testLastReturnMonthPeriod),
            isFromBta = false
          )

          intercept[InternalServerException] {
            TestCheckYourAnswersController.submit(testPostRequest())
          }

        }
      }

      "the user is un-enrolled " should {
        "return a successful response" in {
          mockAuthRetrieveEmptyEnrolment()
          mockStoreMigratedVatNumber(
            testVatNumber,
            Some(testDate.toDesDateFormat),
            Some(testBusinessPostcode)
          )(Future.successful(
            Right(StoreMigratedVatNumberHttpParser.StoreMigratedVatNumberSuccess)
          ))

          val result = TestCheckYourAnswersController.submit(testPostRequest().withSession(SessionKeys.isMigratedKey -> "true"))

          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.CaptureBusinessEntityController.show().url)
        }

        "throw an exception when store vat number failed" in {
          mockAuthRetrieveEmptyEnrolment()
          mockStoreMigratedVatNumber(
            testVatNumber,
            Some(testDate.toDesDateFormat),
            Some(testBusinessPostcode)
          )(Future.failed(new InternalServerException("")))

          intercept[InternalServerException] {
            TestCheckYourAnswersController.submit(testPostRequest().withSession(SessionKeys.isMigratedKey -> "true"))
          }
        }

        "redirect to the correct location with mismatching known facts" in {
          mockAuthRetrieveEmptyEnrolment()
          mockStoreMigratedVatNumber(
            testVatNumber,
            Some(testDate.toDesDateFormat),
            Some(testBusinessPostcode)
          )(
            Future.successful(Left(StoreMigratedVatNumberHttpParser.KnownFactsMismatch))
          )

          val result = TestCheckYourAnswersController.submit(testPostRequest().withSession(SessionKeys.isMigratedKey -> "true"))
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(errorRoutes.VatCouldNotConfirmBusinessController.show().toString)

        }
      }

      "the user is un-enrolled and overseas" should {
        "return a successful response" in {
          mockAuthRetrieveEmptyEnrolment()
          mockStoreMigratedVatNumber(
            testVatNumber,
            Some(testDate.toDesDateFormat),
            None
          )(Future.successful(
            Right(StoreMigratedVatNumberHttpParser.StoreMigratedVatNumberSuccess)
          ))

          val result = TestCheckYourAnswersController.submit(testPostRequest(postCode = None).withSession(
            SessionKeys.isMigratedKey -> "true", SessionKeys.businessEntityKey -> Overseas.toString
          ))

          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.CaptureBusinessEntityController.show().url)
        }

        "throw an exception when store vat number failed" in {
          mockAuthRetrieveEmptyEnrolment()
          mockStoreMigratedVatNumber(
            testVatNumber,
            Some(testDate.toDesDateFormat),
            None
          )(Future.failed(new InternalServerException("")))

          intercept[InternalServerException] {
            TestCheckYourAnswersController.submit(testPostRequest(postCode = None).withSession(
              SessionKeys.isMigratedKey -> "true", SessionKeys.businessEntityKey -> Overseas.toString
            ))
          }
        }

        "redirect to the correct location with mismatching known facts" in {
          mockAuthRetrieveEmptyEnrolment()
          mockStoreMigratedVatNumber(
            testVatNumber,
            Some(testDate.toDesDateFormat),
            None
          )(
            Future.successful(Left(StoreMigratedVatNumberHttpParser.KnownFactsMismatch))
          )

          val result = TestCheckYourAnswersController.submit(testPostRequest(postCode = None).withSession(
              SessionKeys.isMigratedKey -> "true", SessionKeys.businessEntityKey -> Overseas.toString
            ))

          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(errorRoutes.VatCouldNotConfirmBusinessController.show().toString)
        }
      }

      "the vrn is already subscribed" should {
        "redirect to SignUpCompleteClientController if claim subscription passes" in {
          mockAuthRetrieveEmptyEnrolment()
          mockClaimSubscription(
            testVatNumber,
            Some(testBusinessPostcode),
            testDate, isFromBta = false
          )(Future.successful(Right(ClaimSubscriptionHttpParser.SubscriptionClaimed)))

          val result = TestCheckYourAnswersController.submit(testPostRequest().withSession(
            SessionKeys.isAlreadySubscribedKey -> "true"
          ))

          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.SignUpCompleteClientController.show().url)
        }

        "redirect to VatCouldNotConfirmBusinessController if claim subscription fails due to known facts mismatch" in {
          mockAuthRetrieveEmptyEnrolment()
          mockClaimSubscription(
            testVatNumber,
            Some(testBusinessPostcode),
            testDate, isFromBta = false
          )(Future.successful(Left(ClaimSubscriptionHttpParser.KnownFactsMismatch)))

          val result = TestCheckYourAnswersController.submit(testPostRequest().withSession(
            SessionKeys.isAlreadySubscribedKey -> "true"
          ))

          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(errorRoutes.VatCouldNotConfirmBusinessController.show().url)
        }

        "redirect to BusinessAlreadySignedUpController if the subscription is already claimed" in {
          mockAuthRetrieveEmptyEnrolment()
          mockClaimSubscription(
            testVatNumber,
            Some(testBusinessPostcode),
            testDate, isFromBta = false
          )(Future.successful(Left(ClaimSubscriptionHttpParser.AlreadyEnrolledOnDifferentCredential)))

          val result = TestCheckYourAnswersController.submit(testPostRequest().withSession(
            SessionKeys.isAlreadySubscribedKey -> "true"
          ))

          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(errorRoutes.BusinessAlreadySignedUpController.show().url)
        }

        "throw an exception if claim subscription fails" in {
          mockAuthRetrieveEmptyEnrolment()
          mockClaimSubscription(
            testVatNumber,
            Some(testBusinessPostcode),
            testDate, isFromBta = false
          )(Future.successful(Left(ClaimSubscriptionHttpParser.ClaimSubscriptionFailureResponse(INTERNAL_SERVER_ERROR))))

          val result = TestCheckYourAnswersController.submit(testPostRequest().withSession(
            SessionKeys.isAlreadySubscribedKey -> "true"
          ))

          intercept[InternalServerException](result)
        }
      }
    }

    "vat number is missing" should {
      "go to capture vat number page" in {
        mockAuthRetrieveEmptyEnrolment()

        val result = TestCheckYourAnswersController.submit(testPostRequest(vatNumber = None))

        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.CaptureVatNumberController.show().url)
      }
    }

    "vat registration date is missing" should {
      "go to capture vat registration date page" in {
        mockAuthRetrieveEmptyEnrolment()

        val result = TestCheckYourAnswersController.submit(testPostRequest(registrationDate = None))
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.CaptureVatRegistrationDateController.show().url)
      }
    }

    "post code is missing" should {
      "go to business post code page" in {
        mockAuthRetrieveEmptyEnrolment()

        val result = TestCheckYourAnswersController.submit(testPostRequest(postCode = None))
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.BusinessPostCodeController.show().url)
      }
    }

    "the user has filed a vat return before" when {
      "the box 5 figure is missing" should {
        "go to the capture box 5 figure page" in {
          mockAuthRetrieveEmptyEnrolment()

          val result = TestCheckYourAnswersController.submit(testPostRequest(optBox5Figure = None))
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.CaptureBox5FigureController.show().url)
        }
      }

      "the last return month is missing" should {
        "go to the capture last return month page" in {
          mockAuthRetrieveEmptyEnrolment()

          val result = TestCheckYourAnswersController.submit(testPostRequest(optLastReturnMonth = None))
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.CaptureLastReturnMonthPeriodController.show().url)
        }
      }
    }
  }
}
