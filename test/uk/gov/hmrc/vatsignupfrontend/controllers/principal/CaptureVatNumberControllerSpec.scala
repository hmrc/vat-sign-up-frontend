/*
 * Copyright 2018 HM Revenue & Customs
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
import play.api.mvc.AnyContentAsFormUrlEncoded
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.retrieve.{Retrievals, ~}
import uk.gov.hmrc.auth.core.{Admin, Enrolments}
import uk.gov.hmrc.http.InternalServerException
import uk.gov.hmrc.play.test.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.SessionKeys.vatNumberKey
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch.FeatureSwitching
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockControllerComponents
import uk.gov.hmrc.vatsignupfrontend.forms.VatNumberForm
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstantsGenerator
import uk.gov.hmrc.vatsignupfrontend.models.MigratableDates
import uk.gov.hmrc.vatsignupfrontend.services.mocks.{MockStoreVatNumberService, MockVatNumberEligibilityService}

import scala.concurrent.Future


class CaptureVatNumberControllerSpec extends UnitSpec with GuiceOneAppPerSuite
  with MockControllerComponents with MockVatNumberEligibilityService with MockStoreVatNumberService with FeatureSwitching {

  object TestCaptureVatNumberController extends CaptureVatNumberController(
    mockControllerComponents,
    mockVatNumberEligibilityService,
    mockStoreVatNumberService
  )

  lazy val testGetRequest = FakeRequest("GET", "/vat-number")

  def testPostRequest(vatNumber: String): FakeRequest[AnyContentAsFormUrlEncoded] =
    FakeRequest("POST", "/vat-number").withFormUrlEncodedBody(VatNumberForm.vatNumber -> vatNumber)

  "Calling the show action of the Capture Vat Number controller" when {
    "redirect to resolve VAT number controller" in {
      mockAuthAdminRole()

      val result = TestCaptureVatNumberController.show(testGetRequest)

      status(result) shouldBe Status.OK
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
    }

  }

  "Calling the submit action of the Capture Vat Number controller" when {
    "form successfully submitted" when {
      "the vat number passes checksum validation" when {
        "the user has a VAT-DEC enrolment" when {
          "the vat eligibility is successful" when {
            "the inserted vat number matches the enrolment one" when {
              "the VAT number is stored successfully" should {
                "redirect to the business entity type page" in {
                  mockAuthRetrieveVatDecEnrolment(hasIRSAEnrolment = false)
                  mockStoreVatNumberSuccess(testVatNumber, isFromBta = Some(false))

                  val result = TestCaptureVatNumberController.submit(testPostRequest(testVatNumber))

                  status(result) shouldBe Status.SEE_OTHER
                  redirectLocation(result) shouldBe Some(routes.CaptureBusinessEntityController.show().url)
                  session(result) get vatNumberKey should contain(testVatNumber)
                }
              }
              "the user's subscription has been claimed" should {
                "redirect to claimed subscription confirmation page" in {
                  mockAuthRetrieveVatDecEnrolment(hasIRSAEnrolment = false)
                  mockStoreVatNumberSubscriptionClaimed(testVatNumber, isFromBta = Some(false))

                  val result = TestCaptureVatNumberController.submit(testPostRequest(testVatNumber))

                  status(result) shouldBe Status.SEE_OTHER
                  redirectLocation(result) shouldBe Some(routes.SignUpCompleteClientController.show().url)
                }
              }
            }

            "the inserted vat number doesn't match the enrolment one" should {
              "redirect to error page" in {
                val testNonMatchingVat = TestConstantsGenerator.randomVatNumber
                mockAuthRetrieveVatDecEnrolment(hasIRSAEnrolment = false)

                val result = TestCaptureVatNumberController.submit(testPostRequest(testNonMatchingVat))

                status(result) shouldBe Status.SEE_OTHER
                redirectLocation(result) shouldBe Some(routes.IncorrectEnrolmentVatNumberController.show().url)
              }
            }

          }
          "the vat eligibility is unsuccessful" should {
            "redirect to Cannot use service yet when the vat number is ineligible for Making Tax Digital" in {
              mockAuthRetrieveVatDecEnrolment(hasIRSAEnrolment = false)
              mockStoreVatNumberIneligible(testVatNumber, isFromBta = Some(false), migratableDates = MigratableDates())

              val request = testPostRequest(testVatNumber)

              val result = TestCaptureVatNumberController.submit(request)
              status(result) shouldBe Status.SEE_OTHER
              redirectLocation(result) shouldBe Some(routes.CannotUseServiceController.show().url)
            }

            "the vat eligibility is unsuccessful" should {
              "redirect to sign up after this date page when the vat number is ineligible and one date is available" in {
                mockAuthRetrieveVatDecEnrolment(hasIRSAEnrolment = false)
                mockStoreVatNumberIneligible(testVatNumber, isFromBta = Some(false), migratableDates = MigratableDates(Some(testStartDate)))

                val request = testPostRequest(testVatNumber)

                val result = TestCaptureVatNumberController.submit(request)
                status(result) shouldBe Status.SEE_OTHER
                redirectLocation(result) shouldBe Some(routes.MigratableDatesController.show().url)
              }

            }
            "the vat eligibility is unsuccessful" should {
              "redirect to sign up between these dates page when the vat number is ineligible and two dates are available" in {
                mockAuthRetrieveVatDecEnrolment(hasIRSAEnrolment = false)
                mockStoreVatNumberIneligible(testVatNumber, isFromBta = Some(false), migratableDates = MigratableDates(Some(testStartDate), Some(testEndDate)))

                val request = testPostRequest(testVatNumber)

                val result = TestCaptureVatNumberController.submit(request)
                status(result) shouldBe Status.SEE_OTHER
                redirectLocation(result) shouldBe Some(routes.MigratableDatesController.show().url)
              }

            "redirect to Already Signed Up page when the vat number has already been subscribed" in {
              mockAuthRetrieveVatDecEnrolment(hasIRSAEnrolment = false)
              mockStoreVatNumberAlreadySubscribed(testVatNumber, isFromBta = Some(false))

              val request = testPostRequest(testVatNumber)

              val result = TestCaptureVatNumberController.submit(request)
              status(result) shouldBe Status.SEE_OTHER
              redirectLocation(result) shouldBe Some(routes.AlreadySignedUpController.show().url)
            }
          }
          "throw an exception for any other scenario" in {
            mockAuthRetrieveVatDecEnrolment(hasIRSAEnrolment = false)
            mockStoreVatNumberFailure(testVatNumber, isFromBta = Some(false))

            val request = testPostRequest(testVatNumber)
            intercept[InternalServerException] {
              await(TestCaptureVatNumberController.submit(request))
            }
          }

        }

      }

      "the user does not have a VAT-DEC enrolment" when {

        "redirect to the Capture Vat Registration Date page when the vat number is eligible" in {
          mockAuthorise(
            retrievals = Retrievals.credentialRole and Retrievals.allEnrolments
          )(Future.successful(new ~(Some(Admin), Enrolments(Set()))))
          mockVatNumberEligibilitySuccess(testVatNumber)

          implicit val request = testPostRequest(testVatNumber)

          val result = TestCaptureVatNumberController.submit(request)
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.CaptureVatRegistrationDateController.show().url)

          result.session get vatNumberKey should contain(testVatNumber)
        }

        "redirect to Cannot use service yet when the vat number is ineligible for Making Tax Digital" in {
          mockAuthorise(
            retrievals = Retrievals.credentialRole and Retrievals.allEnrolments
          )(Future.successful(new ~(Some(Admin), Enrolments(Set()))))
          mockVatNumberIneligibleForMtd(testVatNumber)
          
          val request = testPostRequest(testVatNumber)
          
          val result = TestCaptureVatNumberController.submit(request)
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.CannotUseServiceController.show().url)
        }
        
          "redirect to sign up after this date when the vat number is ineligible and one date is available" in {
            mockAuthorise(
              retrievals = Retrievals.credentialRole and Retrievals.allEnrolments
            )(Future.successful(new ~(Some(Admin), Enrolments(Set()))))
            mockVatNumberIneligibleForMtd(testVatNumber, migratableDates = MigratableDates(Some(testStartDate)))

            val request = testPostRequest(testVatNumber)

            val result = TestCaptureVatNumberController.submit(request)
            status(result) shouldBe Status.SEE_OTHER
            redirectLocation(result) shouldBe Some(routes.MigratableDatesController.show().url)
          }

          "redirect to sign up between these dates when the vat number is ineligible and two dates are available" in {
            mockAuthorise(
              retrievals = Retrievals.credentialRole and Retrievals.allEnrolments
            )(Future.successful(new ~(Some(Admin), Enrolments(Set()))))
            mockVatNumberIneligibleForMtd(testVatNumber, migratableDates = MigratableDates(Some(testStartDate), Some(testEndDate)))

            val request = testPostRequest(testVatNumber)

            val result = TestCaptureVatNumberController.submit(request)
            status(result) shouldBe Status.SEE_OTHER
            redirectLocation(result) shouldBe Some(routes.MigratableDatesController.show().url)
          }

          "redirect to Invalid Vat Number page when the vat number is invalid" in {
            mockAuthorise(
              retrievals = Retrievals.credentialRole and Retrievals.allEnrolments
            )(Future.successful(new ~(Some(Admin), Enrolments(Set()))))
            mockVatNumberEligibilityInvalid(testVatNumber)
            
            val request = testPostRequest(testVatNumber)
            
            val result = TestCaptureVatNumberController.submit(request)
            status(result) shouldBe Status.SEE_OTHER
            redirectLocation(result) shouldBe Some(routes.InvalidVatNumberController.show().url)


        "redirect to sign up after this date when the vat number is ineligible and one date is available" in {
          mockAuthorise(
            retrievals = Retrievals.credentialRole and Retrievals.allEnrolments
          )(Future.successful(new ~(Some(Admin), Enrolments(Set()))))
          mockVatNumberIneligibleForMtd(testVatNumber, migratableDates = MigratableDates(Some(testStartDate)))

          val request = testPostRequest(testVatNumber)

          val result = TestCaptureVatNumberController.submit(request)
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.MigratableDatesController.show().url)
        }

        "redirect to sign up between these dates when the vat number is ineligible and two dates are available" in {
          mockAuthorise(
            retrievals = Retrievals.credentialRole and Retrievals.allEnrolments
          )(Future.successful(new ~(Some(Admin), Enrolments(Set()))))
          mockVatNumberIneligibleForMtd(testVatNumber, migratableDates = MigratableDates(Some(testStartDate), Some(testEndDate)))

          val request = testPostRequest(testVatNumber)

          val result = TestCaptureVatNumberController.submit(request)
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.MigratableDatesController.show().url)
        }

        "redirect to Invalid Vat Number page when the vat number is invalid" in {
          mockAuthorise(
            retrievals = Retrievals.credentialRole and Retrievals.allEnrolments
          )(Future.successful(new ~(Some(Admin), Enrolments(Set()))))
          mockVatNumberEligibilityInvalid(testVatNumber)

          val request = testPostRequest(testVatNumber)

          val result = TestCaptureVatNumberController.submit(request)
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.InvalidVatNumberController.show().url)
        }

        "redirect to Already Signed Up page when the vat number has already been subscribed" in {
          mockAuthorise(
            retrievals = Retrievals.credentialRole and Retrievals.allEnrolments
          )(Future.successful(new ~(Some(Admin), Enrolments(Set()))))
          mockVatNumberEligibilityAlreadySubscribed(testVatNumber)

          val request = testPostRequest(testVatNumber)

          val result = TestCaptureVatNumberController.submit(request)
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.AlreadySignedUpController.show().url)
        }

        "throw an exception for any other scenario" in {
          mockAuthorise(
            retrievals = Retrievals.credentialRole and Retrievals.allEnrolments
          )(Future.successful(new ~(Some(Admin), Enrolments(Set()))))
          mockVatNumberEligibilityFailure(testVatNumber)

          val request = testPostRequest(testVatNumber)
          intercept[InternalServerException] {
            await(TestCaptureVatNumberController.submit(request))
          }
        }
      }
    }

    "the vat number fails checksum validation" should {
      "redirect to Invalid Vat Number page" in {
        mockAuthorise(
          retrievals = Retrievals.credentialRole and Retrievals.allEnrolments
        )(Future.successful(new ~(Some(Admin), Enrolments(Set()))))
        mockVatNumberEligibilitySuccess(testInvalidVatNumber)

        implicit val request = testPostRequest(testInvalidVatNumber)

        val result = TestCaptureVatNumberController.submit(request)
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.InvalidVatNumberController.show().url)
      }
    }
  }

  "form unsuccessfully submitted" should {
    "reload the page with errors" in {
      mockAuthorise(
        retrievals = Retrievals.credentialRole and Retrievals.allEnrolments
      )(Future.successful(new ~(Some(Admin), Enrolments(Set()))))

      val result = TestCaptureVatNumberController.submit(testPostRequest("invalid"))
      status(result) shouldBe Status.BAD_REQUEST
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
    }
  }
}
