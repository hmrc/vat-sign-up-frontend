/*
 * Copyright 2020 HM Revenue & Customs
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
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.http.InternalServerException
import uk.gov.hmrc.vatsignupfrontend.SessionKeys.{businessEntityKey, isAlreadySubscribedKey, isFromBtaKey, vatNumberKey}
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockVatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.controllers.principal.error.{routes => errorRoutes}
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstantsGenerator
import uk.gov.hmrc.vatsignupfrontend.httpparsers.ClaimSubscriptionHttpParser.{AlreadyEnrolledOnDifferentCredential, InvalidVatNumber, SubscriptionClaimed}
import uk.gov.hmrc.vatsignupfrontend.models.Overseas
import uk.gov.hmrc.vatsignupfrontend.services.mocks.{MockCheckVatNumberEligibilityService, MockClaimSubscriptionService}
import uk.gov.hmrc.vatsignupfrontend.utils.UnitSpec

import scala.concurrent.Future

class ClaimSubscriptionControllerSpec extends UnitSpec with GuiceOneAppPerSuite
  with MockVatControllerComponents with MockClaimSubscriptionService with MockCheckVatNumberEligibilityService {

  object TestClaimSubscriptionController extends ClaimSubscriptionController(
    mockClaimSubscriptionService,
    mockCheckVatNumberEligibilityService
  )

  lazy val testGetRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/claim-subscription")

  "show" when {
    "the BTA claim subscription feature switch is enabled" when {
      "the user has an MTD VRN" should {
        "redirect to AlreadySignedUp" in {

          mockAuthRetrieveMtdVatEnrolment()

          val result = TestClaimSubscriptionController.show(testVatNumber)(testGetRequest)

          status(result) shouldBe SEE_OTHER
          redirectLocation(result) should contain(errorRoutes.AlreadySignedUpController.show().url)
        }
      }

      "the user has a VATDEC enrolment" when {
        "the VAT number on the enrolment matches the one provided in the URL" when {
          "claim subscription service returns SubscriptionClaimed" should {
            "redirect to SignUpCompleteClientController" in {

              mockAuthRetrieveVatDecEnrolment()
              mockClaimSubscription(testVatNumber, isFromBta = true)(Future.successful(Right(SubscriptionClaimed)))

              val result = TestClaimSubscriptionController.show(testVatNumber)(testGetRequest)

              status(result) shouldBe SEE_OTHER
              redirectLocation(result) should contain(mockAppConfig.btaRedirectUrl)
            }
          }

          "claim subscription service returns VatNumberAlreadyEnrolled" should {
            "redirect to Business Already Signed Up error page" in {

              mockAuthRetrieveVatDecEnrolment()
              mockClaimSubscription(testVatNumber, isFromBta = true)(Future.successful(Left(AlreadyEnrolledOnDifferentCredential)))

              val result = TestClaimSubscriptionController.show(testVatNumber)(testGetRequest)

              status(result) shouldBe SEE_OTHER
              redirectLocation(result) should contain(errorRoutes.BusinessAlreadySignedUpController.show().url)
            }
          }

          "claim subscription service returns anything else" should {
            "throw an Internal Server Exception" in {

              mockAuthRetrieveVatDecEnrolment()
              mockClaimSubscription(testVatNumber, isFromBta = true)(Future.successful(Left(InvalidVatNumber("error message"))))

              val result = TestClaimSubscriptionController.show(testVatNumber)(testGetRequest)

              intercept[InternalServerException](result)
            }
          }
        }

        "the VAT number on the enrolment does not match the one provided in the URL" should {
          "throw an Internal Server Exception" in {

            mockAuthRetrieveVatDecEnrolment()

            val nonMatchingVatNumber = TestConstantsGenerator.randomVatNumber

            val result = TestClaimSubscriptionController.show(nonMatchingVatNumber)(testGetRequest)

            intercept[InternalServerException](result)
          }
        }
      }

      "the user does not have a VATDEC enrolment" when {
        "the VAT number is valid and non-overseas" should {
          "redirect to BTA Capture VAT registration date" in {
            mockAuthRetrieveEmptyEnrolment()
            mockIsOverseas(testVatNumber)(Future(false))

            val result = TestClaimSubscriptionController.show(testVatNumber)(testGetRequest)

            status(result) shouldBe SEE_OTHER
            redirectLocation(result) should contain(routes.CaptureVatRegistrationDateController.show().url)

            session(result).get(vatNumberKey) should contain(testVatNumber)
            session(result).get(isAlreadySubscribedKey) should contain("true")
            session(result).get(businessEntityKey) shouldNot contain(Overseas.toString)
            session(result).get(isFromBtaKey) should contain("true")
          }
        }

        "the VAT number is valid and overseas" should {
          "redirect to BTA Capture VAT registration date" in {
            mockAuthRetrieveEmptyEnrolment()
            mockIsOverseas(testVatNumber)(Future(true))

            val result = TestClaimSubscriptionController.show(testVatNumber)(testGetRequest)

            status(result) shouldBe SEE_OTHER
            redirectLocation(result) should contain(routes.CaptureVatRegistrationDateController.show().url)

            session(result).get(vatNumberKey) should contain(testVatNumber)
            session(result).get(isAlreadySubscribedKey) should contain("true")
            session(result).get(businessEntityKey) should contain(Overseas.toString)
            session(result).get(isFromBtaKey) should contain("true")
          }
        }

        "the VAT number is invalid" should {
          "throw an Internal Server Exception" in {
            mockAuthRetrieveEmptyEnrolment()

            val invalidVatNumber = "1"

            val result = TestClaimSubscriptionController.show(invalidVatNumber)(testGetRequest)

            intercept[InternalServerException](result)
          }
        }
      }
    }
  }
}
