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

import org.jsoup.Jsoup
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.http.InternalServerException
import uk.gov.hmrc.vatsignupfrontend.utils.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch.FeatureSwitching
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockVatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.services.mocks.{MockCtReferenceLookupService, MockStoreRegisteredSocietyService}
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.confirm_registered_society

class ConfirmRegisteredSocietyControllerSpec extends UnitSpec with GuiceOneAppPerSuite with MockVatControllerComponents
  with MockStoreRegisteredSocietyService with MockCtReferenceLookupService with FeatureSwitching {

  val view = app.injector.instanceOf[confirm_registered_society]

  object TestConfirmRegisteredSocietyController extends ConfirmRegisteredSocietyController(
    mockStoreRegisteredSocietyService,
    mockCtReferenceLookupService,
    view
  )

  val testGetRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/confirm-registered-society")

  val testPostRequest: FakeRequest[AnyContentAsEmpty.type] =
    FakeRequest("POST", "/confirm-registered-society")

  "Calling the show action of the Confirm Registered Society controller" when {
    "there is a registered society name in the session" should {
      "go to the Confirm Registered Society page" in {
        mockAuthAdminRole()
        val request = testGetRequest.withSession(
          SessionKeys.registeredSocietyNameKey -> testCompanyName
        )

        val result = TestConfirmRegisteredSocietyController.show(request)
        status(result) shouldBe Status.OK
        contentType(result) shouldBe Some("text/html")
        charset(result) shouldBe Some("utf-8")

        val changeLink = Jsoup.parse(contentAsString(result)).getElementById("changeLink")
        changeLink.attr("href") shouldBe routes.CaptureRegisteredSocietyCompanyNumberController.show().url
      }
    }

    "there isn't a registered society name in the session" should {
      "go to the capture registered society company number page" in {
        mockAuthAdminRole()

        val result = TestConfirmRegisteredSocietyController.show(testGetRequest)
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.CaptureRegisteredSocietyCompanyNumberController.show().url)
      }
    }
  }

  "Calling the submit action of the Confirm Registered Society controller" when {
    "the user has a ct enrolment and the ctutr matches the ctutr returned from DES" should {
      "redirect to agree email page" in {
        mockAuthRetrieveIRCTEnrolment()
        mockCtReferenceFound(testCompanyNumber)
        mockStoreRegisteredSocietySuccess(
          vatNumber = testVatNumber,
          companyNumber = testCompanyNumber,
          companyUtr = Some(testSaUtr)
        )

        val request = testPostRequest.withSession(
          SessionKeys.vatNumberKey -> testVatNumber,
          SessionKeys.registeredSocietyCompanyNumberKey -> testCompanyNumber
        )

        val result = TestConfirmRegisteredSocietyController.submit(request)
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.DirectDebitResolverController.show().url)
      }
    }
    "the ct enrolment ctutr does not match the ctutr returned from DES" should {
      "redirect to the capture ct utr page" in {
        mockAuthRetrieveIRCTEnrolment()
        mockCtReferenceFound(testCompanyNumber)
        mockStoreRegisteredSocietyCtMismatch(
          vatNumber = testVatNumber,
          companyNumber = testCompanyNumber,
          companyUtr = Some(testSaUtr)
        )

        val request = testPostRequest.withSession(
          SessionKeys.vatNumberKey -> testVatNumber,
          SessionKeys.registeredSocietyCompanyNumberKey -> testCompanyNumber
        )

        val result = TestConfirmRegisteredSocietyController.submit(request)
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.CaptureRegisteredSocietyUtrController.show().url)

      }
    }
  }
  "the user has a ct enrolment but the company is not found on des" should {
    "store the registered society with no attached ctutr and redirect to agree email page" in {
      mockAuthRetrieveIRCTEnrolment()
      mockCtReferenceNotFound(testCompanyNumber)
      mockStoreRegisteredSocietySuccess(
        vatNumber = testVatNumber,
        companyNumber = testCompanyNumber,
        companyUtr = None
      )

      val request = testPostRequest.withSession(
        SessionKeys.vatNumberKey -> testVatNumber,
        SessionKeys.registeredSocietyCompanyNumberKey -> testCompanyNumber
      )

      val result = TestConfirmRegisteredSocietyController.submit(request)
      status(result) shouldBe Status.SEE_OTHER
      redirectLocation(result) shouldBe Some(routes.DirectDebitResolverController.show().url)
    }
  }
  "the CtReferenceLookup returns that a ctutr exists for the given crn" should {
    "go to the 'Capture Registered Society UTR' page" in {
      mockAuthRetrieveEmptyEnrolment()
      mockCtReferenceFound(testCompanyNumber)

      val request = testPostRequest.withSession(
        SessionKeys.vatNumberKey -> testVatNumber,
        SessionKeys.registeredSocietyCompanyNumberKey -> testCompanyNumber
      )

      val result = TestConfirmRegisteredSocietyController.submit(request)
      status(result) shouldBe Status.SEE_OTHER
      redirectLocation(result) shouldBe Some(routes.CaptureRegisteredSocietyUtrController.show().url)
    }

    "throw internal server exception if ct reference lookup fails" in {
      mockAuthRetrieveEmptyEnrolment()
      mockCtReferenceFailure(testCompanyNumber)(BAD_REQUEST)

      val request = testPostRequest.withSession(
        SessionKeys.vatNumberKey -> testVatNumber,
        SessionKeys.registeredSocietyCompanyNumberKey -> testCompanyNumber
      )

      intercept[InternalServerException] {
        TestConfirmRegisteredSocietyController.submit(request)
      }
    }
    "go to the 'your vat number' page if vat number is missing" in {
      mockAuthRetrieveEmptyEnrolment()
      mockCtReferenceFound(testCompanyNumber)

      val request = testPostRequest.withSession(
        SessionKeys.registeredSocietyCompanyNumberKey -> testCompanyNumber
      )

      val result = TestConfirmRegisteredSocietyController.submit(request)
      status(result) shouldBe Status.SEE_OTHER
      redirectLocation(result) shouldBe Some(routes.ResolveVatNumberController.resolve().url)

    }
  }
  "the CtReferenceLookup returns that a ctutr does not exist for the given crn" should {
    "go to the 'agree to receive emails' page" in {
      mockAuthRetrieveEmptyEnrolment()
      mockCtReferenceNotFound(testCompanyNumber)
      mockStoreRegisteredSocietySuccess(
        vatNumber = testVatNumber,
        companyNumber = testCompanyNumber,
        companyUtr = None
      )

      val request = testPostRequest.withSession(
        SessionKeys.vatNumberKey -> testVatNumber,
        SessionKeys.registeredSocietyCompanyNumberKey -> testCompanyNumber
      )

      val result = TestConfirmRegisteredSocietyController.submit(request)
      status(result) shouldBe Status.SEE_OTHER
      redirectLocation(result) shouldBe Some(routes.DirectDebitResolverController.show().url)
    }

    "throw internal server exception if store registered society fails" in {
      mockAuthRetrieveEmptyEnrolment()
      mockCtReferenceNotFound(testCompanyNumber)
      mockStoreRegisteredSocietyFailure(testVatNumber, testCompanyNumber, None)

      val request = testPostRequest.withSession(
        SessionKeys.vatNumberKey -> testVatNumber,
        SessionKeys.registeredSocietyCompanyNumberKey -> testCompanyNumber
      )

      intercept[InternalServerException] {
        TestConfirmRegisteredSocietyController.submit(request)
      }
    }
    "go to the 'your vat number' page if vat number is missing" in {
      mockAuthRetrieveEmptyEnrolment()
      mockCtReferenceNotFound(testCompanyNumber)

      val request = testPostRequest.withSession(
        SessionKeys.registeredSocietyCompanyNumberKey -> testCompanyNumber
      )

      val result = TestConfirmRegisteredSocietyController.submit(request)
      status(result) shouldBe Status.SEE_OTHER
      redirectLocation(result) shouldBe Some(routes.ResolveVatNumberController.resolve().url)

    }
  }
  "go to the 'capture registered society company number' page if company number is missing" in {
    mockAuthRetrieveEmptyEnrolment()

    val request = testPostRequest.withSession(
      SessionKeys.vatNumberKey -> testVatNumber
    )

    val result = TestConfirmRegisteredSocietyController.submit(request)
    status(result) shouldBe Status.SEE_OTHER
    redirectLocation(result) shouldBe Some(routes.CaptureRegisteredSocietyCompanyNumberController.show().url)
  }
}

