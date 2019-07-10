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

package uk.gov.hmrc.vatsignupfrontend.controllers.principal.partnerships

import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.mvc.AnyContentAsFormUrlEncoded
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthorisationException
import uk.gov.hmrc.play.test.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockControllerComponents
import uk.gov.hmrc.vatsignupfrontend.forms.PartnershipUtrForm._
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.models.Yes

class CapturePartnershipUtrControllerSpec extends UnitSpec with GuiceOneAppPerSuite with MockControllerComponents {

  object TestCapturePartnershipUtrController extends CapturePartnershipUtrController(mockControllerComponents)

  lazy val testGetRequestForNoUtr = FakeRequest("GET", "/partnership-no-utr")
  lazy val testGetRequestForShow = FakeRequest("GET", "/partnership-utr")

  def testPostRequest(utr: String): FakeRequest[AnyContentAsFormUrlEncoded] =
    FakeRequest("POST", "/partnership-utr").withFormUrlEncodedBody(partnershipUtr -> utr)

  "Calling the show action of the CapturePartnershipUtrController" when {
    "go to the Partnership utr page" in {
        mockAuthAdminRole()

      val result = TestCapturePartnershipUtrController.show(testGetRequestForShow)
      status(result) shouldBe Status.OK
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
    }
  }

  "Calling the noUtrSelected action of the CapturePartnershipUtrController" should {
    s"redirect to check your answers page & drop ${SessionKeys.partnershipSautrKey}${SessionKeys.partnershipPostCodeKey} & does not drop any other keys & set ${SessionKeys.hasOptionalSautrKey} = false" in {
      mockAuthAdminRole()

      val result = TestCapturePartnershipUtrController.noUtrSelected(testGetRequestForNoUtr.withSession(
          SessionKeys.partnershipSautrKey -> testSaUtr,
          SessionKeys.partnershipPostCodeKey -> testBusinessPostcode.sanitisedPostCode,
          SessionKeys.previousVatReturnKey -> Yes.stringValue)
      )

      status(result) shouldBe Status.SEE_OTHER
      redirectLocation(result).get shouldBe routes.CheckYourAnswersPartnershipsController.show().url
      session(result) get SessionKeys.partnershipSautrKey shouldBe None
      session(result) get SessionKeys.partnershipPostCodeKey shouldBe None
      session(result) get SessionKeys.previousVatReturnKey should contain(Yes.stringValue)
      session(result) get SessionKeys.hasOptionalSautrKey should contain(false.toString)
    }
    "have an auth check and return exception if not authorised" in {
      mockFailedAuth()

       intercept[AuthorisationException](await(TestCapturePartnershipUtrController.noUtrSelected(testGetRequestForNoUtr)))
    }
  }

  "Calling the submit action of the CapturePartnershipUtrController" when {
    "form successfully submitted" should {
      "redirect to PPOB" in {
        mockAuthAdminRole()

        implicit val request = testPostRequest(testSaUtr)
        val result = TestCapturePartnershipUtrController.submit(request)

        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.PrincipalPlacePostCodeController.show().url)
      }
    }

    "form unsuccessfully submitted" should {
      "reload the page with errors" in {
        mockAuthAdminRole()

        val result = TestCapturePartnershipUtrController.submit(testPostRequest(""))

        status(result) shouldBe Status.BAD_REQUEST
        contentType(result) shouldBe Some("text/html")
        charset(result) shouldBe Some("utf-8")
      }
    }
  }
}
