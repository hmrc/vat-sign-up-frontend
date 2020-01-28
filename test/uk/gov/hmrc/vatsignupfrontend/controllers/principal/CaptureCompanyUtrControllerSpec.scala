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
import play.api.http.Status
import play.api.mvc.{AnyContentAsEmpty, AnyContentAsFormUrlEncoded}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockVatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.forms.CompanyUtrForm._
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.utils.UnitSpec

class CaptureCompanyUtrControllerSpec extends UnitSpec with GuiceOneAppPerSuite with MockVatControllerComponents {

  object TestCaptureCompanyUtrController extends CaptureCompanyUtrController

  lazy val testGetRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/company-utr")

  def testPostRequest(companyUtrVal: String): FakeRequest[AnyContentAsFormUrlEncoded] =
    FakeRequest("POST", "/company-utr").withFormUrlEncodedBody(companyUtr -> companyUtrVal)

  "Calling the show action of the Capture Company Utr controller" should {
    "go to the Capture Company Utr page" in {
      mockAuthAdminRole()

      val result = TestCaptureCompanyUtrController.show(testGetRequest)
      status(result) shouldBe Status.OK
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
    }
  }

  "Calling the submit action of the Capture Company Utr controller" should {
    "go to Check Your Answers Company page" in {
      mockAuthAdminRole()

      val request = testPostRequest(testCompanyUtr)

      val result = TestCaptureCompanyUtrController.submit(request)

      status(result) shouldBe Status.SEE_OTHER
      redirectLocation(result) shouldBe Some(routes.CheckYourAnswersCompanyController.show().url)

      session(result).get(SessionKeys.companyUtrKey) shouldBe Some(testCompanyUtr)
    }
  }

  "form unsuccessfully submitted" should {
    "reload the page with errors" in {
      mockAuthAdminRole()

      val result = TestCaptureCompanyUtrController.submit(testPostRequest("invalid"))
      status(result) shouldBe Status.BAD_REQUEST
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
    }
  }

}
