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
import play.api.mvc.{AnyContentAsEmpty, AnyContentAsFormUrlEncoded}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockVatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.forms.RegisteredSocietyUtrForm._
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.utils.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.capture_registered_society_utr

class CaptureRegisteredSocietyUtrControllerSpec extends UnitSpec with GuiceOneAppPerSuite with MockVatControllerComponents {

  val view = app.injector.instanceOf[capture_registered_society_utr]

  object TestCaptureRegisteredSocietyUtrController extends CaptureRegisteredSocietyUtrController(view)

  lazy val testGetRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/registered-society-utr")

  def testPostRequest(companyUtrVal: String): FakeRequest[AnyContentAsFormUrlEncoded] =
    FakeRequest("POST", "/registered-society-utr").withFormUrlEncodedBody(registeredSocietyUtr -> companyUtrVal)

  "Calling the show action of the Capture Registered Society Utr controller" should {
    "go to the Capture Registered Society Utr page" in {
      mockAuthAdminRole()

      val result = TestCaptureRegisteredSocietyUtrController.show(testGetRequest)
      status(result) shouldBe Status.OK
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
    }
  }

  "Calling the submit action of the Capture Registered Society Utr controller" should {
    "goto check your answers page" in {
      mockAuthAdminRole()

      val request = testPostRequest(testCompanyUtr)

      val result = TestCaptureRegisteredSocietyUtrController.submit(request)

      status(result) shouldBe Status.SEE_OTHER
      redirectLocation(result) shouldBe Some(routes.RegisteredSocietyCheckYourAnswersController.show().url)
      session(result).get(SessionKeys.registeredSocietyUtrKey) shouldBe Some(testCompanyUtr)
    }
  }

  "form unsuccessfully submitted" should {
    "reload the page with errors" in {
      mockAuthAdminRole()

      val result = TestCaptureRegisteredSocietyUtrController.submit(testPostRequest("invalid"))
      status(result) shouldBe Status.BAD_REQUEST
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
    }
  }

}
