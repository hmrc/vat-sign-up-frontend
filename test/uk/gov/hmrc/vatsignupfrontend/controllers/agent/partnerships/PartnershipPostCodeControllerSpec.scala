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

package uk.gov.hmrc.vatsignupfrontend.controllers.agent.partnerships

import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.mvc.AnyContentAsFormUrlEncoded
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.vatsignupfrontend.utils.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockVatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.forms.PartnershipPostCodeForm._
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.views.html.agent.partnerships.partnership_ppob

class PartnershipPostCodeControllerSpec extends UnitSpec with GuiceOneAppPerSuite with MockVatControllerComponents {

  val view = app.injector.instanceOf[partnership_ppob]

  object TestPartnershipPostCodeController extends PartnershipPostCodeController(view)

  lazy val testGetRequest = FakeRequest("GET", "/principal-place-postcode")

  def testPostRequest(partnershipPostCodeVal: String): FakeRequest[AnyContentAsFormUrlEncoded] =
    FakeRequest("POST", "/principal-place-postcode").withFormUrlEncodedBody(partnershipPostCode -> partnershipPostCodeVal)

  "Calling the show action of the Partnership PostCode controller" when {
    "go to the Partnership PostCode page" in {
      mockAuthRetrieveAgentEnrolment()

      val result = TestPartnershipPostCodeController.show(testGetRequest)
      status(result) shouldBe Status.OK
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
    }
  }

  "Calling the submit action of the Partnership PostCode controller" when {
    "form successfully submitted" should {
      "Not implemented" in {
        mockAuthRetrieveAgentEnrolment()

        implicit val request = testPostRequest(testBusinessPostcode.postCode)
        val result = TestPartnershipPostCodeController.submit(request)
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) should contain(routes.CheckYourAnswersPartnershipController.show().url)
      }
    }

    "form unsuccessfully submitted" should {
      "reload the page with errors" in {
        mockAuthRetrieveAgentEnrolment()

        val result = TestPartnershipPostCodeController.submit(testPostRequest(""))
        status(result) shouldBe Status.BAD_REQUEST
        contentType(result) shouldBe Some("text/html")
        charset(result) shouldBe Some("utf-8")
      }
    }
  }

}
