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

package uk.gov.hmrc.vatsignupfrontend.controllers.principal.partnerships

import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.libs.json.Json
import play.api.mvc.{AnyContentAsEmpty, AnyContentAsFormUrlEncoded}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.vatsignupfrontend.utils.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockVatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.forms.PartnershipPostCodeForm._
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.models.PostCode
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.partnerships.principal_place_post_code

class PrincipalPlacePostCodeControllerSpec extends UnitSpec with GuiceOneAppPerSuite with MockVatControllerComponents {

  val view = app.injector.instanceOf[principal_place_post_code]

  object TestPrincipalPlacePostCodeController extends PrincipalPlacePostCodeController(view)

  lazy val testGetRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/principal-place-postcode")

  def testPostRequest(postCode: String): FakeRequest[AnyContentAsFormUrlEncoded] =
    FakeRequest("POST", "/principal-place-postcode").withFormUrlEncodedBody(partnershipPostCode -> postCode)

  "Calling the show action of the Principal Place PostCode controller" when {
    "go to the Principal Place PostCode page" in {
      mockAuthAdminRole()

      val result = TestPrincipalPlacePostCodeController.show(testGetRequest)
      status(result) shouldBe Status.OK
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
    }
  }

  "Calling the submit action of the Principal Place PostCode controller" when {
    "form successfully submitted" should {
      "redirect to partnership CYA page" in {
        mockAuthAdminRole()

        implicit val request: FakeRequest[AnyContentAsFormUrlEncoded] = testPostRequest(testBusinessPostcode.postCode)
        val result = TestPrincipalPlacePostCodeController.submit(request)
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.CheckYourAnswersPartnershipsController.show().url)

        val expectedSessionPostCode = PostCode(testBusinessPostcode.postCode.toUpperCase.replaceAll(" ",""))
        session(result) get SessionKeys.partnershipPostCodeKey should contain(Json.toJson(expectedSessionPostCode).toString())
      }
    }

    "form unsuccessfully submitted" should {
      "reload the page with errors" in {
        mockAuthAdminRole()

        val result = TestPrincipalPlacePostCodeController.submit(testPostRequest(""))
        status(result) shouldBe Status.BAD_REQUEST
        contentType(result) shouldBe Some("text/html")
        charset(result) shouldBe Some("utf-8")
      }
    }
  }

}
