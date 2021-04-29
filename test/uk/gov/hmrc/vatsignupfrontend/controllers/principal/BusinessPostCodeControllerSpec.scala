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
import play.api.mvc.{AnyContentAsEmpty, AnyContentAsFormUrlEncoded}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockVatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.forms.BusinessPostCodeForm._
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.models.DateModel
import uk.gov.hmrc.vatsignupfrontend.services.mocks.MockClaimSubscriptionService
import uk.gov.hmrc.vatsignupfrontend.utils.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.principal_place_of_business
import scala.concurrent.ExecutionContext.Implicits.global

class BusinessPostCodeControllerSpec extends UnitSpec with GuiceOneAppPerSuite with MockVatControllerComponents with MockClaimSubscriptionService {

  lazy val view = app.injector.instanceOf[principal_place_of_business]

  object TestBusinessPostCodeController extends BusinessPostCodeController(view)

  lazy val testGetRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/business-postcode")

  def testPostRequest(businessPostCodeVal: String): FakeRequest[AnyContentAsFormUrlEncoded] =
    FakeRequest("POST", "/business-postcode").withFormUrlEncodedBody(businessPostCode -> businessPostCodeVal)

  val testDate: DateModel = DateModel.dateConvert(LocalDate.now())

  "Calling the show action of the Business PostCode controller" when {
    "go to the Business PostCode page" in {
      mockAuthAdminRole()

      val result = TestBusinessPostCodeController.show(testGetRequest)
      status(result) shouldBe Status.OK
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
    }
  }

  "Calling the submit action of the Business PostCode controller" when {
    "the session contains isAlreadySubscribed: true" should {
      "redirect to check your answers" in {
        mockAuthAdminRole()

        implicit val request: FakeRequest[AnyContentAsFormUrlEncoded] =
          testPostRequest(testBusinessPostcode.postCode).withSession(SessionKeys.isAlreadySubscribedKey -> "true")
        val result = TestBusinessPostCodeController.submit(request)
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.CheckYourAnswersController.show().url)

        session(result).get(SessionKeys.businessPostCodeKey) should contain(
          Json.toJson(testBusinessPostcode.copy(testBusinessPostcode.postCode.toUpperCase.replaceAll(" ", ""))).toString
        )
      }
    }

    "the session contains isMigrated: true" should {
      "redirect to check your answers" in {
        mockAuthAdminRole()

        implicit val request: FakeRequest[AnyContentAsFormUrlEncoded] =
          testPostRequest(testBusinessPostcode.postCode).withSession(SessionKeys.isMigratedKey -> "true")
        val result = TestBusinessPostCodeController.submit(request)
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.CheckYourAnswersController.show().url)

        session(result).get(SessionKeys.businessPostCodeKey) should contain(
          Json.toJson(testBusinessPostcode.copy(testBusinessPostcode.postCode.toUpperCase.replaceAll(" ", ""))).toString
        )
      }
    }

    "the form is successfully submitted" should {
      "redirect to the previous vat return page" in {
        mockAuthAdminRole()

        implicit val request: FakeRequest[AnyContentAsFormUrlEncoded] = testPostRequest(testBusinessPostcode.postCode)
        val result = TestBusinessPostCodeController.submit(request)
        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.PreviousVatReturnController.show().url)

        session(result).get(SessionKeys.businessPostCodeKey) should contain(
          Json.toJson(testBusinessPostcode.copy(testBusinessPostcode.postCode.toUpperCase.replaceAll(" ", ""))).toString
        )
      }
    }
  }


  "form unsuccessfully submitted" should {
    "reload the page with errors" in {
      mockAuthAdminRole()

      val result = TestBusinessPostCodeController.submit(testPostRequest(""))
      status(result) shouldBe Status.BAD_REQUEST
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
    }
  }
}