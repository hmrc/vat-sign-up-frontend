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

import play.api.http.Status
import play.api.mvc.{AnyContentAsEmpty, AnyContentAsFormUrlEncoded}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockVatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.controllers.principal.error.{routes => errorRoutes}
import uk.gov.hmrc.vatsignupfrontend.forms.HaveSoftwareForm._
import uk.gov.hmrc.vatsignupfrontend.utils.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.have_software

class HaveSoftwareControllerSpec extends UnitSpec with MockVatControllerComponents {

  val view = app.injector.instanceOf[have_software]

  object TestHaveSoftwareController extends HaveSoftwareController(view)

  val testGetRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/have-software")

  def testPostRequest(entityTypeVal: String): FakeRequest[AnyContentAsFormUrlEncoded] =
    FakeRequest("POST", "/have-software").withFormUrlEncodedBody(yesNo -> entityTypeVal)

  "Calling the show action of the Have Software controller" should {
    "go to the Have Software page" in {
      val result = TestHaveSoftwareController.show(testGetRequest)

      status(result) shouldBe Status.OK
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
    }
  }

  "Calling the submit action of the Have Software controller" when {
    "form successfully submitted" should {
      "the choice is YES" should {
        "go to Software Ready page" in {
          val result = TestHaveSoftwareController.submit(testPostRequest(entityTypeVal = "yes"))
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.SoftwareReadyController.show().url)
        }
      }

      "the choice is NO" should {
        "go to Choose Software error page" in {
          val result = TestHaveSoftwareController.submit(testPostRequest(entityTypeVal = "no"))
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(errorRoutes.ChooseSoftwareErrorController.show().url)
        }
      }
    }

    "form unsuccessfully submitted" should {
      "reload the page with errors" in {
        val result = TestHaveSoftwareController.submit(testPostRequest("invalid"))
        status(result) shouldBe Status.BAD_REQUEST
        contentType(result) shouldBe Some("text/html")
        charset(result) shouldBe Some("utf-8")
      }
    }
  }

}
