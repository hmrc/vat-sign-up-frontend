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

package uk.gov.hmrc.vatsignupfrontend.controllers.principal.eligibility

import play.api.i18n.Messages
import play.api.mvc.{AnyContentAsEmpty, AnyContentAsFormUrlEncoded}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockVatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.controllers.principal.error.{routes => errorRoutes}
import uk.gov.hmrc.vatsignupfrontend.forms.eligibility.AreYouReadySubmitSoftwareForm._
import uk.gov.hmrc.vatsignupfrontend.forms.submapping.YesNoMapping._
import uk.gov.hmrc.vatsignupfrontend.utils.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.eligibility.are_you_ready_submit_software
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.eligibility.are_you_ready_submit_software

class AreYouReadySubmitSoftwareControllerSpec extends UnitSpec with MockVatControllerComponents {

  val view = app.injector.instanceOf[are_you_ready_submit_software]
  object TestAreYouReadySubmitSoftwareController extends AreYouReadySubmitSoftwareController(view)

  "The show method" should {
    "render the are_you_ready_submit_software view" in {
      implicit val testGetRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/are-you-ready-to-submit")
      implicit lazy val messages: Messages = mockVatControllerComponents.controllerComponents.messagesApi.preferred(testGetRequest)

      val result = TestAreYouReadySubmitSoftwareController.show(testGetRequest)

      status(result) shouldBe OK
      contentAsString(result) shouldBe view(areYouReadySubmitSoftwareForm, routes.AreYouReadySubmitSoftwareController.submit()).body
    }
  }

  "The submit method" should {
    def testPostRequest(entityTypeVal: String): FakeRequest[AnyContentAsFormUrlEncoded] =
      FakeRequest("POST", "/are-you-ready-to-submit").withFormUrlEncodedBody(yesNo -> entityTypeVal)

    "reload the view with errors" when {
      "user does not click a radio button" in {
        implicit val request: FakeRequest[AnyContentAsFormUrlEncoded] = testPostRequest(entityTypeVal = "")
        implicit lazy val messages: Messages = mockVatControllerComponents.controllerComponents.messagesApi.preferred(request)

        val result = TestAreYouReadySubmitSoftwareController.submit(request)

        status(result) shouldBe BAD_REQUEST
        contentAsString(result) shouldBe view(
          areYouReadySubmitSoftwareForm.bindFromRequest(),
          routes.AreYouReadySubmitSoftwareController.submit()
        ).body

      }
    }

    "redirect to Making Tax Digital Software Page" when {
      "user selects the Yes button" in {
        val result = TestAreYouReadySubmitSoftwareController.submit(testPostRequest(entityTypeVal = option_yes))

        status(result) shouldBe SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.MakingTaxDigitalSoftwareController.show().url)
      }
    }

    "redirect to Return Due Page" when {
      "user selects No button" in {
        val result = TestAreYouReadySubmitSoftwareController.submit(testPostRequest(entityTypeVal = option_no))

        status(result) shouldBe SEE_OTHER
        redirectLocation(result) shouldBe Some(errorRoutes.ReturnDueController.show().url)
      }
    }
  }

}
