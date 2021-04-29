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

package uk.gov.hmrc.vatsignupfrontend.controllers.principal.error

import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.i18n.Messages
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.vatsignupfrontend.utils.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockVatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.controllers.principal.{routes => principalRoutes}
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants.testVatNumber
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.{deregistered_enrolled, deregistered_unenrolled}
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.deregistered_enrolled

class DeregisteredVatNumberControllerSpec extends UnitSpec with GuiceOneAppPerSuite with MockVatControllerComponents {

  val enrolledView = app.injector.instanceOf[deregistered_enrolled]
  val unenrolledView = app.injector.instanceOf[deregistered_unenrolled]

  object TestDeregisteredVatNumberController extends DeregisteredVatNumberController(enrolledView, unenrolledView)

  lazy val testGetRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/error/deregistered-vat-number")

  "Calling the show action of DeregisteredVatNumberController" when {
    "an enrolled user enters a deregistered VRN" should {
      "show the enrolled deregistered page" in {
        mockAuthRetrieveMtdVatEnrolment()

        implicit val request: FakeRequest[AnyContentAsEmpty.type] = testGetRequest
        implicit val messages: Messages = mockVatControllerComponents.controllerComponents.messagesApi.preferred(request)
        val result = TestDeregisteredVatNumberController.show(request)

        status(result) shouldBe OK
        contentType(result) shouldBe Some("text/html")
        charset(result) shouldBe Some("utf-8")
        contentAsString(result) shouldBe enrolledView(testVatNumber).body
      }
    }

    "an unenrolled user enters a deregistered VRN" should {
      "redirect the user to the unenrolled deregistered page" in {
        mockAuthRetrieveEmptyEnrolment()

        implicit val request: FakeRequest[AnyContentAsEmpty.type] = testGetRequest
        implicit val messages: Messages = mockVatControllerComponents.controllerComponents.messagesApi.preferred(request)
        val result = TestDeregisteredVatNumberController.show(request)

        status(result) shouldBe OK
        contentType(result) shouldBe Some("text/html")
        charset(result) shouldBe Some("utf-8")
        contentAsString(result) shouldBe unenrolledView(principalRoutes.CaptureVatNumberController.submit().url).body
      }
    }
  }
}
