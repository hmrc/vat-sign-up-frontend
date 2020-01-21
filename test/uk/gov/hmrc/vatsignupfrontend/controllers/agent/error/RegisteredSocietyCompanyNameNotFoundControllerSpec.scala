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

package uk.gov.hmrc.vatsignupfrontend.controllers.agent.error

import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.play.test.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockControllerComponents
import uk.gov.hmrc.vatsignupfrontend.controllers.agent.{routes => agentRoutes}

class RegisteredSocietyCompanyNameNotFoundControllerSpec extends UnitSpec with GuiceOneAppPerSuite with MockControllerComponents {

  object TestRegisteredSocietyCompanyNameNotFoundController extends RegisteredSocietyCompanyNameNotFoundController(mockControllerComponents)

  lazy val testGetRequest = FakeRequest("GET", "/client/error/registered-society-company-name-not-found")

  lazy val testPostRequest: FakeRequest[AnyContentAsEmpty.type] =
    FakeRequest("POST", "/client/error/registered-society-company-name-not-found")

  "Calling the show action of the Registered Society Company Name not found controller" should {
    "show the registered society company name not found page" in {
      mockAuthRetrieveAgentEnrolment()
      val request = testGetRequest

      val result = TestRegisteredSocietyCompanyNameNotFoundController.show(request)
      status(result) shouldBe Status.OK
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
    }
  }

  "Calling the submit action of the Company Name not found controller" should {
    "redirect to capture your registered society company number page" in {
      mockAuthRetrieveAgentEnrolment()

      val result = TestRegisteredSocietyCompanyNameNotFoundController.submit(testPostRequest)
      status(result) shouldBe Status.SEE_OTHER
      redirectLocation(result) should contain(agentRoutes.CaptureRegisteredSocietyCompanyNumberController.show().url)
    }
  }

}
