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

package uk.gov.hmrc.vatsignupfrontend.controllers.principal

import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.play.test.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockControllerComponents

class ChooseSoftwareErrorControllerSpec extends UnitSpec with GuiceOneAppPerSuite with MockControllerComponents {

  object TestChooseSoftwareErrorController extends ChooseSoftwareErrorController(mockControllerComponents)

  lazy val testGetRequest = FakeRequest("GET", "/choose-software")

  lazy val testPostRequest: FakeRequest[AnyContentAsEmpty.type] =
    FakeRequest("POST", "/choose-software")

  "Calling the show action of the ChooseSoftwareError controller" should {
    "show the ChooseSoftwareError page" in {
      mockAuthAdminRole()
      val request = testGetRequest

      val result = TestChooseSoftwareErrorController.show(request)
      status(result) shouldBe Status.OK
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
    }
  }

  "Calling the submit action of the ChooseSoftwareError controller" should {
    "go to the gov.uk guidance page" in {
      mockAuthAdminRole()

      val result = TestChooseSoftwareErrorController.submit(testPostRequest)
      status(result) shouldBe Status.SEE_OTHER
      redirectLocation(result) shouldBe Some(mockAppConfig.guidancePageUrl)
    }
  }

}
