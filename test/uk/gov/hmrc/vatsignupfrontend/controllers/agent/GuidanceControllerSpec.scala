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

package uk.gov.hmrc.vatsignupfrontend.controllers.agent

import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.test.FakeRequest
import uk.gov.hmrc.play.test.UnitSpec
import play.api.test.Helpers._
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockControllerComponents

class GuidanceControllerSpec extends UnitSpec with GuiceOneAppPerSuite with MockControllerComponents {

  object TestGuidanceController extends GuidanceController(mockControllerComponents)

  lazy val testGetRequest = FakeRequest("GET", "/")

  "Calling the show action of the Guidance controller" should {
    "go to the start page" in {

      val result = TestGuidanceController.show(testGetRequest)

      status(result) shouldBe Status.OK
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
    }
  }
}
