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
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.play.test.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockControllerComponents

class PartnershipAsCompanyErrorControllerSpec extends UnitSpec with GuiceOneAppPerSuite with MockControllerComponents {

  object TestPartnershipAsCompanyErrorController extends PartnershipAsCompanyErrorController(mockControllerComponents)

  "Calling the show action of the Partnership As Company Error controller" should {
    "show the Partnership As Controller Error page" in {
      mockAuthAdminRole()

      val result = TestPartnershipAsCompanyErrorController.show(FakeRequest("GET", "/company-number-incorrect"))
      status(result) shouldBe Status.OK
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
    }
  }

  "Calling the submit action of Partnership As Company Error controller" when {
    "redirect to the Capture Business Entity page" in {
      mockAuthAdminRole()

      val result = TestPartnershipAsCompanyErrorController.submit(FakeRequest("POST", "/company-number-incorrect"))
      status(result) shouldBe Status.SEE_OTHER
      redirectLocation(result) should contain(routes.CaptureBusinessEntityController.show().url)
    }
  }

}
