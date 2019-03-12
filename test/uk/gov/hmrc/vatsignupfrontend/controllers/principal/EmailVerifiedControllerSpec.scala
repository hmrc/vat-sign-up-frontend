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
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch.ContactPreferencesJourney
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockControllerComponents

class EmailVerifiedControllerSpec extends UnitSpec with GuiceOneAppPerSuite with MockControllerComponents {

  object TestEmailVerifiedController extends EmailVerifiedController(mockControllerComponents)

  "Calling the show action of the Email Verified controller" should {
      lazy val testGetRequest = FakeRequest("GET", "/email-verified")

      "go to the Email Verified page" in {
        mockAuthAdminRole()

        val result = TestEmailVerifiedController.show(testGetRequest)

        status(result) shouldBe Status.OK
        contentType(result) shouldBe Some("text/html")
        charset(result) shouldBe Some("utf-8")
      }
    }

  "Calling the submit action of the Email Verified controller" should {
    lazy val testPostRequest = FakeRequest("POST", "/email-verified")
    "return a not implemented" when {
      "the contact preferences feature switch is enabled" in {
        mockAuthAdminRole()
        enable(ContactPreferencesJourney)

        val result = TestEmailVerifiedController.submit(testPostRequest)

        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.ReceiveEmailNotificationsController.show().url)
      }
    }
    "redirect to Terms controller" when {
      "the contact preferences feature switch is disabled" in {
        mockAuthAdminRole()
        disable(ContactPreferencesJourney)

        val result = TestEmailVerifiedController.submit(testPostRequest)

        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.TermsController.show().url)
      }
    }
  }

}
