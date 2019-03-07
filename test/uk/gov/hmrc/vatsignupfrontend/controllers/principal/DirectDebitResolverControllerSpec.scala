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
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import uk.gov.hmrc.play.test.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.SessionKeys._
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch.DirectDebitTermsJourney
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockControllerComponents
import play.api.http.Status.{NOT_IMPLEMENTED, SEE_OTHER}

class DirectDebitResolverControllerSpec extends UnitSpec with GuiceOneAppPerSuite with MockControllerComponents {

  object TestDirectDebitResolverController extends DirectDebitResolverController(mockControllerComponents)

  private def sessionValues(directDebitFlag: Option[String]): Iterable[(String, String)] = directDebitFlag map(directDebitKey -> _)

  def testGetRequest(directDebitFlag: Option[String]): FakeRequest[AnyContentAsEmpty.type] = {
    FakeRequest("GET", "/direct-debit-resolver")
      .withSession(sessionValues(directDebitFlag).toSeq: _*)
  }

  "Calling the show action of the Direct Debit Resolver controller" when {
    "all prerequisite data are in session" when {
      "the feature switch has been enabled show" should {
        "respond with Not Implemented" in { // TODO: Once implementation for views has been completed, test can be adapted.
          mockAuthAdminRole()
          enable(DirectDebitTermsJourney)

          val result = TestDirectDebitResolverController.show(
            testGetRequest(directDebitFlag = Some("true")))

          status(result) shouldBe NOT_IMPLEMENTED
        }
      }

      "the feature switch has been disabled" should {
        "redirect to the email template" in { // TODO: Once implementation for views has been completed, test can be adapted.
          mockAuthAdminRole()
          disable(DirectDebitTermsJourney)

          val result = TestDirectDebitResolverController.show(
            testGetRequest(directDebitFlag = Some("true")))

          status(result) shouldBe SEE_OTHER
        }
      }
    }

    "prerequisite data are missing from session" when {
      "the feature switch has been enabled" should {
        "redirect to the email template" in { // TODO: Once implementation for views has been completed, test can be adapted.
          mockAuthAdminRole()
          enable(DirectDebitTermsJourney)

          val result = TestDirectDebitResolverController.show(
            testGetRequest(directDebitFlag = None))

          status(result) shouldBe SEE_OTHER
        }
      }
    }
  }
}
