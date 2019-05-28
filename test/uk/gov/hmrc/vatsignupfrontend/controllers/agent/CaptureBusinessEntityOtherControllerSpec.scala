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

package uk.gov.hmrc.vatsignupfrontend.controllers.agent

import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.mvc.AnyContentAsFormUrlEncoded
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.play.test.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch._
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockControllerComponents
import uk.gov.hmrc.vatsignupfrontend.forms.OtherBusinessEntityForm._
import uk.gov.hmrc.vatsignupfrontend.models.BusinessEntity.BusinessEntitySessionFormatter
import uk.gov.hmrc.vatsignupfrontend.models._

class CaptureBusinessEntityOtherControllerSpec extends UnitSpec with GuiceOneAppPerSuite with MockControllerComponents {

  object TestCaptureBusinessEntityOtherController extends CaptureBusinessEntityOtherController(mockControllerComponents)

  implicit val testGetRequest = FakeRequest("GET", "/business-type-other")

  def testPostRequest(entityTypeVal: String): FakeRequest[AnyContentAsFormUrlEncoded] =
    FakeRequest("POST", "/business-type-other").withFormUrlEncodedBody(businessEntity -> entityTypeVal)


  "Calling the show action of the Capture Entity Type controller" should {
    "go to the Capture Entity Type page" in {
      mockAuthRetrieveAgentEnrolment()

      val result = TestCaptureBusinessEntityOtherController.show(testGetRequest)
      status(result) shouldBe Status.OK
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
    }
  }


  "Calling the submit action of the Capture Business Entity controller" when {
    "form successfully submitted" should {

      "the business entity is vat group" when {
        "redirect to Vat Group Resolver page" in {
          mockAuthRetrieveAgentEnrolment()

          val result = await(TestCaptureBusinessEntityOtherController.submit(testPostRequest(vatGroup)))
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.VatGroupResolverController.resolve().url)
          result.session get SessionKeys.businessEntityKey should contain(BusinessEntitySessionFormatter.toString(VatGroup))
        }
      }

      "the business entity is division" when {
        "redirect to Division Resolver page" in {
          mockAuthRetrieveAgentEnrolment()

          enable(DivisionJourney)
          val result = await(TestCaptureBusinessEntityOtherController.submit(testPostRequest(division)))
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.DivisionResolverController.resolve().url)
          result.session get SessionKeys.businessEntityKey should contain(BusinessEntitySessionFormatter.toString(Division))
        }
      }

      "the business entity is an Unincorporated Association" when {
        "redirect to the Unincorporated Association resolver page" in {
          mockAuthRetrieveAgentEnrolment()

          enable(UnincorporatedAssociationJourney)
          val result = await(TestCaptureBusinessEntityOtherController.submit(testPostRequest(unincorporatedAssociation)))
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.UnincorporatedAssociationResolverController.resolve().url)
          result.session get SessionKeys.businessEntityKey should contain(BusinessEntitySessionFormatter.toString(UnincorporatedAssociation))
        }
      }

      "the business entity is a trust" when {
        "redirect to the trust resolver controller" in {
          mockAuthRetrieveAgentEnrolment()
          enable(TrustJourney)

          val result = await(TestCaptureBusinessEntityOtherController.submit(testPostRequest(trust)))
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.TrustResolverController.resolve().url)
          result.session get SessionKeys.businessEntityKey should contain(BusinessEntitySessionFormatter.toString(Trust))
        }
      }

      "the business entity is a registered society" when {
        "redirect to the capture registered society company number controller" in {
          mockAuthRetrieveAgentEnrolment()
          enable(RegisteredSocietyJourney)

          val result = await(TestCaptureBusinessEntityOtherController.submit(testPostRequest(registeredSociety)))
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.CaptureRegisteredSocietyCompanyNumberController.show().url)
          result.session get SessionKeys.businessEntityKey should contain(BusinessEntitySessionFormatter.toString(RegisteredSociety))
        }
      }

      "the business entity is a charity" when {
        "redirect to the charity resolver controller" in {
          mockAuthRetrieveAgentEnrolment()

          val result = await(TestCaptureBusinessEntityOtherController.submit(testPostRequest(charity)))
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.CharityResolverController.resolve().url)
          result.session get SessionKeys.businessEntityKey should contain(BusinessEntitySessionFormatter.toString(Charity))
        }
      }

      "the business entity is a government organisation" when {
        "redirect to the NOT IMPLEMENTED" in {
          mockAuthRetrieveAgentEnrolment()
          enable(GovernmentOrganisationJourney)

          val result = await(TestCaptureBusinessEntityOtherController.submit(testPostRequest(governmentOrganisation)))
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.GovernmentOrganisationResolverController.resolve().url)
          result.session get SessionKeys.businessEntityKey should contain(BusinessEntitySessionFormatter.toString(GovernmentOrganisation))
        }
      }
    }

    "form unsuccessfully submitted" should {
      "reload the page with errors" in {
        mockAuthRetrieveAgentEnrolment()

        val result = TestCaptureBusinessEntityOtherController.submit(testPostRequest("invalid"))
        status(result) shouldBe Status.BAD_REQUEST
        contentType(result) shouldBe Some("text/html")
        charset(result) shouldBe Some("utf-8")
      }
    }

  }

}
