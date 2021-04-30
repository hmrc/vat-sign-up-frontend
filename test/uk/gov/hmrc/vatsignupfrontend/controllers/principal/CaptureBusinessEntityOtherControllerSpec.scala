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

import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.mvc.{AnyContentAsEmpty, AnyContentAsFormUrlEncoded}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.vatsignupfrontend.utils.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockVatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.forms.OtherBusinessEntityForm._
import uk.gov.hmrc.vatsignupfrontend.models.BusinessEntity.BusinessEntitySessionFormatter
import uk.gov.hmrc.vatsignupfrontend.models._
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.capture_business_entity_other

class CaptureBusinessEntityOtherControllerSpec extends UnitSpec with GuiceOneAppPerSuite with MockVatControllerComponents {

  val view = app.injector.instanceOf[capture_business_entity_other]
  object TestCaptureBusinessEntityOtherController extends CaptureBusinessEntityOtherController(view)

  val testGetRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/business-type-other")

  def testPostRequest(entityTypeVal: String): FakeRequest[AnyContentAsFormUrlEncoded] =
    FakeRequest("POST", "/business-type-other").withFormUrlEncodedBody(businessEntity -> entityTypeVal)

  "Calling the show action of the Capture Entity Type controller" should {
    "go to the Capture Entity Other Type page" in {
      mockAuthAdminRole()

      val result = TestCaptureBusinessEntityOtherController.show(testGetRequest)
      status(result) shouldBe Status.OK
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
    }
  }

  "Calling the submit action of the Capture Business Entity Other controller" when {
    "form successfully submitted" when {
      "the business entity is vat group" should {
        "goto vat group resolver" in {
          mockAuthAdminRole()
          implicit val request: FakeRequest[AnyContentAsFormUrlEncoded] = testPostRequest(vatGroup)

          val result = TestCaptureBusinessEntityOtherController.submit(request)
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.VatGroupResolverController.resolve().url)

          session(result).get(SessionKeys.businessEntityKey) should contain(BusinessEntitySessionFormatter.toString(VatGroup))
        }
      }

      "the business entity is unincorporated association" should {
        "goto unincorporated association resolver" in {
          mockAuthAdminRole()
          implicit val request: FakeRequest[AnyContentAsFormUrlEncoded] = testPostRequest(unincorporatedAssociation)

          val result = TestCaptureBusinessEntityOtherController.submit(request)
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.UnincorporatedAssociationResolverController.resolve().url)

          session(result).get(SessionKeys.businessEntityKey) should contain(
            BusinessEntitySessionFormatter.toString(UnincorporatedAssociation)
          )
        }
      }

      "the business entity is trust" should {
        "goto trust resolver" in {
          mockAuthAdminRole()
          implicit val request: FakeRequest[AnyContentAsFormUrlEncoded] = testPostRequest(trust)

          val result = TestCaptureBusinessEntityOtherController.submit(request)
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.TrustResolverController.resolve().url)

          session(result).get(SessionKeys.businessEntityKey) should contain(
            BusinessEntitySessionFormatter.toString(Trust)
          )
        }
      }

      "the business entity is registered society" should {
        "goto the capture society company number page" in {
          mockAuthAdminRole()
          implicit val request: FakeRequest[AnyContentAsFormUrlEncoded] = testPostRequest(registeredSociety)

          val result = TestCaptureBusinessEntityOtherController.submit(request)
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.CaptureRegisteredSocietyCompanyNumberController.show().url)

          session(result).get(SessionKeys.businessEntityKey) should contain(
            BusinessEntitySessionFormatter.toString(RegisteredSociety)
          )
        }
      }

      "the business entity is a charity" should {
        "goto charity resolver" in {
          mockAuthAdminRole()
          implicit val request: FakeRequest[AnyContentAsFormUrlEncoded] = testPostRequest(charity)

          val result = TestCaptureBusinessEntityOtherController.submit(request)
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.CharityResolverController.resolve().url)

          session(result).get(SessionKeys.businessEntityKey) should contain(
            BusinessEntitySessionFormatter.toString(Charity)
          )
        }
      }

      "the business entity is a government organisation" should {
        "goto government organisation resolver" in {
          mockAuthAdminRole()
          implicit val request: FakeRequest[AnyContentAsFormUrlEncoded] = testPostRequest(governmentOrganisation)

          val result = TestCaptureBusinessEntityOtherController.submit(request)
          status(result) shouldBe Status.SEE_OTHER
          redirectLocation(result) shouldBe Some(routes.GovernmentOrganisationResolverController.resolve().url)

          session(result).get(SessionKeys.businessEntityKey) should contain(
            BusinessEntitySessionFormatter.toString(GovernmentOrganisation)
          )
        }
      }

    }

    "form unsuccessfully submitted" should {
      "reload the page with errors" in {
        mockAuthAdminRole()

        val result = TestCaptureBusinessEntityOtherController.submit(testPostRequest("invalid"))
        status(result) shouldBe Status.BAD_REQUEST
        contentType(result) shouldBe Some("text/html")
        charset(result) shouldBe Some("utf-8")
      }
    }

  }

}
