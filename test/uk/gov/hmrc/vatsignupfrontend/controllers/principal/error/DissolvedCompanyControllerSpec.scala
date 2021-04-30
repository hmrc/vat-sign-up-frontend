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
import play.api.http.Status
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.vatsignupfrontend.utils.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockVatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.controllers.principal.partnerships.{routes => partnershipRoutes}
import uk.gov.hmrc.vatsignupfrontend.controllers.principal.{routes => principalRoutes}
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants.testCompanyName
import uk.gov.hmrc.vatsignupfrontend.models.{LimitedCompany, LimitedPartnership, RegisteredSociety}
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.dissolved_company

class DissolvedCompanyControllerSpec extends UnitSpec with GuiceOneAppPerSuite with MockVatControllerComponents {

  val view = app.injector.instanceOf[dissolved_company]

  object TestDissolvedCompanyController extends DissolvedCompanyController(view)

  lazy val testGetRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/error/dissolved-company")

  "Calling the show action of DissolvedCompanyController with a company name in session" should {
    "show the dissolved company view" in {
      mockAuthAdminRole()

      val result = TestDissolvedCompanyController.show(testGetRequest.withSession(SessionKeys.companyNameKey -> testCompanyName))

      status(result) shouldBe Status.OK
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
    }
  }

  "Calling the show action of DissolvedCompanyController without a company name in session" should {
    "redirect to CaptureCompanyNumberController if the business entity is Limited Company" in {
      mockAuthAdminRole()

      val result = TestDissolvedCompanyController.show(testGetRequest.withSession(SessionKeys.businessEntityKey -> LimitedCompany.toString))

      status(result) shouldBe Status.SEE_OTHER
      redirectLocation(result) shouldBe Some(principalRoutes.CaptureCompanyNumberController.show().url)
    }
    "redirect to CapturePartnershipCompanyNumberController if the business entity is Partnership" in {
      mockAuthAdminRole()

      val result = TestDissolvedCompanyController.show(testGetRequest.withSession(SessionKeys.businessEntityKey -> LimitedPartnership.toString))

      status(result) shouldBe Status.SEE_OTHER
      redirectLocation(result) shouldBe Some(partnershipRoutes.CapturePartnershipCompanyNumberController.show().url)
    }
    "redirect to CaptureRegisteredSocietyCompanyNumberController if the business entity is Registered Society" in {
      mockAuthAdminRole()

      val result = TestDissolvedCompanyController.show(testGetRequest.withSession(SessionKeys.businessEntityKey -> RegisteredSociety.toString))

      status(result) shouldBe Status.SEE_OTHER
      redirectLocation(result) shouldBe Some(principalRoutes.CaptureRegisteredSocietyCompanyNumberController.show().url)
    }
    "redirect to CaptureBusinessEntityController without a Business Entity" in {
      mockAuthAdminRole()

      val result = TestDissolvedCompanyController.show(testGetRequest)

      status(result) shouldBe Status.SEE_OTHER
      redirectLocation(result) shouldBe Some(principalRoutes.CaptureBusinessEntityController.show().url)
    }
  }
}
