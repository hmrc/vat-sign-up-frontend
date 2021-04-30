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

package uk.gov.hmrc.vatsignupfrontend.controllers.principal.partnerships

import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.i18n.Messages
import play.api.mvc.{AnyContentAsEmpty, AnyContentAsFormUrlEncoded}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthorisationException
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.mocks.MockVatControllerComponents
import uk.gov.hmrc.vatsignupfrontend.forms.PartnershipUtrForm
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.models._
import uk.gov.hmrc.vatsignupfrontend.utils.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.partnerships.capture_partnership_utr
import uk.gov.hmrc.vatsignupfrontend.views.html.principal.partnerships.capture_partnership_utr

class CapturePartnershipUtrControllerSpec extends UnitSpec with GuiceOneAppPerSuite with MockVatControllerComponents {

  val view = app.injector.instanceOf[capture_partnership_utr]

  object TestCapturePartnershipUtrController extends CapturePartnershipUtrController(view)

  val testGetRequestForNoUtr: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/partnership-no-utr")
  val testGetRequestForShow: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/partnership-utr")

  def testPostRequest(utr: String): FakeRequest[AnyContentAsFormUrlEncoded] =
    FakeRequest("POST", "/partnership-utr").withFormUrlEncodedBody(PartnershipUtrForm.partnershipUtr -> utr)

  "Calling the show action of the CapturePartnershipUtrController" when {
    s"go to the Partnership utr page with the right content && $GeneralPartnership" in {
      mockAuthAdminRole()

      implicit val testGetRequestForShow: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/partnership-utr").withSession(
        SessionKeys.businessEntityKey -> BusinessEntity.GeneralPartnershipKey
      )
      implicit val messages: Messages = mockVatControllerComponents.controllerComponents.messagesApi.preferred(testGetRequestForShow)

      lazy val viewBody = view(
        partnershipUtrForm = PartnershipUtrForm.partnershipUtrForm.form,
        postAction = routes.CapturePartnershipUtrController.submit(),
        displayGeneralPartnershipAccordion = true
      ).body

      val result = TestCapturePartnershipUtrController.show(testGetRequestForShow.withSession(
        SessionKeys.businessEntityKey -> BusinessEntity.GeneralPartnershipKey
      ))
      status(result) shouldBe Status.OK
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
      contentAsString(result) shouldBe viewBody
    }

    s"go to the Partnership utr page with the right content && $LimitedPartnership" in {
      mockAuthAdminRole()

      implicit val testGetRequestForShow: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/partnership-utr").withSession(
        SessionKeys.businessEntityKey -> BusinessEntity.LimitedPartnershipKey
      )

      implicit val messages: Messages = mockVatControllerComponents.controllerComponents.messagesApi.preferred(testGetRequestForShow)

      lazy val viewBody = view(
        partnershipUtrForm = PartnershipUtrForm.partnershipUtrForm.form,
        postAction = routes.CapturePartnershipUtrController.submit(),
        displayGeneralPartnershipAccordion = false
      ).body

      val result = TestCapturePartnershipUtrController.show(testGetRequestForShow.withSession(
        SessionKeys.businessEntityKey -> BusinessEntity.LimitedPartnershipKey
      ))
      status(result) shouldBe Status.OK
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
      contentAsString(result) shouldBe viewBody
    }
  }

  "Calling the noUtrSelected action of the CapturePartnershipUtrController" should {
    s"redirect to check your answers page & drop ${SessionKeys.partnershipSautrKey}${SessionKeys.partnershipPostCodeKey}" in {
      mockAuthAdminRole()

      val result = TestCapturePartnershipUtrController.noUtrSelected(testGetRequestForNoUtr.withSession(
        SessionKeys.partnershipSautrKey -> testSaUtr,
        SessionKeys.partnershipPostCodeKey -> testBusinessPostcode.sanitisedPostCode,
        SessionKeys.previousVatReturnKey -> Yes.stringValue)
      )

      status(result) shouldBe Status.SEE_OTHER
      redirectLocation(result).get shouldBe routes.CheckYourAnswersPartnershipsController.show().url
      session(result) get SessionKeys.partnershipSautrKey shouldBe None
      session(result) get SessionKeys.partnershipPostCodeKey shouldBe None
      session(result) get SessionKeys.previousVatReturnKey should contain(Yes.stringValue)
    }

    "have an auth check and return exception if not authorised" in {
      mockFailedAuth()

      intercept[AuthorisationException](TestCapturePartnershipUtrController.noUtrSelected(testGetRequestForNoUtr))
    }
  }

  "Calling the submit action of the CapturePartnershipUtrController" when {
    s"form successfully submitted by $GeneralPartnership" should {
      "redirect to PPOB" in {
        mockAuthAdminRole()

        implicit val request: FakeRequest[AnyContentAsFormUrlEncoded] = testPostRequest(testSaUtr).withSession(
          SessionKeys.businessEntityKey -> BusinessEntity.GeneralPartnershipKey
        )

        val result = TestCapturePartnershipUtrController.submit(request)

        status(result) shouldBe Status.SEE_OTHER
        redirectLocation(result) shouldBe Some(routes.PrincipalPlacePostCodeController.show().url)
        session(result) get SessionKeys.businessEntityKey should contain(BusinessEntity.GeneralPartnershipKey)
        session(result) get SessionKeys.partnershipSautrKey should contain(testSaUtr)
      }
    }

  }

  s"form successfully submitted by $LimitedPartnership" should {
    "redirect to PPOB" in {
      mockAuthAdminRole()

      implicit val request: FakeRequest[AnyContentAsFormUrlEncoded] = testPostRequest(testSaUtr).withSession(
        SessionKeys.businessEntityKey -> BusinessEntity.LimitedPartnershipKey
      )

      val result = TestCapturePartnershipUtrController.submit(request)

      status(result) shouldBe Status.SEE_OTHER
      redirectLocation(result) shouldBe Some(routes.PrincipalPlacePostCodeController.show().url)
      session(result) get SessionKeys.businessEntityKey should contain(BusinessEntity.LimitedPartnershipKey)
      session(result) get SessionKeys.partnershipSautrKey should contain(testSaUtr)
    }
  }

  "form unsuccessfully submitted" should {
    s"reload the page with errors with the right content && $GeneralPartnership" in {
      mockAuthAdminRole()

      implicit val request: FakeRequest[AnyContentAsFormUrlEncoded] = testPostRequest("")

      implicit val messages: Messages = mockVatControllerComponents.controllerComponents.messagesApi.preferred(request)

      lazy val viewBody = view(
        partnershipUtrForm = PartnershipUtrForm.partnershipUtrForm.form.bindFromRequest()(testGetRequestForNoUtr),
        postAction = routes.CapturePartnershipUtrController.submit(),
        displayGeneralPartnershipAccordion = true
      ).body

      val result = TestCapturePartnershipUtrController.submit(request.withSession(
        SessionKeys.businessEntityKey -> BusinessEntity.GeneralPartnershipKey
      ))

      status(result) shouldBe Status.BAD_REQUEST
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
      contentAsString(result) shouldBe viewBody
    }

    s"reload the page with errors with the right content && $LimitedPartnership" in {
      mockAuthAdminRole()

      implicit val request: FakeRequest[AnyContentAsFormUrlEncoded] = testPostRequest("")

      implicit val messages: Messages = mockVatControllerComponents.controllerComponents.messagesApi.preferred(request)

      lazy val viewBody = view(
        partnershipUtrForm = PartnershipUtrForm.partnershipUtrForm.form.bindFromRequest()(testGetRequestForNoUtr),
        postAction = routes.CapturePartnershipUtrController.submit(),
        displayGeneralPartnershipAccordion = false
      ).body

      val result = TestCapturePartnershipUtrController.submit(request.withSession(
        SessionKeys.businessEntityKey -> BusinessEntity.LimitedPartnershipKey
      ))

      status(result) shouldBe Status.BAD_REQUEST
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
      contentAsString(result) shouldBe viewBody
    }
  }

}
