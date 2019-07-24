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

package uk.gov.hmrc.vatsignupfrontend.forms
import play.api.data.{Form, FormError}
import uk.gov.hmrc.play.test.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.forms.submapping.YesNoMapping
import uk.gov.hmrc.vatsignupfrontend.models._


class AreYouReadySubmitSoftwareFormSpec extends UnitSpec {

  s"the $AreYouReadySubmitSoftwareForm" should {
    "return error when filled with no data" in {
      val formValidated: Form[YesNo] = AreYouReadySubmitSoftwareForm.areYouReadySubmitSoftwareForm.bind(
        Map(
          AreYouReadySubmitSoftwareForm.yesNo -> ""
        )
      )

      formValidated.errors should contain(FormError(AreYouReadySubmitSoftwareForm.yesNo,"error.principal.are_you_ready_submit_software"))
    }
    s"return error when filled with invalid data not $Yes or $No and not Empty String" in {
      val formValidated: Form[YesNo] = AreYouReadySubmitSoftwareForm.areYouReadySubmitSoftwareForm.bind(
        Map(
          AreYouReadySubmitSoftwareForm.yesNo -> "invalidFormData"
        )
      )

      formValidated.errors should contain(FormError(AreYouReadySubmitSoftwareForm.yesNo,"error.principal.are_you_ready_submit_software"))
    }
    s"return a successfully bound form for $Yes value" in {
      val formValidated: Form[YesNo] = AreYouReadySubmitSoftwareForm.areYouReadySubmitSoftwareForm.bind(
        Map(
          AreYouReadySubmitSoftwareForm.yesNo -> YesNoMapping.option_yes
        )
      )
      formValidated.value shouldBe Some(Yes)
    }
    s"return a successfully bound form for $No value" in {
      val formValidated: Form[YesNo] = AreYouReadySubmitSoftwareForm.areYouReadySubmitSoftwareForm.bind(
        Map(
          AreYouReadySubmitSoftwareForm.yesNo -> YesNoMapping.option_no
        )
      )
      formValidated.value shouldBe Some(No)
    }
  }
}
