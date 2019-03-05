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

import org.scalatest.Matchers._
import org.scalatestplus.play.PlaySpec
import play.api.data.FormError
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstantsGenerator

class CompanyNumberFormSpec extends PlaySpec {

  import uk.gov.hmrc.vatsignupfrontend.forms.CompanyNumberForm._

  val validateCompanyNumberForm = companyNumberForm(isAgent = false, isPartnership = false)

  "The companyNumberForm" should {

    "validate that data had been entered - none-partnership" in {
      val validateCompanyNumberForm = companyNumberForm(isAgent = false, isPartnership = false)
      val notEnteredErrorKey = "error.principal.company_number_not_entered"
      val formWithError = validateCompanyNumberForm.bind(Map(companyNumber -> ""))
      formWithError.errors should contain(FormError(companyNumber, notEnteredErrorKey))
    }

    "validate that data had been entered - partnership" in {
      val validateCompanyNumberForm = companyNumberForm(isAgent = false, isPartnership = true)
      val notEnteredErrorKey = "error.principal.partnership_company_number_not_entered"
      val formWithError = validateCompanyNumberForm.bind(Map(companyNumber -> ""))
      formWithError.errors should contain(FormError(companyNumber, notEnteredErrorKey))
    }

    "validate that data had been entered - agent none-partnership client" in {
      val validateCompanyNumberForm = companyNumberForm(isAgent = true, isPartnership = false)
      val notEnteredErrorKey = "error.agent.company_number_not_entered"
      val formWithError = validateCompanyNumberForm.bind(Map(companyNumber -> ""))
      formWithError.errors should contain(FormError(companyNumber, notEnteredErrorKey))
    }

    "validate that data had been entered - agent partnership client" in {
      val validateCompanyNumberForm = companyNumberForm(isAgent = true, isPartnership = true)
      val notEnteredErrorKey = "error.agent.partnership_company_number_not_entered"
      val formWithError = validateCompanyNumberForm.bind(Map(companyNumber -> ""))
      formWithError.errors should contain(FormError(companyNumber, notEnteredErrorKey))
    }

    "validate that data containing 8 digits passes" in {
      val testCrn = TestConstantsGenerator.randomCrnNumeric
      val actual = validateCompanyNumberForm.bind(Map(companyNumber -> testCrn)).value
      actual shouldBe Some(testCrn)
    }

    "validate that data containing more than 8 digits fails" in {
      val invalidLengthErrorKey = "error.invalid_company_number_length"
      val testCrn = TestConstantsGenerator.randomCrnNumeric + "1"
      val formWithError = validateCompanyNumberForm.bind(Map(companyNumber -> testCrn))
      formWithError.errors should contain(FormError(companyNumber, invalidLengthErrorKey))
    }

    "validate that data containing fewer than 8 digits but greater than 0 passes" in {
      val testCrn = "1"
      val actual = validateCompanyNumberForm.bind(Map(companyNumber -> testCrn)).value
      actual shouldBe Some(testCrn)
    }

    "validate that the a CRN that starts with a blocked prefix fails validation" in {
      val testCrn = TestConstantsGenerator.randomBlockedCrn
      val result = isBlockedCrn(testCrn)
      result shouldBe(true)
    }

    "validate that the a CRN that starts with a valid prefix passes validation" in {
      val testCrn = TestConstantsGenerator.randomCrnAlphaNumeric
      val result = isBlockedCrn(testCrn)
      result shouldBe(false)
    }

    "validate that the a CRN that starts with no prefix passes validation" in {
      val testCrn = TestConstantsGenerator.randomCrnNumeric
      val result = isBlockedCrn(testCrn)
      result shouldBe(false)
    }
  }

}
