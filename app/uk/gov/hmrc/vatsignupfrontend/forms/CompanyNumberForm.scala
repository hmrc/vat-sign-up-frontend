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

import play.api.data.Form
import play.api.data.Forms._
import play.api.data.validation.Constraint
import uk.gov.hmrc.vatsignupfrontend.forms.prevalidation.PreprocessedForm
import uk.gov.hmrc.vatsignupfrontend.forms.validation.utils.ConstraintUtil._
import uk.gov.hmrc.vatsignupfrontend.forms.validation.utils.MappingUtil._
import uk.gov.hmrc.vatsignupfrontend.forms.validation.utils.ValidationHelper._

object CompanyNumberForm {

  val companyNumber = "companyNumber"
  private val maxLength = 8
  private val minLength = 1

  def withinMinAndMaxLength: Constraint[String] = Constraint("companyNumber.maxLength")(
    companyNumber => validateNot(
      constraint = (companyNumber.length >= minLength) && (companyNumber.length <= maxLength),
      principalErrMsg = "error.invalid_company_number_length"
    )
  )

  def crnNotEntered(isAgent: Boolean, isPartnership: Boolean): Constraint[String] = Constraint("companyNumber.notEntered")(
    companyNumber => {

      val principalErrMsg =
        if (isPartnership)
          "error.principal.partnership_company_number_not_entered"
        else
          "error.principal.company_number_not_entered"

      val agentErrMsg =
        if (isPartnership)
          "error.agent.partnership_company_number_not_entered"
        else
          "error.agent.company_number_not_entered"

      validate(
        constraint = companyNumber.isEmpty,
        principalErrMsg = principalErrMsg,
        agentErrMsg = Some(agentErrMsg),
        isAgent = isAgent
      )
    }
  )

  private def companyNumberValidationForm(isAgent: Boolean, isPartnership: Boolean) = Form(
    single(
      companyNumber -> optText.toText.verifying(crnNotEntered(isAgent = isAgent, isPartnership = isPartnership) andThen withinMinAndMaxLength)
    )
  )

  import uk.gov.hmrc.vatsignupfrontend.forms.prevalidation.CaseOption._
  import uk.gov.hmrc.vatsignupfrontend.forms.prevalidation.TrimOption._

  def companyNumberForm(isAgent: Boolean, isPartnership: Boolean) = PreprocessedForm(
    validation = companyNumberValidationForm(isAgent = isAgent, isPartnership = isPartnership),
    trimRules = Map(companyNumber -> all),
    caseRules = Map(companyNumber -> upper)
  )

}
