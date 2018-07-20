/*
 * Copyright 2018 HM Revenue & Customs
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
import play.api.data.validation.{Constraint, Invalid, Valid}
import uk.gov.hmrc.vatsignupfrontend.forms.validation.utils.ConstraintUtil._
import uk.gov.hmrc.vatsignupfrontend.forms.prevalidation._
import uk.gov.hmrc.vatsignupfrontend.forms.validation.utils.Patterns.{vatNumberRegex, vatNumberLengthRegex, vatNumberCharactersRegex}

object VatNumberForm {

  val vatNumber = "vatNumber"

  val vatNumberLength: Constraint[String] = Constraint("vat_number.Length")(
    x => if (!(x matches vatNumberLengthRegex)) Invalid("error.invalid_vat_number_length") else Valid
  )

  val vatNumberFormat: Constraint[String] = Constraint("vat_number.format")(
    x => if (!(x matches vatNumberRegex)) Invalid("error.invalid_vat_number") else Valid
  )

  val vatNumberCharacters: Constraint[String] = Constraint("vat_number.Length")(
    x => if (!(x matches vatNumberCharactersRegex)) Invalid("error.invalid_vat_number_characters") else Valid
  )

  private val vatNumberValidationForm = Form(
    single(
      vatNumber -> text.verifying(vatNumberLength andThen vatNumberCharacters andThen vatNumberFormat)
    )
  )

  import uk.gov.hmrc.vatsignupfrontend.forms.prevalidation.CaseOption._
  import uk.gov.hmrc.vatsignupfrontend.forms.prevalidation.TrimOption._

  val vatNumberForm = PreprocessedForm(
    validation = vatNumberValidationForm,
    trimRules = Map(vatNumber -> all),
    caseRules = Map(vatNumber -> upper)
  )
}
