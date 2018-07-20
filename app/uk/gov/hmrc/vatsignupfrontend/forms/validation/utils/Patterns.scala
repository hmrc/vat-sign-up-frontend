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

package uk.gov.hmrc.vatsignupfrontend.forms.validation.utils

import scala.util.matching.Regex

object Patterns {

  // ISO 8859-1 standard
  // ASCII range {32 to 126} + {160 to 255} all values inclusive
  val iso8859_1Regex =
  """^([\x20-\x7E\xA0-\xFF])*$"""

  // http://emailregex.com/
  // n.b. this regex rejects upper case characters
  val emailRegex = """^(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|"(?:[\x01-\x08\x0b\x0c\x0e-\x1f\
                     |x21\x23-\x5b\x5d-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])*")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[
                     |a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\[(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4]
                     |[0-9]|[01]?[0-9][0-9]?|[a-z0-9-]*[a-z0-9]:(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21-\x5a\x53-\x7f]|\\[\x01-\
                     |x09\x0b\x0c\x0e-\x7f])+)\])$""".stripMargin

  val ninoRegex = """^((?!(BG|GB|KN|NK|NT|TN|ZZ)|(D|F|I|Q|U|V)[A-Z]|[A-Z](D|F|I|O|Q|U|V))[A-Z]{2})[0-9]{6}[A-D]$"""

  val vatNumberLengthRegex = "{9}"

  val vatNumberRegex = "[0-9]{9}"

  val vatNumberCharactersRegex = "[0-9]+"

  val companyUtrRegex = "[0-9]{10}"

  val postcodeRegex = """^[A-Z]{1,2}[0-9][0-9A-Z]?\s?[0-9][A-Z]{2}$|BFPO\s?[0-9]{1,3}$"""

  def validText(text: String): Boolean = text matches iso8859_1Regex

  def validEmail(text: String): Boolean = text matches emailRegex

  def validNino(text: String): Boolean = text matches ninoRegex

  def validPostcode(text: String): Boolean = text matches postcodeRegex

  object CompanyNumber {
    val allNumbersRegex: Regex = "^([0-9]{1,8})$".r
    val withPrefixRegex: Regex = "^([A-Za-z][A-Za-z0-9])([0-9]{0,6})$".r

    // https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/426891/uniformResourceIdentifiersCustomerGuide.pdf
    lazy val validCompanyNumberPrefixes = Set(
      "AC", "ZC", "FC", "GE",
      "LP", "OC", "SE", "SA",
      "SZ", "SF", "GS", "SL",
      "SO", "SC", "ES", "NA",
      "NZ", "NF", "GN", "NL",
      "NC", "R0", "NI", "EN",
      "IP", "SP", "IC", "SI",
      "NP", "NV", "RC", "SR",
      "NR", "NO"
    )
  }

}
