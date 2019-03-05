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

package uk.gov.hmrc.vatsignupfrontend.helpers

import uk.gov.hmrc.vatsignupfrontend.forms.validation.utils.Patterns.CompanyNumber._

import scala.annotation.tailrec
import scala.util.Random

object TestConstantsGenerator {

  private val rand = new Random()

  private val UPPER_BOUND_8_DIGIT_NUMBER = 99999999
  private val UPPER_BOUND_7_DIGIT_NUMBER = 9999999
  private val UPPER_BOUND_6_DIGIT_NUMBER = 999999

  private def calcVatCheckSum(value: String): String = {
    val intValue = value.toInt

    // not efficient but saves writing out a hardcoded calculation or another recursive function
    val constants = (2 to 8).reverse
    val initSum = value.map(_.asDigit).zip(constants)
      .map { case (digit, constant) => digit * constant }.sum

    @tailrec
    def deduct(num: Int): Int = num - 97 match {
      case res if res > 0 => deduct(res)
      case res => res * -1
    }

    f"${deduct(initSum)}%02d"
  }

  def randomVatNumber: String = {
    val randomLead = f"${rand.nextInt(UPPER_BOUND_7_DIGIT_NUMBER + 1)}%07d"
    randomLead + calcVatCheckSum(randomLead)
  }

  def randomCrn: String = rand.nextInt(2) match {
    case 0 => randomCrnNumeric
    case 1 => randomCrnAlphaNumeric
    case 2 => randomCrnNumericNoLeadingZeros
  }

  private lazy val prefixes = validCompanyNumberPrefixes.toList

  private lazy val blockedPrefixes = blockedCompanyNumberPrefixes.toList

  def randomPrefix: String = prefixes(rand.nextInt(prefixes.size)).toLowerCase

  def randomBlockedPrefix: String = blockedPrefixes(rand.nextInt(blockedPrefixes.size))

  def randomCrnNumeric: String = "%08d".format(rand.nextInt(UPPER_BOUND_8_DIGIT_NUMBER) + 1)

  def randomCrnNumericNoLeadingZeros: String = "%8d".format(rand.nextInt(UPPER_BOUND_7_DIGIT_NUMBER) + 1)

  def randomCrnAlphaNumeric: String = randomPrefix + "%06d".format(rand.nextInt(UPPER_BOUND_6_DIGIT_NUMBER) + 1)

  def randomBlockedCrn: String = randomBlockedPrefix + "%06d".format(rand.nextInt(UPPER_BOUND_6_DIGIT_NUMBER) + 1)

  private def randomString(alphabet: String)(max: Int, min: Int = 1): String = {
    val rdm = rand.nextInt(max) + 1
    val length = if (rdm < min) min else rdm
    Stream.continually(rand.nextInt(alphabet.length)).map(alphabet).take(length).mkString
  }

  def randomAlpha(max: Int): String =
    randomString(('a' to 'z').mkString("") + ('A' to 'Z').mkString(""))(max)

  def randomNumeric(max: Int): String =
    randomString(('0' to '9').mkString(""))(max)


  def randomUTRNumeric(): String =
    randomString(('0' to '9').mkString(""))(10, 10)


  private def randomAlphaNumericWithAdditional(additionalChars: String)(max: Int): String =
    randomString(('a' to 'z').mkString("") + ('A' to 'Z').mkString("") + ('0' to '9').mkString("") + additionalChars)(max)

  def randomAlphaNumeric(max: Int): String = randomAlphaNumericWithAdditional("")(max)

  def randomEmail: String = {
    randomAlphaNumeric(1) +
      randomAlphaNumericWithAdditional(additionalChars = "_.+-")(rand.nextInt(16) + 1).replaceAll("[.]+", ".") +
      randomAlphaNumeric(1) +
      "@" +
      randomAlphaNumeric(1) + randomAlphaNumericWithAdditional(additionalChars = "-")(rand.nextInt(10) + 1) + randomAlphaNumeric(1) +
      "." + {
      (0 to rand.nextInt(2)).map(_ => randomAlphaNumeric(1) + randomAlphaNumericWithAdditional(additionalChars = "-")(rand.nextInt(3) + 1) + randomAlphaNumeric(1)).mkString(".")
    }
  }.toLowerCase()

  def randomPostCode: String =
    randomAlpha(2) +
      randomNumeric(1) + randomNumeric(1) + // must have 2 numeric
      randomAlpha(1) + randomAlpha(1) // must end with 2 alphas

}
