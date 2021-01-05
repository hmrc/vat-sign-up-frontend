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

package uk.gov.hmrc.vatsignupfrontend.helpers

import java.time.LocalDate
import java.util.UUID

import uk.gov.hmrc.auth.core.Enrolment
import uk.gov.hmrc.domain.Generator
import uk.gov.hmrc.vatsignupfrontend.Constants.Enrolments._
import uk.gov.hmrc.vatsignupfrontend.models.{DateModel, January, PostCode}

import scala.util.Random

object TestConstants {
  val testVatNumber: String = TestConstantsGenerator.randomVatNumber
  val testInvalidVatNumber: String = "999999999"
  val testName: String = UUID.randomUUID().toString
  val testBusinessPostcode: PostCode = PostCode(TestConstantsGenerator.randomPostCode)
  val testCompanyNumber: String = TestConstantsGenerator.randomCrnNumeric
  val testCompanyUtr: String = TestConstantsGenerator.randomUTRNumeric()
  val testCompanyName: String = Random.alphanumeric.take(10).mkString
  val testEmail: String = TestConstantsGenerator.randomEmail
  val testAgentEmail: String = TestConstantsGenerator.randomEmail
  val testNino: String = new Generator().nextNino.nino
  val testSaUtr: String = f"${Math.abs(Random.nextLong() % 10000000000L)}%010d"
  val testUri: String = "/test/uri"
  val testPartnershipType: String = "limitedPartnership"

  val testAgentReference = UUID.randomUUID().toString

  val testBox5Figure = "99999999999.99"
  val testBox5FigureNegative = "-99999999999.99"
  val testLastReturnMonthPeriod = January.toString

  val testAgentEnrolment: Enrolment = Enrolment(agentEnrolmentKey) withIdentifier(agentReferenceKey, testAgentReference)
  val testVatDecEnrolment: Enrolment = Enrolment(VatDecEnrolmentKey) withIdentifier(VatReferenceKey, testVatNumber)
  val testMtdVatEnrolment: Enrolment = Enrolment(MtdVatEnrolmentKey) withIdentifier(MtdVatReferenceKey, testVatNumber)

  val testIRSAEnrolment: Enrolment = Enrolment(IRSAEnrolmentKey) withIdentifier(IRSAReferenceKey, testSaUtr)
  val testIRCTEnrolment: Enrolment = Enrolment(IRCTEnrolmentKey) withIdentifier(IRCTReferenceKey, testSaUtr)
  val testPartnershipEnrolment: Enrolment = Enrolment(PartnershipEnrolmentKey) withIdentifier(PartnershipReferenceKey, testSaUtr)

  val testStartDate: LocalDate = LocalDate.now()
  val testEndDate: LocalDate = testStartDate.plusMonths(3)

  val testDateModel: DateModel = DateModel.dateConvert(LocalDate.now())
}
