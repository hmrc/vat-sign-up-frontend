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

package uk.gov.hmrc.vatsignupfrontend


object SessionKeys {
  val vatNumberKey = "VatNumber"
  val isFromBtaKey = "BTA"
  val companyNumberKey = "CompanyNumber"
  val registeredSocietyCompanyNumberKey = "RegisteredSocietyCompanyNumber"
  val companyNameKey = "CompanyName"
  val registeredSocietyNameKey = "RegisteredSocietyCompanyName"
  val companyUtrKey = "CompanyUtr"
  val registeredSocietyUtrKey = "RegisteredSocietyUtr"
  val emailKey = "Email"
  val transactionEmailKey = "TransactionEmail"
  val userDetailsKey = "UserDetails"
  val businessEntityKey = "BusinessEntity"
  val identityVerificationContinueUrlKey = "IdentityVerification"
  val vatRegistrationDateKey = "VatRegistrationDate"
  val businessPostCodeKey = "BusinessPostCode"
  val partnershipPostCodeKey = "PartnershipPostCode"
  val partnershipSautrKey = "PartnershipSautr"
  val partnershipTypeKey = "PartnershipType"
  val migratableDatesKey = "MigratableDates"
  val ninoSourceKey = "NinoSource"
  val previousVatReturnKey = "previousVatReturn"
  val lastReturnMonthPeriodKey = "lastReturnMonthPeriod"
  val box5FigureKey = "box5Figure"
  val acceptedDirectDebitTermsKey = "acceptedDirectDebitTerms"
  val directDebitKey = ??? // TODO : To be confirmed
}
