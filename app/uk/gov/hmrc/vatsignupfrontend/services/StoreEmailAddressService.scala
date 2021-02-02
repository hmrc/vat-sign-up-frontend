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

package uk.gov.hmrc.vatsignupfrontend.services

import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.vatsignupfrontend.connectors.StoreEmailAddressConnector
import uk.gov.hmrc.vatsignupfrontend.httpparsers.StoreEmailAddressHttpParser.StoreEmailAddressResponse

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class StoreEmailAddressService @Inject()(val storeEmailAddressConnector: StoreEmailAddressConnector) {

  def storeEmailAddress(vatNumber: String, email: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[StoreEmailAddressResponse] =
    storeEmailAddressConnector.storeEmailAddress(vatNumber, email)

  def storeTransactionEmailAddress(vatNumber: String, transactionEmail: String)
                                  (implicit hc: HeaderCarrier, ec: ExecutionContext): Future[StoreEmailAddressResponse] =
    storeEmailAddressConnector.storeTransactionEmailAddress(vatNumber, transactionEmail)

  def storeTransactionEmailAddress(vatNumber: String, transactionEmail: String, passcode: String)
                                  (implicit hc: HeaderCarrier, ec: ExecutionContext): Future[StoreEmailAddressResponse] =
    storeEmailAddressConnector.storeTransactionEmailAddress(vatNumber, transactionEmail, passcode)

}