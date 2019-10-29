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

package uk.gov.hmrc.vatsignupfrontend.connectors.mocks

import org.mockito.ArgumentMatchers
import org.mockito.Mockito._
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{BeforeAndAfterEach, Suite}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.vatsignupfrontend.connectors.StoreMigratedVatNumberConnector
import uk.gov.hmrc.vatsignupfrontend.httpparsers.StoreMigratedVatNumberHttpParser.StoreMigratedVatNumberResponse
import uk.gov.hmrc.vatsignupfrontend.models.PostCode

import scala.concurrent.Future

trait MockStoreMigratedVatNumberConnector extends MockitoSugar with BeforeAndAfterEach {
  this: Suite =>

  override def beforeEach(): Unit = {
    super.beforeEach()
    reset(mockStoreMigratedVatNumberConnector)
  }

  val mockStoreMigratedVatNumberConnector: StoreMigratedVatNumberConnector = mock[StoreMigratedVatNumberConnector]

  def mockStoreMigratedVatNumber(vatNumber: String)(response: Future[StoreMigratedVatNumberResponse]): Unit =
    when(mockStoreMigratedVatNumberConnector.storeVatNumber(ArgumentMatchers.eq(vatNumber))(ArgumentMatchers.any[HeaderCarrier]))
      .thenReturn(response)

  def mockStoreMigratedVatNumber(vatNumber: String,
                                 registrationDate: String,
                                 optPostCode: Option[PostCode])(response: Future[StoreMigratedVatNumberResponse]): Unit =
    when(mockStoreMigratedVatNumberConnector.storeVatNumber(
      ArgumentMatchers.eq(vatNumber),
      ArgumentMatchers.eq(registrationDate),
      ArgumentMatchers.eq(optPostCode)
    )(ArgumentMatchers.any[HeaderCarrier]))
      .thenReturn(response)

}
