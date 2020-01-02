/*
 * Copyright 2020 HM Revenue & Customs
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
import org.scalatest.{BeforeAndAfterEach, Suite}
import org.scalatest.mockito.MockitoSugar
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.vatsignupfrontend.connectors.VatNumberEligibilityConnector
import uk.gov.hmrc.vatsignupfrontend.httpparsers.VatNumberEligibilityHttpParser.VatNumberEligibilityResponse
import scala.concurrent._

trait MockVatNumberEligibilityConnector extends MockitoSugar with BeforeAndAfterEach {
  this: Suite =>

  val mockVatNumberEligibilityConnector = mock[VatNumberEligibilityConnector]

  override def beforeEach(): Unit = {
    super.beforeEach()
    reset(mockVatNumberEligibilityConnector)
  }

  def mockVatNumberEligibility(vatNumber: String)(response: Future[VatNumberEligibilityResponse]): Unit =
    when(mockVatNumberEligibilityConnector.checkVatNumberEligibility(
      ArgumentMatchers.eq(vatNumber)
    )(
      ArgumentMatchers.any[HeaderCarrier],
      ArgumentMatchers.any[ExecutionContext]
    )) thenReturn response

}
