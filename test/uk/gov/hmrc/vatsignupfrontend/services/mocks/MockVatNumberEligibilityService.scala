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

package uk.gov.hmrc.vatsignupfrontend.services.mocks

import org.mockito.ArgumentMatchers
import org.mockito.Mockito._
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{BeforeAndAfterEach, Suite}
import uk.gov.hmrc.vatsignupfrontend.httpparsers.VatNumberEligibilityHttpParser._
import uk.gov.hmrc.vatsignupfrontend.services.VatNumberEligibilityService

import scala.concurrent.Future


trait MockVatNumberEligibilityService extends BeforeAndAfterEach with MockitoSugar {
  self: Suite =>

  val mockVatNumberEligibilityService: VatNumberEligibilityService = mock[VatNumberEligibilityService]

  override protected def beforeEach(): Unit = {
    super.beforeEach()
    reset(mockVatNumberEligibilityService)
  }

  def mockVatNumberEligibility(vatNumber: String)(returnValue: Future[VatNumberEligibilityResponse]): Unit = {
    when(mockVatNumberEligibilityService.checkVatNumberEligibility(ArgumentMatchers.eq(vatNumber))(ArgumentMatchers.any(), ArgumentMatchers.any()))
      .thenReturn(returnValue)
  }


}
