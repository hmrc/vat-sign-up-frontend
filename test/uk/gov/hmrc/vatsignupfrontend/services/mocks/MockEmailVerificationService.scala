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

package uk.gov.hmrc.vatsignupfrontend.services.mocks

import org.mockito.ArgumentMatchers
import org.scalatest.Suite
import org.scalatestplus.mockito.MockitoSugar
import uk.gov.hmrc.vatsignupfrontend.models.RequestEmailPasscodeResult
import uk.gov.hmrc.vatsignupfrontend.services.EmailVerificationService
import org.mockito.Mockito._
import org.mockito.stubbing.OngoingStubbing
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future

trait MockEmailVerificationService extends MockitoSugar {
  self: Suite =>

  val mockEmailVerificationService = mock[EmailVerificationService]

  def mockRequestEmailPasscode(email: String, language: String)
                              (response: Future[RequestEmailPasscodeResult]): OngoingStubbing[Future[RequestEmailPasscodeResult]] =
    when(mockEmailVerificationService.requestEmailVerificationPasscode(
      ArgumentMatchers.eq(email),
      ArgumentMatchers.eq(language)
    )(
      ArgumentMatchers.any[HeaderCarrier]
    )) thenReturn response

}
