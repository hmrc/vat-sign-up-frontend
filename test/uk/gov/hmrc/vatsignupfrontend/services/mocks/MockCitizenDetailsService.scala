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

package uk.gov.hmrc.vatsignupfrontend.services.mocks

import org.mockito.ArgumentMatchers
import org.mockito.Mockito._
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{BeforeAndAfterEach, Suite}
import uk.gov.hmrc.vatsignupfrontend.httpparsers.CitizenDetailsHttpParser.{CitizenDetailsResponse,
CitizenDetailsRetrievalFailureResponse, CitizenDetailsRetrievalSuccess, MoreThanOneCitizenMatched, NoCitizenRecord}
import uk.gov.hmrc.vatsignupfrontend.models.UserDetailsModel
import uk.gov.hmrc.vatsignupfrontend.services.CitizenDetailsService

import scala.concurrent.Future


trait MockCitizenDetailsService extends BeforeAndAfterEach with MockitoSugar {
  self: Suite =>

  val mockCitizenDetailsService: CitizenDetailsService = mock[CitizenDetailsService]

  override protected def beforeEach(): Unit = {
    super.beforeEach()
    reset(mockCitizenDetailsService)
  }

  private def mockCitizenDetails(sautr: String)(returnValue: Future[CitizenDetailsResponse]): Unit = {
    when(mockCitizenDetailsService.getCitizenDetails(
      ArgumentMatchers.eq(sautr)
    )(ArgumentMatchers.any(), ArgumentMatchers.any()))
      .thenReturn(returnValue)
  }

  def mockCitizenDetailsSuccess(sautr: String, userDetails: UserDetailsModel): Unit =
    mockCitizenDetails(sautr)(Future.successful(Right(CitizenDetailsRetrievalSuccess(userDetails))))

  def mockCitizenDetailsNotFound(sautr: String): Unit =
    mockCitizenDetails(sautr)(Future.successful(Left(NoCitizenRecord)))

  def mockCitizenDetailsMoreThanOne(sautr: String): Unit =
    mockCitizenDetails(sautr)(Future.successful(Left(MoreThanOneCitizenMatched)))

  def mockCitizenDetailsFailure(sautr: String): Unit =
    mockCitizenDetails(sautr)(Future.successful(Left(CitizenDetailsRetrievalFailureResponse(500))))

}
