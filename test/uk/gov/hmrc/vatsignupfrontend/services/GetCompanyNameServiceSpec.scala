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

package uk.gov.hmrc.vatsignupfrontend.services

import org.mockito.ArgumentMatchers
import org.mockito.Mockito._
import org.scalatest.mockito.MockitoSugar
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.test.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.connectors.GetCompanyNameConnector
import uk.gov.hmrc.vatsignupfrontend.forms.CompanyNumberForm
import uk.gov.hmrc.vatsignupfrontend.forms.validation.utils.Patterns.CompanyNumber._
import uk.gov.hmrc.vatsignupfrontend.helpers.TestConstants._
import uk.gov.hmrc.vatsignupfrontend.httpparsers.GetCompanyNameHttpParser.GetCompanyNameSuccess
import uk.gov.hmrc.vatsignupfrontend.models.companieshouse.NonPartnershipEntity

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Random

class GetCompanyNameServiceSpec extends UnitSpec with MockitoSugar {

  val mockConnector = mock[GetCompanyNameConnector]

  object TestStoreVatNumberService extends GetCompanyNameService(mockConnector)

  implicit val hc = HeaderCarrier()

  "getCompanyName" should {
    val result = Right(GetCompanyNameSuccess(testCompanyName, NonPartnershipEntity))

    "return the result of the connector" in {
      when(mockConnector.getCompanyName(ArgumentMatchers.eq(testCompanyNumber))(ArgumentMatchers.any()))
        .thenReturn(Future.successful(result))

      val r = TestStoreVatNumberService.getCompanyName(testCompanyNumber)

      // null pointer exception would have been thrown if the arguments weren't converted to the expected string format
      await(r) shouldBe result
    }
  }

  def validateCompanyNumber(companyNumber: String): Unit = {
    val validation = CompanyNumberForm.companyNumberForm(isAgent = false, isPartnership = false).bind(Map("companyNumber" -> companyNumber))
    validation.errors shouldBe Seq.empty
    validation.get shouldBe companyNumber
  }

}
