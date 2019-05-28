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

package uk.gov.hmrc.vatsignupfrontend.forms

import play.api.data.Forms._
import play.api.data.format.Formatter
import play.api.data.{Form, FormError}
import uk.gov.hmrc.vatsignupfrontend.models._

object OtherBusinessEntityForm {

  val businessEntity: String = "business-entity"

  val vatGroup: String = "vat-group"

  val division: String = "division"

  val unincorporatedAssociation: String = "unincorporated-association"

  val trust: String = "trust"

  val registeredSociety = "registered-society"

  val charity: String = "charity"

  val governmentOrganisation = "government-organisation"

  val businessEntityError: String = "error.business-entity-other"


  private def formatter: Formatter[BusinessEntity] = new Formatter[BusinessEntity] {
    override def bind(key: String, data: Map[String, String]): Either[Seq[FormError], BusinessEntity] = {
      data.get(key) match {
        case Some(`vatGroup`) => Right(VatGroup)
        case Some(`division`) => Right(Division)
        case Some(`unincorporatedAssociation`) => Right(UnincorporatedAssociation)
        case Some(`trust`) => Right(Trust)
        case Some(`registeredSociety`) => Right(RegisteredSociety)
        case Some(`charity`) => Right(Charity)
        case Some(`governmentOrganisation`) => Right(GovernmentOrganisation)
        case _ => Left(Seq(FormError(key, businessEntityError)))
      }
    }

    override def unbind(key: String, value: BusinessEntity): Map[String, String] = {
      val stringValue = value match {
        case VatGroup => vatGroup
        case Division => division
        case UnincorporatedAssociation => unincorporatedAssociation
        case Trust => trust
        case RegisteredSociety => registeredSociety
        case Charity => charity
        case GovernmentOrganisation => governmentOrganisation
      }

      Map(key -> stringValue)
    }
  }

  def businessEntityForm: Form[BusinessEntity] = Form(
    single(
      businessEntity -> of(formatter)
    )
  )
}
