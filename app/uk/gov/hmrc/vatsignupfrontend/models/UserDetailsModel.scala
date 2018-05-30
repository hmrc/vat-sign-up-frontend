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

package uk.gov.hmrc.vatsignupfrontend.models

import java.time.format.{DateTimeFormatter, ResolverStyle}
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Json, OWrites, Reads}

case class UserDetailsModel(firstName: String,
                            lastName: String,
                            nino: String,
                            dateOfBirth: DateModel)


object UserDetailsModel {
  implicit val reads = Json.reads[UserDetailsModel]
  implicit val writes = Json.writes[UserDetailsModel]

  val matchingStubWrites = new OWrites[UserDetailsModel] {
    private val dobFormat = DateTimeFormatter.ofPattern("ddMMyyyy").withResolverStyle(ResolverStyle.STRICT)

    def writes(userDetails: UserDetailsModel) = Json.obj(
      "firstname" -> Json.obj("value" -> userDetails.firstName),
      "lastName"  -> Json.obj("value" -> userDetails.lastName),
      "nino"  -> Json.obj("value" -> userDetails.nino),
      "dob"  -> Json.obj("value" -> userDetails.dateOfBirth.toLocalDate.format(dobFormat))
    )
  }

  val citizenDetailsReads: Reads[UserDetailsModel] =
    ((JsPath \ "name" \ "current" \ "firstName").read[String] and
      (JsPath \ "name" \ "current" \ "lastName").read[String] and
      (JsPath \ "ids" \ "nino").read[String] and
      (JsPath \ "dateOfBirth").read[String].map(dob => DateModel.convertCitizenDetailsDate(dob))
      )(UserDetailsModel.apply _)

}