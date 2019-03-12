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
import uk.gov.hmrc.vatsignupfrontend.models.{ContactPreference, Digital, Paper}

object ContactPreferencesForm {

  val contactPreference: String = "contact-preference"

  val digital: String = "digital"

  val paper: String = "paper"

  val agentContactPreferenceError: String = "error.agent.contact-preference"

  val principalContactPreferenceError: String = "error.principal.receive_email_notifications"

  private def formatter(isAgent: Boolean): Formatter[ContactPreference] = new Formatter[ContactPreference] {
    override def bind(key: String, data: Map[String, String]): Either[Seq[FormError], ContactPreference] = {
      data.get(key) match {
        case Some(`digital`) => Right(Digital)
        case Some(`paper`) => Right(Paper)
        case _ => Left(Seq(FormError(key, if (isAgent) agentContactPreferenceError else principalContactPreferenceError)))
      }
    }

    override def unbind(key: String, value: ContactPreference): Map[String, String] = {
      val stringValue = value match {
        case Digital => digital
        case Paper => paper
      }
      Map(key -> stringValue)
    }
  }

  def contactPreferencesForm(isAgent: Boolean): Form[ContactPreference] = Form(
    single(
      contactPreference -> of(formatter(isAgent = isAgent))
    )
  )
}
