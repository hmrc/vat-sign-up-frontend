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

package uk.gov.hmrc.vatsignupfrontend.assets

object MessageLookup {

  object ServiceName {
    val agentSuffix = " - Use software to submit your client's VAT Returns - GOV.UK"
    val principalSuffix = " - Use software to submit your VAT Returns - GOV.UK"
  }

  object Base {
    val continue = "Continue"
    val continueToSignUp = "Continue to sign up"
    val confirmAndContinue = "Confirm and continue"
    val confirm = "Confirm"
    val acceptAndContinue = "Accept and continue"
    val acceptAndSend = "Accept and send"
    val agreeAndContinue = "Agree and continue"
    val agree = "Agree"
    val submit = "Submit"
    val update = "Update"
    val signOut = "Sign out"
    val signUp = "Sign up"
    val goBack = "Go back"
    val signUpClient = "Sign up your client"
    val signUpAnotherClient = "Sign up another client"
    val signUpWithAnotherId = "Sign in with another user ID"
    val day = "Day"
    val month = "Month"
    val year = "Year"
    val change = "Change"
    val tryAgain = "Try again"
    val startNow = "Start now"
    val yes = "Yes"
    val no = "No"
    val errPrefix = "Error:"
  }

  object ErrorMessage {
    val invalidVatNumber = "Please enter a valid vat registration number"
    val invalidCompanyNumber = "Please enter a valid company number"
  }

  object NotEnrolledToAS {
    val heading: String = "You can't use this service yet"
    val title = heading + ServiceName.agentSuffix
    val line1 = "To use this service, you need to set up an agent services account."
  }

  object NoAgentClientRelationship {
    val heading: String = "You can't sign up this client yet"
    val title = heading + ServiceName.agentSuffix
    val line1 = "To use this service, your client needs to authorise you as their agent."
  }

  object CannotUseServiceYet {
    val heading: String = "Your client's business is not eligible at this time"
    val title = heading + ServiceName.agentSuffix
    val line1 = "Try again later."
  }

  object CaptureVatNumber {
    val heading: String = "What is your client's VAT number?"
    val title = heading + ServiceName.agentSuffix
    val description = "This is the 9-digit number they received when they registered for VAT."
    val hint = "For example, 123456789"
  }

  object ConfirmVatNumber {
    val heading: String = "Confirm your client's VAT number"
    val title = heading + ServiceName.agentSuffix
    val link = "Change VAT number"
  }

  object CaptureCompanyNumber {
    val heading: String = "What is your client's company registration number?"
    val title = heading + ServiceName.agentSuffix
    val link = "Companies House website (opens in a new window or tab)"
    val line1 = s"You can find the company registration number on the $link."
  }

  object AgentUsingPrincipalJourney {
    val heading: String = "You have logged in with the wrong type of account"
    val title = heading + ServiceName.principalSuffix
    val line1 = "If you are an agent signing up your client to Making Tax Digital for VAT you need to use the agent service."
  }

  object PrincipalCompanyNameNotFound {
    val heading: String = "We could not confirm your company"
    val title = heading + ServiceName.principalSuffix
    val line1 = "The company number you entered is not on our system."
  }

  object PrincipalSignInDifferentDetails {
    val heading: String = "You need to sign in with different details"
    val title = heading + ServiceName.principalSuffix
    val line1 = "The details you have used to sign in are for a different sole trader. You need to sign out, then sign in with the correct Government Gateway details."
    val linkText = "change your answer"
    val line2 = s"You can $linkText."
  }

  object PrincipalIncorrectEnrolmentVatNumber {
    val heading: String = "You need to sign in with different details"
    val title = heading + ServiceName.principalSuffix
    val line1 = "The VAT number you entered is for a different business that has its own Government Gateway account. Once you've signed out, you need to sign in with the correct details."
    val linkText = "change your answer"
    val line2 = s"If you've entered the wrong VAT number, you can $linkText."
  }

  object CaptureBusinessEntity {
    val agentHeading: String = "What type of business or group is your client?"
    val agentTitle = agentHeading + ServiceName.agentSuffix
    val principalHeading: String = "What type of business or group are you signing up?"
    val principalTitle = principalHeading + ServiceName.principalSuffix
    val radioSoleTrader: String = "Sole trader"
    val radioLimitedCompany: String = "Limited company"
    val radioGeneralPartnership: String = "General partnership"
    val radioLimitedPartnership: String = "Limited partnership (including limited liability partnerships)"
    val radioVatGroup: String = "VAT group"
    val radioDivision: String = "Administrative Division"
    val radioUnincorporatedAssociation: String = "Unincorporated Association"
    val radioRegisteredSociety: String = "Registered Society (including community benefit societies and co-operative societies)"
    val radioTrust: String = "Trust"
    val radioCharity: String = "CIO (charity)"
    val radioGovernmentOrganisation: String = "Government organisations and public sector"
    val radioOther: String = "Other"
    val radioOrOther: String = "or Other"
  }

  object YesNo {
    val heading: String = "Do you have more than one VAT registered business?"
    val title = heading + ServiceName.principalSuffix
    val radioYes: String = "yes"
    val radioNo: String = "no"
  }

  object CaptureLastReturnMonthPeriod {
    val heading: String = "Select the last month of your latest VAT accounting period, submitted to HMRC"
    val title: String = "Select the last month of your latest VAT accounting period, submitted to HMRC" + ServiceName.principalSuffix
    val line: String = "You can find this out by logging into your online VAT account or you can find this on your VAT return."
    val subHeading: String = "Example 1"
    val subHeading2: String = "Example 2"
    val line2: String = "You submit your VAT Return every three months or quarterly. In the 'accounting period' January to March, the last month in that 'accounting period' is March. You must therefore select March."
    val line3: String = "If you submit your VAT Return monthly, the last accounting period you 'submitted for' was January. You must select January."
    val formhint1 = "Select the last month in your latest VAT accounting period."
    val months: List[String] = List("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  }

  object ClientDetails {
    val heading = "Enter your client's details"
    val title = heading + ServiceName.agentSuffix
    val line1 = "We will attempt to match these details against information we currently hold."
    val field1 = "First name"
    val field2 = "Last name"
    val field3 = "National Insurance number"
    val field4 = "Date of birth"
    val formhint1_line1 = "For example, 'QQ 12 34 56 C'."
    val formhint2 = "For example, 10 12 1990"
  }

  object ConfirmCompanyNumber {
    val heading: String = "Confirm your client's company number"
    val title = heading + ServiceName.agentSuffix
    val companyNumberHeading = "Company number"
    val link = "Change company number"
  }

  object AgreeCaptureEmail {
    val heading: String = "Agree for your client to get secure messages from HMRC"
    val title = heading + ServiceName.agentSuffix
    val insetText = "To join this trial, your client must agree to get secure messages instead of letters."
    val line1 = "When your client has a new message about VAT in their business tax account, we will send an email notification to let them know."
    val line2 = "Your client will need to sign in to their business tax account to read their secure messages."
  }

  object CaptureAgentEmail {
    val heading: String = "What is your email address?"
    val title = heading + ServiceName.agentSuffix
    val line1 = "We will only send you an email to let you know when you can report your client's VAT through compatible software. This might take up to 72 hours."
    val hint = "For example, yourname@example.com"
  }

  object CaptureClientEmail {
    val heading: String = "What is your client's email address?"
    val title = heading + ServiceName.agentSuffix
    val line1 = "We'll only use the email address to:"
    val bullet1 = "send a Direct Debit advance notice 10 working days before each payment is taken"
    val bullet2 = "send VAT notifications"
    val line2 = "If your client does not want to receive Direct Debit notices by email, they will need to cancel their Direct Debit."
    val hint = "For example, yourname@example.com"
  }

  object ConfirmEmail {
    val heading: String = "Check your client's email address"
    val title = heading + ServiceName.agentSuffix
    val emailHeading = "Email address"
    val link = "Change email address"
  }

  object ConfirmAgentEmail {
    val heading: String = "Confirm your email address"
    val title = heading + ServiceName.agentSuffix
    val link = "Change email address"
  }

  object SentClientEmail {
    val heading: String = "We have sent your client an email"
    val title = heading + ServiceName.agentSuffix

    def line1(email: String) = s"We have sent an email to $email. Your client needs to click on the link in the email to verify their email address."

    val line2 = "They must verify their email address to get secure message notifications and other VAT emails from HMRC."
  }

  object VerifyAgentEmail {
    val heading: String = "Verify your email address"
    val title = heading + ServiceName.agentSuffix

    def line1(email: String) = s"We have sent an email to $email. Click on the link in the email to verify your email address."

    val linkText1 = "change your email address"
    val line2 = s"You can $linkText1 if it is not correct."
    val accordionHeading = "I did not get an email"
    val linkText2 = "send it again"
    val accordionText = s"Check your junk folder. If it is not there we can $linkText2. If we send your email again, any previous links will stop working."
  }

  object AgentEmailVerified {
    val heading: String = "You have verified your email address"
    val title = heading + ServiceName.agentSuffix
    val line1 = "You need to continue to sign up your client. We will send you a confirmation message after you sign up this client."
  }

  object UseDifferentEmailAddress {
    val heading: String = "You need to enter a different email address"
    val title = heading + ServiceName.agentSuffix
    val line1 = "The email address you entered for your client is the same as your own email address."
  }

  object AgentConfirmCompany {
    val heading: String = "Confirm your client's company"
    val title = heading + ServiceName.agentSuffix
    val link = "Change company"
  }

  object AgentCompanyNameNotFound {
    val heading: String = "We could not confirm your client's company"
    val title = heading + ServiceName.agentSuffix
    val line1 = "The company number you entered is not on our system."
  }

  object ClientVerifiedEmail {
    val heading: String = "You've verified your email address"
    val title = heading + ServiceName.agentSuffix
    val line1 = "HM Revenue and Customs will contact you by email instead of sending letters."
    val link = "Return to GOV.UK"
  }

  object ConfirmClient {
    val heading = "Confirm your client's details"
    val title = heading + ServiceName.agentSuffix
    val firstName = "First name"
    val lastName = "Last name"
    val nino = "National Insurance number"
    val dob = "Date of birth"
  }

  object ConfirmNino {
    val heading = "Confirm your client's details"
    val title = heading + ServiceName.agentSuffix
    val nino = "What is your client's National Insurance number?"
    val businessEntity = "What type of business or group is your client?"
  }

  object FailedClientMatching {
    val heading: String = "There's a problem"
    val title = heading + ServiceName.agentSuffix
    val description = "The details you've entered are not on our system."
  }

  object ClientAlreadySignedUp {
    val heading: String = "Your client has already signed up"
    val title = heading + ServiceName.agentSuffix
    val line1 = "This client's details are already in use."
  }

  object Terms {
    val heading: String = "Terms of participation"
    val title = heading + ServiceName.agentSuffix
    val line1 = "By taking part in this trial, you agree that either you or your client will:"
    val bullet1 = "use relevant software to record your client's sales and purchases, then to submit their VAT Returns"
    val bullet2 = "submit each VAT Return within one calendar month and 7 days from the end of their accounting period"
    val bullet3 = "tell HMRC if your client stops trading and then submit their final VAT Return"
    val bullet4 = "tell HMRC if your client wants to leave this trial"
    val line2 = "These terms are not contractual and your client can stop taking part in this trial at any time."
  }

  object AgentInformationReceived {
    val heading: String = "We have received your client's information"
    val title = heading + ServiceName.agentSuffix

    object Section1 {
      val heading = "What happens next"
      val line1 = "We will let you know when you can report your client's VAT through compatible software. This usually takes 72 hours."
      val bullet1 = "Choose your software (opens in a new window or tab). If your client will also use software, it is important they choose a package that can interact with yours."
      val bullet2 = "Allow your software to submit VAT Returns to HMRC. You might need to sign in with your Government Gateway details."
      val bullet3 = "Use software to record your client's sales and purchases."
      val bullet4 = "Submit your client's VAT Returns before their deadlines."
      val line2 = "Your client can view their VAT Return deadlines in their accounting software or business tax account."
    }

  }

  object YourVatNumber {
    val heading: String = "Confirm you want to use this VAT number to sign up"
    val title = heading + ServiceName.principalSuffix
    val vatNumberHeading = "VAT number"
    val link = "I want to use a different VAT number"
  }

  object VatNumber {
    val heading: String = "What is your VAT number?"
    val title = heading + ServiceName.principalSuffix
    val line1 = "This is the 9-digit number on your VAT registration certificate."
  }

  object CouldNotConfirmVatNumber {
    val heading: String = "We could not confirm your client's VAT number"
    val title = heading + ServiceName.agentSuffix
    val line1 = "The information you provided does not match the details on our system."
    val tryAgain = "Try again"
  }

  object AgentCouldNotFindPartnership {
    val heading: String = "We could not confirm your client's company number for their Limited Partnership"
    val title = heading + ServiceName.agentSuffix
    val line1 = "The company number is not on our system."
  }

  object AgentCouldNotConfirmPartnership {
    val heading: String = "We could not confirm your client's Partnership"
    val title = heading + ServiceName.agentSuffix
    val line1 = "The information you provided does not match or is incorrect."
  }

  object VatRegistrationDate {
    val heading = "When did you become VAT registered?"
    val title = heading + ServiceName.principalSuffix
    val line1 = "You can find this date on your VAT registration certificate."
    val formhint1 = "For example, 6 4 2017"
  }

  object PrincipalCouldNotConfirmVatNumber {
    val heading: String = "We could not confirm your VAT number"
    val title = heading + ServiceName.principalSuffix
    val line1 = "The VAT number you entered is not on our system."
  }

  object SignInWithDifferentDetails {
    val heading: String = "You need to sign in with different details"
    val title = heading + ServiceName.principalSuffix
    val line1 = "You need to sign in with the Government Gateway details for the business you want to sign up."
  }

  object SignInWithDifferentDetailsPartnership {
    val heading: String = "You need to sign in with different details"
    val title = heading + ServiceName.principalSuffix
    val line1 = "You must use a Government Gateway user ID that is linked to the Unique Taxpayer Reference for your partnership."
  }

  object PrincipalPlaceOfBusiness {
    val heading = "What is the postcode where your business is registered for VAT?"
    val title = heading + ServiceName.principalSuffix
    val label = "UK postcode"
    val hint = "For example, AB1 2YZ"
  }

  object PrincipalGuidance {
    val heading: String = "Use software to submit your VAT Returns"
    val title = ServiceName.principalSuffix.substring(3)
    val line1 = "HM Revenue and Customs (HMRC) are changing the way you submit your VAT Returns."
    val line2 = "From April 2019, VAT registered businesses with a turnover above £85,000 must use relevant third party software to submit their VAT Returns."
    val line3 = "If you're a sole trader or a director of a limited company you can try this new way of reporting now, instead of waiting until April 2019."
    val line4 = "You'll need to sign up for this new service, even if you already use software to submit your VAT Returns."

    object Section1 {
      val heading = "How it works"
      val line1 = "To sign up for this service, you need to use software to record your purchases and sales. If you don't already record in this way, submit your next VAT Return on GOV.UK or by post before you sign up. This will give you more time to complete your first return using software."
      val number1 = "You need to choose relevant third party software. If you already use software, check with your supplier to see if you can use it with this service."
      val number2 = "Sign up for this service and agree to get emails instead of letters."
      val number3 = "Allow your software to submit VAT Returns to HMRC. You might need to sign in with your Government Gateway details."
      val number4 = "Use your software to record your purchases and sales, then to submit your VAT Returns. We'll email you to let you know when to send a return."
      val line2 = "You can also choose to:"
      val bullet1 = "pay throughout the year, if it helps you manage your income"
      val bullet2 = "get your accountant to send your VAT Returns"
      val bullet3 = "view your VAT Return deadlines in your business tax account"
    }

    object Section2 {
      val heading = "Sign up"
      val line1 = "To sign up you'll need the Government Gateway details you got when you registered for VAT."
      val line2 = "If you haven't submitted a VAT Return online before, then you'll also need your VAT registration certificate."
    }

  }

  object PrincipalCheckYourAnswers {
    val heading = "Check your answers"
    val title = heading + ServiceName.principalSuffix
    val subHeading = "You've told us"
    val yourVatNumber = "What is your VAT number?"
    val vatRegistrationDate = "What is your VAT registration date?"
    val businessPostCode = "What is your business postcode?"
    val businessEntity = "What type of business are you registered as?"
    val box5Figure = "Your VAT return total or Box 5 figure"
    val lastReturnMonth = "The last month of your latest VAT accounting period"
    val previousVatReturn = "Are you currently submitting VAT returns?"
  }

  object PrincipalNoCtEnrolmentSummary {
    val heading = "Check your answers"
    val title = heading + ServiceName.principalSuffix
    val subHeading = "You've told us"
    val companyNumber = "What is your company number?"
    val companyUtr = "What is your company Unique Taxpayer Reference?"
    val businessEntity = "What is your business type?"
  }

  object PrincipalCannotUseServiceYet {
    val heading: String = "Your business is not eligible at this time"
    val title = heading + ServiceName.principalSuffix
    val line1 = "Try again later."
  }

  object PrincipalCaptureEmail {
    val heading: String = "What is the business email address?"
    val title = heading + ServiceName.principalSuffix
    val line1 = "We will email you to confirm you've signed up your business to use software to submit its VAT Returns (this can take up to 72 hours)."
    val line2 = "We'll email you a Direct Debit advance notice 10 working days before each payment is taken."
    val line3 = "If you do not want to receive Direct Debit notices by email, you will need to cancel your Direct Debit."
    val hint = "For example, yourname@example.com"
  }

  object PrincipalAgreeCaptureEmail {
    val heading: String = "Agree to get emails instead of letters"
    val title = heading + ServiceName.principalSuffix
    val line1 = "When you have a new message about VAT in your HMRC account, we'll send you an email to let you know."
    val line2 = "You'll need to sign in to your account to read the message."
  }

  object PrincipalConfirmEmail {
    val heading: String = "Check your email address"
    val title = heading + ServiceName.principalSuffix
    val emailHeading = "Email address"
    val link = "Change email address"
  }

  object PrincipalVerifyEmail {
    val heading: String = "Verify your email address"
    val title = heading + ServiceName.principalSuffix

    def line1(email: String) = s"We've sent an email to $email. Click on the link in the email to verify your email address."

    val linkText1 = "change your email address"
    val line2 = s"You can $linkText1 if it is not correct."
    val accordionHeading = "I did not get an email"
    val linkText2 = "send it again"
    val accordionText = s"Check your junk folder. If it's not there we can $linkText2. If we send your email again, any previous links will stop working."
  }

  object PrincipalEmailVerified {
    val heading: String = "You've verified your email address"
    val title = heading + ServiceName.principalSuffix
    val line1 = "You'll now receive messages and email notifications from HMRC."
  }

  object YourDetails {
    val heading: String = "Enter your details"
    val title = heading + ServiceName.principalSuffix
    val line1 = "We will attempt to match these details against information we currently hold."
    val field1 = "First name"
    val field2 = "Last name"
    val field3 = "National Insurance number"
    val field4 = "Date of birth"
    val formhint1_line1 = "For example, 'QQ 12 34 56 C'."
    val formhint2 = "For example, 10 12 1990"
  }

  object CaptureNino {
    val heading: String = "What is your National Insurance number?"
    val title = heading + ServiceName.principalSuffix
    val formHint = "It's on your National Insurance card, benefit letter, payslip or P60. For example 'QQ 12 34 56 C'."
  }

  object AgentCaptureNino {
    val heading: String = "What is your client's National Insurance number?"
    val title = heading + ServiceName.agentSuffix
    val formHint = "It's on your client's National Insurance card, benefit letter, payslip or P60. For example, 'QQ 12 34 56 C'."
  }

  object ConfirmDetails {
    val heading = "Confirm your details"
    val title = heading + ServiceName.principalSuffix
    val subHeading = "You've told us"
    val firstName = "First name"
    val lastName = "Last name"
    val nino = "National Insurance number"
    val dob = "Date of birth"
  }

  object PrincipalConfirmNino {
    val heading: String = "Check your answers"
    val title = heading + ServiceName.principalSuffix
    val subHeading = "You've told us"
    val businessEntity = "What is your business type?"
    val nino = "What is your National Insurance number?"
  }

  object PrincipalConfirmYourDetails {
    val heading: String = "Confirm your details"
    val title = heading + ServiceName.principalSuffix
    val firstName = "First name"
    val lastName = "Last name"
    val dob = "Date of birth"
    val nino = "National Insurance number"
    val link = "Change details"
  }

  object FailedIdentityVerification {
    val heading: String = "We couldn't confirm your details"
    val title = heading + ServiceName.principalSuffix
    val line1 = "The information you provided doesn't match the details on our system."
    val tryAgain = "Try again"
  }

  object CouldNotConfirmBusiness {
    val heading: String = "We could not confirm your business"
    val title = heading + ServiceName.principalSuffix
    val line1 = "The information you provided does not match the details we have about your business."
    val tryAgain = "Try again"
  }

  object PartnershipAsCompanyError {
    val heading: String = "We could not confirm your business"
    val title = heading + ServiceName.principalSuffix
    val line1 = "The company number you entered is not for a limited company."
    val tryAgain = "Try again"
  }

  object BTACannotConfirmBusiness {
    val heading: String = "We cannot confirm the business"
    val title = heading + ServiceName.principalSuffix
    val line1 = "The information you provided does not match the details we have about the business."
    val tryAgain = "Try again"
  }

  object IdentityVerificationSuccess {
    val heading: String = "We've confirmed your identity"
    val title = heading + ServiceName.principalSuffix
    val line1 = "You can now sign up for this service."
  }

  object FailedMatching {
    val heading: String = "We couldn't confirm your details"
    val title = heading + ServiceName.principalSuffix
    val line1 = "The information you provided doesn't match the details on our system."
    val tryAgain = "Try again"
  }

  object PrincipalCaptureCompanyNumber {
    val heading: String = "What is your company registration number?"
    val title = heading + ServiceName.principalSuffix
    val link = "search for your company number (opens in a new window or tab)"
    val line1 = s"You can $link on Companies House."
  }

  object PrincipalConfirmCompanyNumber {
    val heading: String = "Confirm your company number"
    val title = heading + ServiceName.principalSuffix
    val companyNumberHeading = "Company number"
    val link = "Change company number"
  }

  object PrincipalConfirmCompany {
    val heading: String = "Confirm your company"
    val title = heading + ServiceName.principalSuffix
    val link = "Change company"
  }

  object PrincipalConfirmRegisteredSociety {
    val heading: String = "Confirm your registered society"
    val title = heading + ServiceName.principalSuffix
    val link = "Change company"
  }

  object PrincipalRegisteredSocietyCompanyNameNotFound {
    val heading: String = "We could not confirm your registered society"
    val title = heading + ServiceName.principalSuffix
    val line1 = "The information you provided does not match the details we have for your registered society."
  }

  object PrincipalCaptureCompanyUtr {
    val heading: String = "What is your company's Unique Taxpayer Reference number?"
    val title = heading + ServiceName.principalSuffix
    val line1 = "You can find it on letters about Corporation Tax from HMRC. It is 10-digits and is sometimes called a UTR."
  }

  object PrincipalTerms {
    val heading: String = "Terms of participation"
    val title = heading + ServiceName.principalSuffix
    val line1 = "By taking part in this trial, you agree to:"
    val bullet1 = "use relevant software to record your sales and purchases, then to submit your VAT Returns"
    val bullet2 = "submit each VAT Return within one calendar month and 7 days from the end of your accounting period"
    val bullet3 = "authorise any third party you use (such as your accountant) and have responsibility for any information they give to HMRC on your behalf"
    val bullet4 = "tell HMRC if you stop trading and then submit your final VAT Return"
    val bullet5 = "tell HMRC if you want to leave this trial"
    val line2 = "These terms are not contractual and you can stop taking part in the trial at any time."
  }

  object PrincipalInformationReceived {

    val heading: String = "We have received your information"
    val title = heading + ServiceName.principalSuffix

    object Section {
      val link = "/guidance/find-software-thats-compatible-with-making-tax-digital-for-vat"
      val linkText = "software that works with Making Tax Digital for VAT (opens in a new window or tab)"

      val heading = "What happens next"
      val line = "We will send you an email within 72 hours to confirm. Do not submit a VAT Return during this time."
      val bullet1 = s"You must use ${linkText}."
      val bullet2 = "You must only use the new way of submitting your VAT Return through software with Making Tax Digital for VAT. Do not use the old way of sending your VAT Return."
      val bullet3 = "Check that your software is connected to send VAT Returns to HMRC directly. You might need to sign in with your Government Gateway user ID."
      val bullet4 = "Use the software to record all your sales and purchases."
      val bullet5 = "After April, keep your business details up to date using HMRC services (this is also called business tax account). Do not use the VAT online services."
    }

  }

  object PrincipalAlreadySignedUp {
    val heading: String = "You have already signed up"
    val title = heading + ServiceName.principalSuffix
    val line = "Your sign in details are already in use."
  }

  object AgentGuidance {
    val heading = "Use software to submit your client's VAT Returns"
    val title = ServiceName.agentSuffix.substring(3)
    val line1 = "From April 2019, sole trader businesses and limited companies with a turnover above £85,000 must submit their VAT Returns using software that supports Making Tax Digital."
    val line2 = "If your client is VAT registered they can choose to sign up for this new reporting method now. To help us test this new way of working out your tax, they'll need to:"
    val bullet1 = "use software that supports Making Tax Digital to record their purchases and sales"
    val bullet2 = "submit their VAT Returns using their software"
    val bullet3 = "agree to get emails instead of letters"

    val subHeadingBeforeYouStart = "Before you start"
    val beforeYouStartLine1 = "To sign your clients up to this service you'll need to:"
    val beforeYouStartBullet1 = "set up an agent services account"
    val beforeYouStartBullet2 = "add clients to your account"

    val subHeadingSignUpYourClients = "Sign up your clients"
    val signUpYourClientsLine1 = "Once you've set up your agent services account, you can sign up your clients for this new way of reporting their tax. This will add clients to your account."
    val signUpYourClientsLine2 = "You can sign up your clients if they're either a sole trader or limited company. Your clients can submit their VAT Returns using software, or you can submit them on their behalf."

    val subHeadingSoftware = "Software"
    val softwareLine1 = "You need to choose software that supports Making Tax Digital. You can still use your existing software if it supports Making Tax Digital."
    val softwareLine2 = "If you and your client will use software, it's important they choose a software package that can interact with yours."

    val subHeadingGetHelp = "Get help"
    val getHelpLine1 = "You need to contact your software supplier if you need help using your software, for example to upload sales records or sending updates to HMRC."
    val getHelpLine2 = "If you have other questions about sending updates, contact HMRC."

  }

  object AssistantCredentialError {
    val heading = "You cannot use this service"
    val title = heading + ServiceName.principalSuffix
    val line1 = "You can only sign up this business if you are an administrator."
  }

  object SignUpCompleteClient {
    val heading = "You have added VAT to your business tax account"
    val title = heading + ServiceName.principalSuffix
    val line1 = "You can now use your business tax account to:"
    val bullet1 = "see what you owe"
    val bullet2 = "check your deadlines"
    val bullet3 = "pay your VAT"
    val bullet4 = "tell us about any changes to your business"
  }

  object ConfirmPartnershipUtr {
    val title = "Is this the right Unique Taxpayer Reference for the partnership?" + ServiceName.principalSuffix
    val headingGeneralPartnership = "Is this the right Unique Taxpayer Reference (UTR) for the partnership?"

    def headingLimitedPartnership(name: String) = s"Is this the right Unique Taxpayer Reference (UTR) for $name?"

    val line1 = "We hold the following information:"
    val line2 = "Unique Taxpayer Reference (UTR)"
  }

  object PrincipalCapturePartnershipCompanyNumber {
    val heading: String = "What is the partnership's company number?"
    val title = heading + ServiceName.principalSuffix
    val linkText = "search for your company number (opens in a new window or tab)"
    val line1 = s"You can $linkText on Companies House."
  }

  object PrincipalCouldNotConfirmPartnershipCompany {
    val heading: String = "We could not confirm your Limited Partnership's company number"
    val title = heading + ServiceName.principalSuffix
    val line1 = "The company number is not on our system."
  }

  object PrincipalCouldNotConfirmPartnershipKnownFacts {
    val heading: String = "We could not confirm your Partnership"
    val title = heading + ServiceName.principalSuffix
    val line1 = "The information you provided does not match or is incorrect."
  }

  object PrincipalCouldNotConfirmLimitedPartnership {
    val heading: String = "We could not confirm your Limited Partnership"
    val title = heading + ServiceName.principalSuffix
    val line1 = "The information you provided is not for a Limited Partnership."
  }

  object PrincipalCaptureRegisteredSocietyUtr {
    val heading: String = "What is your registered society's Unique Taxpayer Reference number?"
    val title = heading + ServiceName.principalSuffix
    val line1 = "You can find it on letters about Corporation Tax from HMRC. It is 10-digits and is sometimes called a UTR."
  }

  object SignUpAfterThisDate {
    val heading = "You must come back later to sign up"
    val title = heading + ServiceName.principalSuffix
    val linkText = "Making Tax Digital guidance"
    val line1 = "We're unable to set you up for Making Tax Digital for VAT at present. Either this is because:"
    val line2 = "If you submit monthly returns and are now unable to sign up in time to file your May return by the deadline do not worry. Please submit your return using the old method and come back after the date below to sign up in time to submit your June return. We will not charge a penalty in this instance."

    def line3(date: String) = s"You'll be able to sign up for this service after $date."

    val line4 = s"For the latest information and rules to follow go to $linkText"
    val bullet1 = "a Direct Debit payment is due shortly"
    val bullet2 = "you are very close to a busy filing period"
  }

  object SignUpBetweenTheseDates {
    val heading = "You must come back later to sign up"
    val title = heading + ServiceName.principalSuffix
    val linkText = "Making Tax Digital guidance"
    val line1 = "We're unable to set you up for Making Tax Digital for VAT at present. Either this is because:"
    val line2 = "If you submit monthly returns and are now unable to sign up in time to file your May return by the deadline do not worry. Please submit your return using the old method and come back after the date below to sign up in time to submit your June return. We will not charge a penalty in this instance."

    def line3(startDate: String, endDate: String) = s"You'll be able to sign up for this service between $startDate and $endDate."

    val line4 = s"For the latest information and rules to follow go to $linkText"
    val bullet1 = "a Direct Debit payment is due shortly"
    val bullet2 = "you are very close to a busy filing period"
  }

  object AgentSignUpAfterThisDate {
    val heading = "You must come back later to sign up"
    val title = heading + ServiceName.agentSuffix
    val linkText = "Making Tax Digital guidance"
    val line1 = "We're unable to set your client up for Making Tax Digital for VAT at present. Either this is because:"
    val line2 = "If you submit monthly returns for your client and are now unable to sign up in time to file the May return by the deadline do not worry. Please submit the return using the old method and come back after the date below to sign up in time to submit the June return. We will not charge a penalty in this instance."

    def line3(date: String) = s"You'll be able to sign up your client for this service after $date."

    val line4 = s"For the latest information and rules to follow go to $linkText"
    val bullet1 = "a Direct Debit payment is due shortly"
    val bullet2 = "you are very close to a busy filing period"
  }

  object AgentSignUpBetweenTheseDates {
    val heading = "You must come back later to sign up"
    val title = heading + ServiceName.agentSuffix
    val linkText = "Making Tax Digital guidance"
    val line1 = "We're unable to set your client up for Making Tax Digital for VAT at present. Either this is because:"
    val line2 = "If you submit monthly returns for your client and are now unable to sign up in time to file the May return by the deadline do not worry. Please submit the return using the old method and come back after the date below to sign up in time to submit the June return. We will not charge a penalty in this instance."

    def line3(startDate: String, endDate: String) = s"You'll be able to sign up your client for this service between $startDate and $endDate."

    val line4 = s"For the latest information and rules to follow go to $linkText"
    val bullet1 = "a Direct Debit payment is due shortly"
    val bullet2 = "you are very close to a busy filing period"
  }

  object AgentCapturePartnershipCompanyNumber {
    val heading: String = "What is the company number of your client's partnership?"
    val title = heading + ServiceName.agentSuffix
    val link = "check the Companies House register (opens in a new window or tab)"
    val line1 = s"If you are not sure, $link."
  }

  object AgentCompanyNameNotFoundLP {
    val heading: String = "We could not confirm your client's Limited Partnership"
    val title = heading + ServiceName.agentSuffix
    val line1 = "The information you provided is not for a Limited Partnership."
  }

  object AgentCapturePartnershipUtr {
    val heading: String = "What is your client's Partnership Unique Taxpayer Reference (UTR) number?"
    val title = heading + ServiceName.agentSuffix
    val line = "This is a 10 digit number. You can find it on letters to the partnership from HM Revenue & Customs."
  }

  object ConfirmPartnership {
    val heading = "Confirm the partnership"
    val title = heading + ServiceName.principalSuffix
    val link = "Change the partnership"
  }

  object AgentConfirmPartnership {
    val heading = "Confirm the partnership"
    val title = heading + ServiceName.agentSuffix
    val link = "Change the partnership"
  }

  object AgentPartnershipPostcode {
    val heading = "Where is your client's partnership registered for Self Assessment?"
    val title = heading + ServiceName.agentSuffix
    val label = "UK postcode"
  }

  object CapturePartnershipUtr {
    val heading = "What is the partnership's Unique Taxpayer Reference (UTR)?"
    val title = heading + ServiceName.principalSuffix
    val line1 = "This is a 10-digit number. You can find it on letters to the partnership from HM Revenue & Customs."
  }

  object PartnershipPrincipalPlaceOfBusiness {
    val heading = "Where is the partnership registered for Self Assessment?"
    val title = heading + ServiceName.principalSuffix
    val label = "UK postcode"
  }

  object PartnershipsCYA {
    val heading = "Check your answers"
    val title = heading + ServiceName.principalSuffix
    val businessEntity = "What type of business or group are you signing up?"
    val companyNumber = "What is the partnership's company number?"
    val companyUtr = "What is the partnership's Unique Taxpayer Reference?"
    val postCode = "Where is the partnership registered for Self Assessment?"
    val hasOptionalSautr = "Does your partnership have a Self Assessment Unique Taxpayer Reference (UTR) number?"
  }

  object AgentCheckYourAnswers {
    val heading = "Confirm your client's details"
    val title = heading + ServiceName.agentSuffix
    val yourUtr = "What is your client's Unique Taxpayer Reference (UTR)?"
    val yourCompanyNumber = "What is your client's company number?"
    val yourBusinessPostCode = "Where is your client's principal place of business?"
    val generalPartnership: String = "General partnership"
    val limitedPartnership: String = "Limited partnership (including limited liability partnerships)"
    val hasOptionalSautr: String = "Does your client's partnership have a Self Assessment Unique Taxpayer Reference (UTR) number?"
  }

  object VerifySoftwareError {
    val heading: String = "Making Tax Digital for VAT: verify your software"
    val title = heading + ServiceName.principalSuffix
    val line1 = "You must confirm with your provider that your accounting software is ready to submit your VAT Returns directly to HMRC."
    val line2 = "They will be able to tell you if your software works with Making Tax Digital for VAT to submit your VAT Returns through the software."
    val returnToGovUK = "Return to GOV.UK"
  }

  object HaveSoftware {
    val heading: String = "Do you have accounting software for managing your VAT records?"
    val title = heading + ServiceName.principalSuffix
    val radioYes: String = "yes"
    val radioNo: String = "no"
    val yes: String = "Yes, I have accounting software"
    val no: String = "No, I do not have accounting software"
  }

  object SoftwareReady {
    val heading: String = "Is your software ready to submit your Making Tax Digital for VAT Returns directly to HMRC?"
    val title = heading + ServiceName.principalSuffix
    val line1 = "Your software must work with Making Tax Digital for VAT."
    val line2 = "You must check with your provider that your accounting software is ready to submit your VAT Returns directly to HMRC."
    val radioYes: String = "yes"
    val radioNo: String = "no"
    val yes: String = "Yes, my software submits through Making Tax Digital for VAT"
    val no: String = "No, my software does not submit through Making Tax Digital for VAT"
  }

  object ChooseSoftwareError {
    val heading: String = "Making Tax Digital for VAT: software is needed"
    val title = heading + ServiceName.principalSuffix
    val link = "list of software providers (opens in a new window or tab)"
    val line1 = "You must choose accounting software that works with Making Tax Digital for VAT. You will need to submit your VAT Returns to HMRC through the software."
    val line2 = s"We have a $link that support this VAT Returns process."
    val returnToGovUK = "Return to GOV.UK"
  }

  object AgentConfirmRegisteredSociety {
    val heading: String = "Confirm your client's registered society"
    val title = heading + ServiceName.agentSuffix
    val link = "Change company"
  }

  object AgentRegisteredSocietyCompanyNameNotFound {
    val heading: String = "We could not confirm your client's registered society"
    val title = heading + ServiceName.agentSuffix
    val line1 = "The information you provided does not match the details we have for your client's registered society."
  }

  object MigrationInProgressError {
    val heading: String = "You have already signed up"
    val title = heading + ServiceName.principalSuffix
    val line = "Your Making Tax Digital for VAT account is being set up. You will receive an email within 72 hours."
  }

  object AgentMigrationInProgressError {
    val heading: String = "Your client is already signed up"
    val title = heading + ServiceName.agentSuffix
    val line = "Your client's Making Tax Digital for VAT account is being set up. They will receive an email within 72 hours."
  }

  object PrincipalCheckYourAnswersRegisteredSociety {
    val heading = "Check your answers"
    val title = heading + ServiceName.principalSuffix
    val subHeading = "You've told us"
    val businessEntity = "What is your business type?"
    val registeredSociety = "Registered Society"
    val companyNumber = "What is your company number?"
    val companyUtr = "What is your registered society's Unique Taxpayer Reference?"
  }

  object BtaBusinessAlreadySignedUp {
    val heading = "Your business is already signed up"
    val title = heading + ServiceName.principalSuffix
    val linkText = "find or recover your account."
    val linkId = "recoverAccount"
    val line1 = "Your business is set up for Making Tax Digital for VAT with a different Government Gateway user ID. Try signing in with another Government Gateway user ID."
    val line2 = s"If you have lost your details, $linkText"
  }

  object CtEnrolmentDetailsDoNotMatch {
    val heading: String = "Your details do not match"
    val title = heading + ServiceName.principalSuffix
    val line1 = "Your details do not match the company number you provided."
    val companyNumber = "Change your company number"
  }

  object PrincipalCannotSignUpAnotherAccount {
    val heading = "You have already signed up"
    val title = heading + ServiceName.principalSuffix
    val line1 = "Your Government Gateway user ID is already linked to Making Tax Digital for VAT."
    val line2 = "If you want to sign up another business to Making Tax Digital for VAT, you'll need to use another Government Gateway user ID."
    val line3 = "If you do not have a Government Gateway user ID for your business, you'll need to create a new account."
    val buttonText = "Create a Government Gateway user ID"
  }

  object CaptureBox5Figure {
    val heading = "Enter your VAT return total or Box 5 amount"
    val title = heading + ServiceName.principalSuffix
    val line = "You can find this amount in box number 5 on your latest VAT Return."
    val line2 = "The format of this number needs to be two decimal places, for example £123.00."
  }

  object CancelDirectDebit {
    val heading = "If you do not agree to Direct Debit terms and conditions"
    val title = heading + ServiceName.principalSuffix
    val line1 = "You will need to:"
    val bullet1 = "cancel your Direct Debit with your bank"
    val bullet2 = "wait 4 working days before trying to sign up again"
    val line2 = "You do not need to notify HMRC that you have cancelled your Direct Debit."
    val linkText = "Go back to Direct Debit terms and conditions"
    val linkId = "directDebitTerms"
    val link = "/direct-debit-terms-and-conditions"
    val buttonText = "Logout"
  }

  object PrincipalDirectDebitTermsAndConditions {
    val heading = "Direct Debit terms and conditions"
    val title = heading + ServiceName.principalSuffix
    val link1 = "Direct Debit terms and conditions (opens in a new window or tab)"
    val line = s"You will need to read and agree to the $link1 to continue."
    val link2 = "I do not agree"
  }

  object ReceiveEmailNotifications {
    val heading: String = "How should we contact the business about VAT?"
    val title = heading + ServiceName.principalSuffix
    val line1 = "We can email you when you have a new message about VAT in your HMRC account."
    val line2 = "We may still need to send you letters if this is the only service available or if the law requires us to do so."
    val radioDigital: String = "digital"
    val radioPaper: String = "paper"

    def radioButtonEmail(email: String) = s"Send emails to $email"

    val paper: String = "Send letters only"
    val error = "Select if you want to be contacted by email or letters only"
  }

  object AgentReceiveEmailNotifications {
    val heading: String = "How should we contact your client about VAT?"
    val title = heading + ServiceName.agentSuffix
    val line1 = "We can email your client when they have a new message about VAT in their HMRC account. Your client will be able to change the contact preference later."
    val line2 = "We may still need to send your client letters if this is the only service available or if the law requires us to do so."
    val digital = "Send emails only"
    val paper: String = "Send my client letters only"
    val error = "Select if you want to be contacted by email or letters only"
  }

  object PrincipalSendYourApplication {
    val heading: String = "Send your application"
    val title = heading + ServiceName.principalSuffix
    val line1 = "By submitting this notification you are confirming that, to the best of your knowledge, the details you are providing are correct."
  }

  object AgentSendYourApplication {
    val heading: String = "Send your application"
    val title = heading + ServiceName.agentSuffix
    val line1 = "By submitting this notification you are confirming that, to the best of your knowledge, the details you are providing are correct."
  }

  object PrincipalJointVentureOrProperty {
    val heading: String = "Are you either a Joint Venture or Property Partnership?"
    val title = heading + ServiceName.principalSuffix
  }

  object PrincipalDoYouHaveAUtr {
    val heading: String = "Does your partnership have a Self Assessment Unique Taxpayer Reference (UTR) number?"
    val title: String = heading + ServiceName.principalSuffix
    val line: String = "This is a 10-digit number. You can find it on letters to the partnership from HMRC."
  }

  object AgentDoesYourClientHaveAUtr {
    val heading: String = "Does your client's partnership have a Self Assessment Unique Taxpayer Reference (UTR) number?"
    val title: String = heading + ServiceName.agentSuffix
    val line: String = "This is a 10-digit number. You can find it on letters to the partnership from HMRC."
  }

  object PrincipalCheckYourAnswersFinal {
    val heading: String = "Check your answers before sending"
    val title: String = heading + ServiceName.principalSuffix
    val vat_number: String = "VAT Number"
    val business_entity: String = "Type of Business"
    val nino: String = "National Insurance Number"
    val partnership_utr: String = "Partnership UTR"
    val partnership_number: String = "Partnership Company Registration Number"
    val partnership_name: String = "Partnership Company Name"
    val registered_society_name: String = "Registered Society Company Name"
    val company_number: String = "Company Registration Number"
    val company_name: String = "Company Name"
    val email_address: String = "Business Email Address"
    val contact_preference: String = "How we contact you"
    val digital: String = "Emails"
    val letter: String = "Letters"
  }

}
