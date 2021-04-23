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

package uk.gov.hmrc.vatsignupfrontend.assets

object MessageLookup {

  object ServiceName {
    val agentSuffix = " - Use software to submit your client’s VAT Returns - GOV.UK"
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
    val heading: String = "You need to sign in with an agent services account"
    val title = heading + ServiceName.agentSuffix
    val line1 = "Sign in again with an agent services account to continue."
    val line2 = "If you do not have one, create an agent services account."
  }

  object NoAgentClientRelationship {
    val heading: String = "You can’t sign up this client yet"
    val title = heading + ServiceName.agentSuffix
    val line1 = "To use this service, your client needs to authorise you as their agent."
  }

  object CannotUseServiceYet {
    val heading: String = "Your client’s business is not eligible at this time"
    val title = heading + ServiceName.agentSuffix
    val line1 = "Try again later."
  }

  object CaptureVatNumber {
    val heading: String = "What is your client’s VAT number?"
    val title = heading + ServiceName.agentSuffix
    val description = "This is the 9-digit number they received when they registered for VAT."
    val hint = "For example, 123456789"
  }

  object ConfirmVatNumber {
    val heading: String = "Confirm your client’s VAT number"
    val title = heading + ServiceName.agentSuffix
    val link = "Change VAT number"
  }

  object CaptureCompanyNumber {
    val heading: String = "What is your client’s company registration number?"
    val title = heading + ServiceName.agentSuffix
    val link = "Companies House website (opens in a new window or tab)"
    val line1 = s"You can find the company registration number on the $link."
  }

  object PrincipalDissolvedCompany {
    def heading(companyName: String): String = s"$companyName is not eligible to be signed up"
    def title(companyName: String): String = heading(companyName) + ServiceName.principalSuffix
    val paragraph1: String = "The number you’ve entered is for a company that has been dissolved or closed."
    val paragraph2: String = "Try again using the most recent company number sent from Companies House."
    val confirm: String = "Enter a different number"
  }

  object AgentDissolvedCompany {
    def heading(companyName: String): String = s"$companyName is not eligible to be signed up"
    def title(companyName: String): String = heading(companyName) + ServiceName.agentSuffix
    val paragraph1: String = "The number you’ve entered is for a company that has been dissolved or closed."
    val paragraph2: String = "Try again using the most recent company number sent from Companies House."
    val confirm: String = "Enter a different number"
  }

  object PrincipalDeregisteredEnrolled {
    val heading: String = "You cannot sign up with this VAT registration number"
    val title: String = heading + ServiceName.principalSuffix
    def paragraph1(vatNumber: String): String = s"The user ID and password you have entered is for a business which has been deregistered from paying VAT. The VAT registration number is: $vatNumber"
    val paragraph2: String = "You can either:"
    val bullet1: String = "sign out and sign back in with a different user ID and password, if you have another VAT registration number"
    val link: String = "VAT registration service (opens in a new window or tab)"
    val bullet2: String = s"register for VAT using the $link"
  }

  object PrincipalDeregisteredUnenrolled {
    val heading: String = "You cannot sign up with this VAT registration number"
    val title: String = heading + ServiceName.principalSuffix
    val paragraph1: String = "The VAT registration number you have entered is for a business which has deregistered from paying VAT. If you have another VAT registration number, enter that instead."
    val link: String = "VAT registration service (opens in a new window or tab)"
    val paragraph2: String = s"Alternatively, you can register for VAT using the $link."
    val another_number: String = "Enter another VAT number"
  }


  object AgentDeregistered {
    val heading: String = "You cannot sign up with this VAT registration number"
    val title: String = heading + ServiceName.agentSuffix
    val paragraph1: String = "The VAT registration number you have entered is for a business which has deregistered from paying VAT. If you have another VAT registration number, enter that instead."
    val link: String = "VAT registration service (opens in a new window or tab)"
    val paragraph2: String = s"Alternatively, you can register for VAT using the $link."
    val another_number: String = "Enter another VAT number"
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
    val line1 = "The VAT number you entered is for a different business that has its own Government Gateway user ID. Once you’ve signed out, you need to sign in with the correct details."
    val linkText = "change your answer"
    val line2 = s"If you’ve entered the wrong VAT number, you can $linkText."
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
    val radioVatGroup: String = s"VAT group A group of related companies (where one is under the control of the others) registering to submit a VAT return together. For example, a parent company that has a subsidiary or subsidiaries."
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
    val heading: String = "Select the last month of your latest VAT accounting period"
    val title: String = heading + ServiceName.principalSuffix
    val line: String = "You can find this by signing into your online VAT account. You can also find it in your latest VAT Return submitted to HMRC."
    val subHeading: String = "Latest VAT accounting period, example 1"
    val subHeading2: String = "Latest VAT accounting period, example 2"
    val line2: String = "You submit your VAT Return quarterly (every three months). In the ’accounting period’ January to March, the last month in that ’accounting period’ is March. You must therefore select March."
    val line3: String = "If you submit your VAT Return monthly, the last accounting period you ’submitted for’ was January. You must select January."
    val formhint1 = "Select the last month in your latest VAT accounting period."
    val months: List[String] = List("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  }

  object ClientDetails {
    val heading = "Enter your client’s details"
    val title = heading + ServiceName.agentSuffix
    val line1 = "We will attempt to match these details against information we currently hold."
    val field1 = "First name"
    val field2 = "Last name"
    val field3 = "National Insurance number"
    val field4 = "Date of birth"
    val formhint1_line1 = "For example, ’QQ 12 34 56 C’."
    val formhint2 = "For example, 10 12 1990"
  }

  object ConfirmCompanyNumber {
    val heading: String = "Confirm your client’s company number"
    val title = heading + ServiceName.agentSuffix
    val companyNumberHeading = "Company number"
    val link = "Change company number"
  }

  object CaptureAgentEmail {
    val heading: String = "What is your email address?"
    val title = heading + ServiceName.agentSuffix
    val line1 = "We will only send you an email to let you know when you can report your client’s VAT through compatible software. This might take up to 72 hours."
    val hint = "For example, yourname@example.com"
  }

  object CaptureClientEmail {
    val heading: String = "What is your client’s email address?"
    val title = heading + ServiceName.agentSuffix
    val hasDirectDebit = "We will send a Direct Debit advance notice 10 working days before each payment is taken. If your client does not want to receive Direct Debit notices by email, they will need to cancel their Direct Debit."
    val noDirectDebit = "HMRC uses your client’s email address to provide them with a range of digital services and communications."
    val link = "HMRC Privacy Notice (opens in a new window or tab)"
    val disclaimer = s"Full details of how we use your information are in the $link."
    val hint = "For example, yourname@example.com"
  }

  object ConfirmEmail {
    val heading: String = "Check your client’s email address"
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

    def line1(email: String) = s"We’ve sent an email to $email. Click on the link in the email to verify the business email address. You must confirm your email address to continue the sign-up journey. Do not sign out."

    val line2 = "Use the same device (for example computer, tablet, smartphone) and web browser (for example Chrome, Safari, Firefox, Internet Explorer) to complete this email verification step. You will then be able to sign up your clients."
    val linkText1 = "change your email address"
    val line3 = s"You can $linkText1 if it’s not correct."
    val accordionHeading = "I did not get an email"
    val linkText2 = "send it again"
    val accordionText = s"Check your spam folder. If it’s not there we can $linkText2. If we send your email again, any previous links will stop working."
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
    val heading: String = "Confirm your client’s company"
    val title = heading + ServiceName.agentSuffix
    val link = "Change company"
  }

  object AgentCompanyNameNotFound {
    val heading: String = "We could not confirm your client’s company"
    val title = heading + ServiceName.agentSuffix
    val line1 = "The company number you entered is not on our system."
  }

  object ClientVerifiedEmail {
    val heading: String = "You’ve verified your email address"
    val title = heading + ServiceName.agentSuffix
    val line1 = "HM Revenue and Customs will contact you by email instead of sending letters."
    val link = "Return to GOV.UK"
  }

  object ConfirmClient {
    val heading = "Confirm your client’s details"
    val title = heading + ServiceName.agentSuffix
    val firstName = "First name"
    val lastName = "Last name"
    val nino = "National Insurance number"
    val dob = "Date of birth"
  }

  object AgentConfirmNino {
    val heading = "Confirm your client’s details"
    val title = heading + ServiceName.agentSuffix
    val businessEntity = "Business type"
    val nino = "National Insurance number"

  }

  object FailedClientMatching {
    val heading: String = "There is a problem"
    val title = heading + ServiceName.agentSuffix
    val description = "The details you’ve entered are not on our system."
  }

  object ClientAlreadySignedUp {
    val heading: String = "Your client has already signed up"
    val title = heading + ServiceName.agentSuffix
    val line1 = "This client’s details are already in use."
  }

  object Terms {
    val heading: String = "Terms of participation"
    val title = heading + ServiceName.agentSuffix
    val line1 = "By taking part in this trial, you agree that either you or your client will:"
    val bullet1 = "use relevant software to record your client’s sales and purchases, then to submit their VAT Returns"
    val bullet2 = "submit each VAT Return within one calendar month and 7 days from the end of their accounting period"
    val bullet3 = "tell HMRC if your client stops trading and then submit their final VAT Return"
    val bullet4 = "tell HMRC if your client wants to leave this trial"
    val line2 = "These terms are not contractual and your client can stop taking part in this trial at any time."
  }

  object AgentCheckYourAnswersFinal {
    val heading = "Check your answers before sending"
    val title = heading + ServiceName.agentSuffix
    val vrn = "VAT number"
    val companyNumber = "Company Registration Number"
    val companyName = "Company Name"
    val partnershipCompanyNumber = "Partnership number"
    val partnershipCompanyName = "Partnership name"
    val registeredSocietyCompanyName = "Registered Society Company Name"
    val registeredSocietyCompanyNumber = "Society Registration number"
    val businessEntity = "Type of business"
    val nino = "National Insurance Number"
    val partnershipUtr = "Partnership UTR"
    val agentEmail = "Your email address"
    val contactPreference = "How to contact your client"
    val clientEmail = "Client email address"
  }

  object AgentInformationReceived {
    val heading: String = "We have received your request to sign up"
    val title = heading + ServiceName.agentSuffix

    object Section1 {
      val heading = "What happens next"
      val line1 = "You should advise your client that (where applicable):"
      val bullet1 =  "You’ve signed them up to Making Tax Digital for VAT."
      val bullet2 = "They’ll get email alerts to sign into HMRC services to access and read important secure messages."
      val bullet3 = "They’ll get an email alert 10 days before Direct Debit payments are taken from their bank account."
      val line2 = "Your client can view their VAT Return deadlines in their accounting software or business tax account."
    }

    object Section2 {
      val heading = "What you must do next"
      val bullet1 = "Allow and authorise your software to send VAT Returns to HMRC."
      val bullet2 = "Use your software to submit your client’s VAT Return."
      val bullet3 = "If required, help your client to choose compatible software. Advise them to start keeping their income, expenditure and VAT Records using this software."
      val bullet4 = "Where relevant, advise your client to view their VAT Return deadlines. They can do this within their accounting software or by signing into HMRC services (also called Business tax account)."
    }
    object Section3{
      val signout = "Finish and sign out"
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
    val line1 = "This is the 9 numbers on your VAT registration certificate. It is sometimes called a VAT registration number or VRN."
  }

  object CouldNotConfirmVatNumber {
    val heading: String = "We could not confirm your client’s VAT number"
    val title = heading + ServiceName.agentSuffix
    val line1 = "The information you provided does not match the details on our system."
    val tryAgain = "Try again"
  }

  object AgentCouldNotFindPartnership {
    val heading: String = "We could not confirm your client’s company number for their Limited Partnership"
    val title = heading + ServiceName.agentSuffix
    val line1 = "The company number is not on our system."
  }

  object AgentCouldNotConfirmPartnership {
    val heading: String = "We could not confirm your client’s Partnership"
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
    val hint = "For example, AB1 2YZ"
  }

  object PrincipalGuidance {
    val heading: String = "Use software to submit your VAT Returns"
    val title = ServiceName.principalSuffix.substring(3)
    val line1 = "HM Revenue and Customs (HMRC) are changing the way you submit your VAT Returns."
    val line2 = "From April 2019, VAT registered businesses with a turnover above £85,000 must use relevant third party software to submit their VAT Returns."
    val line3 = "If you’re a sole trader or a director of a limited company you can try this new way of reporting now, instead of waiting until April 2019."
    val line4 = "You’ll need to sign up for this new service, even if you already use software to submit your VAT Returns."

    object Section1 {
      val heading = "How it works"
      val line1 = "To sign up for this service, you need to use software to record your purchases and sales. If you don’t already record in this way, submit your next VAT Return on GOV.UK or by post before you sign up. This will give you more time to complete your first return using software."
      val number1 = "You need to choose relevant third party software. If you already use software, check with your supplier to see if you can use it with this service."
      val number2 = "Sign up for this service and agree to get emails instead of letters."
      val number3 = "Allow your software to submit VAT Returns to HMRC. You might need to sign in with your Government Gateway details."
      val number4 = "Use your software to record your purchases and sales, then to submit your VAT Returns. We’ll email you to let you know when to send a return."
      val line2 = "You can also choose to:"
      val bullet1 = "pay throughout the year, if it helps you manage your income"
      val bullet2 = "get your accountant to send your VAT Returns"
      val bullet3 = "view your VAT Return deadlines in your business tax account"
    }

    object Section2 {
      val heading = "Sign up"
      val line1 = "To sign up you’ll need the Government Gateway details you got when you registered for VAT."
      val line2 = "If you haven’t submitted a VAT Return online before, then you’ll also need your VAT registration certificate."
    }

  }

  object PrincipalCheckYourAnswers {
    val heading = "Check your answers"
    val title = heading + ServiceName.principalSuffix
    val subHeading = "You’ve told us"
    val yourVatNumber = "Your VAT number"
    val vatRegistrationDate = "Your VAT registration date"
    val businessPostCode = "Where your business is registered for VAT"
    val previousVatReturn = "If you are currently submitting VAT returns"
    val box5Figure = "Your VAT return total or Box 5 amount"
    val lastReturnMonth = "The last month in your latest accounting period"
  }

  object PrincipalCheckYourAnswersCompanySummary {
    val heading = "Check your answers"
    val title = heading + ServiceName.principalSuffix
    val subHeading = "You’ve told us"
    val companyNumber = "Your company number"
    val companyUtr = "Your company’s Unique Taxpayer Reference"
    val businessEntity = "Your business type"
  }

  object PrincipalCannotUseServiceYet {
    val heading: String = "Your business is not eligible at this time"
    val title = heading + ServiceName.principalSuffix
    val line1 = "Try again later."
  }

  object PrincipalRecentlyRegistered {
    val heading: String = "You cannot sign up for Making Tax Digital yet"
    val title = heading + ServiceName.principalSuffix
    val line1 = "You cannot sign up to Making Tax Digital (MTD) within the first 7 days of registering for VAT. You will be able to sign up 7 days after your effective date of registration."
    val button1 = "Return to VAT account"
  }

  object AgentRecentlyRegistered {
    val heading: String = "You cannot sign up for Making Tax Digital yet"
    val title = heading + ServiceName.principalSuffix
    val line1 = "You cannot sign up to Making Tax Digital (MTD) within the first 7 days of registering for VAT. You will be able to sign up 7 days after your effective date of registration."
    val button1 = "Return to VAT account"
    val button2 = "Sign a different business up to MTD"
  }

  object PrincipalCaptureEmail {
    val heading: String = "What is the business email address?"
    val title = heading + ServiceName.principalSuffix
    val line1 = "We will email you to confirm you’ve signed up your business to use software to submit its VAT Returns (this can take up to 72 hours)."
    val line2 = "We’ll email you a Direct Debit advance notice 10 working days before each payment is taken."
    val line3 = "If you do not want to receive Direct Debit notices by email, you will need to cancel your Direct Debit."
    val hint = "For example, yourname@example.com"
    val link = "HMRC Privacy Notice (opens in a new window or tab)"
    val disclaimer = s"Full details of how we use your information are in the $link."
  }

  object PrincipalConfirmEmail {
    val heading: String = "Check your email address"
    val title = heading + ServiceName.principalSuffix
    val emailHeading = "Email address"
    val link = "Change email address"
  }

  object PrincipalVerifyEmail {
    val heading: String = "Verify your business email address"
    val title = heading + ServiceName.principalSuffix

    def line1(email: String) = s"We’ve sent an email to $email. Click on the link in the email to verify the business email address. You must confirm your email address to continue the sign-up journey. Do not sign out."

    val line2 = "Use the same device (for example, computer, tablet, smartphone etc) and web browser (for example, Google Chrome, Safari, Internet Explorer etc) to complete this email verification step. You can then continue to sign up."
    val linkText1 = "change your email address"
    val line3 = s"You can $linkText1 if it’s not correct."
    val accordionHeading = "I did not get an email"
    val linkText2 = "send it again"
    val accordionText = s"Check your spam folder. If it’s not there we can $linkText2. If we send your email again, any previous links will stop working."
  }

  object PrincipalEmailVerified {
    val heading: String = "You’ve verified your email address"
    val title = heading + ServiceName.principalSuffix
    val line1 = "You’ll now receive messages and email notifications from HMRC."
  }

  object YourDetails {
    val heading: String = "Enter your details"
    val title = heading + ServiceName.principalSuffix
    val line1 = "We will attempt to match these details against information we currently hold."
    val field1 = "First name"
    val field2 = "Last name"
    val field3 = "National Insurance number"
    val field4 = "Date of birth"
    val formhint1_line1 = "For example, ’QQ 12 34 56 C’."
    val formhint2 = "For example, 10 12 1990"
  }

  object CaptureNino {
    val heading: String = "What is your National Insurance number?"
    val title = heading + ServiceName.principalSuffix
    val formHint = "It’s on your National Insurance card, benefit letter, payslip or P60. For example ’QQ 12 34 56 C’."
  }

  object AgentCaptureNino {
    val heading: String = "What is your client’s National Insurance number?"
    val title = heading + ServiceName.agentSuffix
    val formHint = "It’s on your client’s National Insurance card, benefit letter, payslip or P60. For example, ’QQ 12 34 56 C’."
  }

  object ConfirmDetails {
    val heading = "Confirm your details"
    val title = heading + ServiceName.principalSuffix
    val subHeading = "You’ve told us"
    val firstName = "First name"
    val lastName = "Last name"
    val nino = "National Insurance number"
    val dob = "Date of birth"
  }

  object PrincipalConfirmNino {
    val heading: String = "Check your answers"
    val title = heading + ServiceName.principalSuffix
    val subHeading = "You’ve told us"
    val businessEntity = "Your business type"
    val nino = "Your National Insurance number"
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

  object FailedMatching {
    val heading: String = "We couldn’t confirm your details"
    val title = heading + ServiceName.principalSuffix
    val line1 = "The information you provided doesn’t match the details on our system."
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
    val heading: String = "What is your company’s Unique Taxpayer Reference?"
    val title = heading + ServiceName.principalSuffix
    val line1 = "This is 10 numbers, for example 1234567890. It will be on tax returns and other letters about Corporation Tax. It may be called ’reference’, ’UTR’ or ’official use’."
    val linkText = "I have lost my UTR"
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

    val heading: String = "We have received your request to sign up"
    val title = heading + ServiceName.principalSuffix
    val vatNumber = "VAT registration number"

    object Section {
      val line = "You’ll get a confirmation email within the next 3 days."
      val line2 = "Do not submit a VAT Return during this time."

      val heading = "Next steps"
      val bullet1 = "Stop using VAT online services. Do not use your old way of sending your VAT Return to HMRC."
      val bullet2 = "Start using your compatible software to keep records of your business sales and purchases, also called income and expenditure."
      val bullet3 = "Allow and authorise your software to connect to HMRC."
      val bullet4 = "Only use this software to submit your VAT Returns to HMRC."
      val bullet5 = "Manage and update all your business details by signing in to HMRC services (this is also called a Business tax account)."
    }

  }

  object PrincipalResignupInformationReceived {

    val heading: String = "You have signed up to submit your VAT Returns using software"
    val title = heading + ServiceName.principalSuffix
    val vatNumber = "VAT registration number"

    object Section {
      val line = "Do not submit a VAT return until you have set up your software."

      val heading = "What happens next"
      val bullet1 = "Stop using VAT online services. Do not use your old way of sending your VAT Return to HMRC."
      val bullet2 = "Start using your compatible software to keep records of your business sales and purchases, also called income and expenditure."
      val bullet3 = "Allow and authorise your software to connect to HMRC."
      val bullet4 = "Use this software to submit your VAT Returns to HMRC."
      val text = "You can set up or update your:"
      val linkText1 = "Direct Debit for VAT"
      val linkText2 = "contact preference for VAT"
      val linkText3 = "business details"
      val linkText3b = s"$linkText3 such as business name, addresses and contact details"
    }

    val buttonText = "View your VAT account"

  }

  object AgentResignupInformationReceived {

    val heading: String = "You have signed up your client to submit VAT Returns using software"
    val title = "We have received your request to sign up" + ServiceName.agentSuffix
    val vatNumber = "VAT registration number"

    object Section {
      val line = "You should advise your client that (where applicable):"

      val heading = "What happens next"
      val bullet1 = "you’ve signed them up to Making Tax Digital for VAT"
      val bullet2 = "they’ll get email alerts to sign into HMRC services to access and read important secure messages"
      val bullet3 = "they’ll get an email alert 10 days before Direct Debit payments are taken from their bank account"
      val s2bullet1 = "Allow and authorise your software to send VAT Returns to HMRC."
      val s2bullet2 = "Use your software to submit your client’s VAT Return."
      val s2bullet3 = "If required, help your client to choose compatible software. Advise them to start keeping their income, expenditure and VAT records using this software."
      val s2bullet4 = "Where relevant, advise your client to view their VAT Return deadlines. They can do this within their accounting software or by signing into HMRC services (this is also known as a Business tax account)"
    }

      val buttonText = "Sign up another client"

  }

  object PrincipalAlreadySignedUp {
    val heading: String = "You have already signed up"
    val title = heading + ServiceName.principalSuffix
    val line = "Your sign in details are already in use."
  }

  object AgentGuidance {
    val heading = "Use software to submit your client’s VAT Returns"
    val title = ServiceName.agentSuffix.substring(3)
    val line1 = "From April 2019, sole trader businesses and limited companies with a turnover above £85,000 must submit their VAT Returns using software that supports Making Tax Digital."
    val line2 = "If your client is VAT registered they can choose to sign up for this new reporting method now. To help us test this new way of working out your tax, they’ll need to:"
    val bullet1 = "use software that supports Making Tax Digital to record their purchases and sales"
    val bullet2 = "submit their VAT Returns using their software"
    val bullet3 = "agree to get emails instead of letters"

    val subHeadingBeforeYouStart = "Before you start"
    val beforeYouStartLine1 = "To sign your clients up to this service you’ll need to:"
    val beforeYouStartBullet1 = "set up an agent services account"
    val beforeYouStartBullet2 = "add clients to your account"

    val subHeadingSignUpYourClients = "Sign up your clients"
    val signUpYourClientsLine1 = "Once you’ve set up your agent services account, you can sign up your clients for this new way of reporting their tax. This will add clients to your account."
    val signUpYourClientsLine2 = "You can sign up your clients if they’re either a sole trader or limited company. Your clients can submit their VAT Returns using software, or you can submit them on their behalf."

    val subHeadingSoftware = "Software"
    val softwareLine1 = "You need to choose software that supports Making Tax Digital. You can still use your existing software if it supports Making Tax Digital."
    val softwareLine2 = "If you and your client will use software, it’s important they choose a software package that can interact with yours."

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
    val heading: String = "What is the partnership’s company number?"
    val title = heading + ServiceName.principalSuffix
    val linkText = "search for your company number (opens in a new window or tab)"
    val line1 = s"You can $linkText on Companies House."
  }

  object PrincipalCouldNotConfirmPartnershipCompany {
    val heading: String = "We could not confirm your Limited Partnership’s company number"
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
    val heading: String = "What is your registered society’s Unique Taxpayer Reference?"
    val title = heading + ServiceName.principalSuffix
    val line1 = "This is 10 numbers, for example 1234567890. It will be on tax returns and other letters about Corporation Tax. It may be called ’reference’, ’UTR’ or ’official use’."
    val link = "I have lost my UTR"
  }

  object SignUpAfterThisDate {
    val heading = "You must come back later to sign up"
    val title = heading + ServiceName.principalSuffix
    val line1 = "We’re unable to sign you up for Making Tax Digital for VAT at present."
    val line2 = "This is either because:"
    val bullet1 = "a Direct Debit payment is due shortly"
    val bullet2 = "you are close to a filing period"
    val line3 = "Submit your current return using your usual method, then sign up to use compatible software to submit your next return. We will not charge you a penalty for this."

    def line4(date: String) = s"You’ll be able to sign up for this service after $date."

    val linkText = "Making Tax Digital"
    val line5 = s"Find more information about $linkText"
  }

  object SignUpBetweenTheseDates {
    val heading = "You must come back later to sign up"
    val title = heading + ServiceName.principalSuffix
    val line1 = "We’re unable to sign you up for Making Tax Digital for VAT at present."
    val line2 = "This is either because:"
    val bullet1 = "a Direct Debit payment is due shortly"
    val bullet2 = "you are close to a filing period"
    val line3 = "Submit your current return using your usual method, then sign up to use compatible software to submit your next return. We will not charge you a penalty for this."

    def line4(startDate: String, endDate: String) = s"You’ll be able to sign up for this service between $startDate and $endDate."

    val linkText = "Making Tax Digital"
    val line5 = s"Find more information about $linkText"
  }

  object AgentSignUpAfterThisDate {
    val heading = "You must come back later to sign up your client"
    val title = heading + ServiceName.agentSuffix
    val linkText = "Making Tax Digital"
    val line1 = "You’re not able to sign up your client for Making Tax Digital for VAT at present."
    val line2 = "This is either because:"
    val line3 = "Submit their current return using your usual method. Then you can sign up your client to Making Tax Digital so you can use compatible software to submit their next return. We will not charge your client a penalty for this."

    def line4(date: String) = s"You’ll be able to sign up your client for this service after $date."

    val line5 = s"Find more information about $linkText"
    val bullet1 = "a Direct Debit payment is due shortly"
    val bullet2 = "they’re close to a filing period"
  }

  object AgentSignUpBetweenTheseDates {
    val heading = "You must come back later to sign up your client"
    val title = heading + ServiceName.agentSuffix
    val linkText = "Making Tax Digital"
    val line1 = "You’re not able to sign up your client for Making Tax Digital for VAT at present."
    val line2 = "This is either because:"
    val line3 = "Submit their current return using your usual method. Then you can sign up your client to Making Tax Digital so you can use compatible software to submit their next return. We will not charge your client a penalty for this."

    def line4(startDate: String, endDate: String) = s"You’ll be able to sign up your client for this service between $startDate and $endDate."

    val line5 = s"Find more information about $linkText"
    val bullet1 = "a Direct Debit payment is due shortly"
    val bullet2 = "they’re close to a filing period"
  }

  object AgentCapturePartnershipCompanyNumber {
    val heading: String = "What is the company number of your client’s partnership?"
    val title = heading + ServiceName.agentSuffix
    val link = "check the Companies House register (opens in a new window or tab)"
    val line1 = s"If you are not sure, $link."
  }

  object AgentCompanyNameNotFoundLP {
    val heading: String = "We could not confirm your client’s Limited Partnership"
    val title = heading + ServiceName.agentSuffix
    val line1 = "The information you provided is not for a Limited Partnership."
  }

  object AgentCapturePartnershipUtr {
    val heading: String = "What is your client’s partnership Unique Taxpayer Reference (UTR)?"
    val title = heading + ServiceName.agentSuffix
    val line = "This is a 10 digit number. You can find it on letters to the partnership from HM Revenue & Customs."
    val hint = "For example, 0123456789"
    val accordionHeading = "I do not have my client’s Unique Taxpayer Reference"
    val accordionText = "Entering your client’s UTR will help us identify the correct partnership."
    val accordionLink1 = "How your client can find their UTR"
    val accordionLink2 = "My client’s partnership does not have a UTR"
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
    val heading = "Where is your client’s partnership registered for Self Assessment?"
    val title = heading + ServiceName.agentSuffix
  }

  object CapturePartnershipUtr {
    val heading = "What is the partnership’s Unique Taxpayer Reference?"
    val title = heading + ServiceName.principalSuffix
    val line1 = "This is 10 numbers, for example 1234567890. It will be on tax returns and other letters about Self Assessment. It may be called ’reference’, ’UTR’ or ’official use’."
    val accordionHeading = "I do not have my Unique Taxpayer Reference"
    val accordionText = "Your UTR helps us identify your partnership. I cannot find my UTR My partnership does not have a UTR"
    val cannotFind = "I cannot find my UTR"
  }

  object PartnershipPrincipalPlaceOfBusiness {
    val heading = "What is the postcode where the partnership is registered for Self Assessment?"
    val title = heading + ServiceName.principalSuffix
  }

  object PartnershipsCYA {
    val heading = "Check your answers"
    val title = heading + ServiceName.principalSuffix
    val businessEntity = "Type of business or group"
    val companyNumber = "Partnership’s company number"
    val companyUtr = "The partnership’s Unique Taxpayer Reference"
    val postCode = "The postcode where the partnership is registered for Self Assessment"
    val noSautr = "I do not have one"
  }

  object AgentCheckYourAnswers {
    val heading = "Confirm your client’s details"
    val title = heading + ServiceName.agentSuffix
    val yourUtr = "Partnership Self Assessment UTR"
    val yourCompanyNumber = "Partnership company number"
    val yourBusinessPostCode = "Where your client’s partnership is registered for Self Assessment"
    val generalPartnership: String = "General partnership"
    val limitedPartnership: String = "Limited partnership (including limited liability partnerships)"
    val noSAUTR: String = "My client does not have one"
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

  object AreYouReadySubmitSoftware {
    val heading = "Are you ready to submit your next VAT Return using software compatible with Making Tax Digital?"
    val title = heading + ServiceName.principalSuffix
    val line1 = "Once you’ve signed up for Making Tax Digital, you can only send in your VAT Return using software."
    val line2 = "If you’re not ready to use software yet, you should send your next VAT Return using your usual method."
    val errorMessage = "Select yes if you’re ready to submit your next VAT return using compatible software"
  }
  object AgentConfirmRegisteredSociety {
    val heading: String = "Confirm your client’s registered society"
    val title = heading + ServiceName.agentSuffix
    val link = "Change company"
  }

  object AgentRegisteredSocietyCompanyNameNotFound {
    val heading: String = "We could not confirm your client’s registered society"
    val title = heading + ServiceName.agentSuffix
    val line1 = "The information you provided does not match the details we have for your client’s registered society."
  }

  object MigrationInProgressError {
    val heading: String = "You have already signed up"
    val title = heading + ServiceName.principalSuffix
    val line = "Your Making Tax Digital for VAT account is being set up. You will receive an email within 72 hours."
  }

  object AgentMigrationInProgressError {
    val heading: String = "Your client is already signed up"
    val title = heading + ServiceName.agentSuffix
    val line = "Your client’s Making Tax Digital for VAT account is being set up. They will receive an email within 72 hours."
  }

  object PrincipalCheckYourAnswersRegisteredSociety {
    val heading = "Check your answers"
    val title = heading + ServiceName.principalSuffix
    val subHeading = "You’ve told us"
    val businessEntity = "Your business type"
    val registeredSociety = "Registered Society"
    val companyNumber = "Your company number"
    val companyUtr = "Your company’s Unique Taxpayer Reference"
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

  object CaptureBox5Figure {
    val heading = "What is your latest VAT Return total?"
    val title = heading + ServiceName.principalSuffix
    val line = "You can find this amount in box number 5 on your latest VAT Return submitted to HMRC."
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

  object PrincipalDoYouHaveAUtr {
    val heading: String = "Does your partnership have a Self Assessment Unique Taxpayer Reference (UTR) number?"
    val title: String = heading + ServiceName.principalSuffix
    val line: String = "This is a 10-digit number. You can find it on letters to the partnership from HMRC."
  }

  object AgentDoesYourClientHaveAUtr {
    val heading: String = "Does your client’s partnership have a Self Assessment Unique Taxpayer Reference (UTR) number?"
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
    val partnership_company_number: String = "Partnership Company Registration Number"
    val partnership_name: String = "Partnership Company Name"
    val registered_society_name: String = "Registered Society Company Name"
    val company_number: String = "Company Registration Number"
    val company_name: String = "Company Name"
    val email_address: String = "Business Email Address"
    val contact_preference: String = "How we contact you"
    val digital: String = "Email"
    val letter: String = "Letters"
  }

  object PrincipalReturnDue {
    val heading: String = "As you’re not ready to use software yet, you should send your next VAT Return using your usual method"
    val title: String = heading + ServiceName.principalSuffix
    val line_1: String = "From April 2019 it became mandatory to submit VAT Returns using compatible software if your annual taxable turnover is above £85,000."
    val line_2: String = "You can choose to sign up if your annual taxable turnover is below £85,000."
    val link_id: String = "mtdGuidance"
    val link = "http://www.gov.uk/guidance/check-when-a-business-must-follow-the-rules-for-making-tax-digital-for-vat"
    val link_text = "Find out if and when you (or your clients) need to follow the rules"
    val line_3: String = s"${link_text} for Making Tax Digital for VAT, or apply for an exemption."
  }

  object MakingTaxDigitalSoftware {
    val heading = "Things you must know and do"
    val title = heading + ServiceName.principalSuffix
    val line1 = "This is a new way of keeping your VAT Records and submitting your VAT Returns online by using software which is compatible with HMRC."
    val line2 = "Things you must do first:"
    val bullet1 = "get software which connects you to HMRC"
    val bullet2 = "start keeping your VAT Records using this software"
    val findSoftware = "Find out more information about Making Tax Digital for VAT (opens in a new tab)"
    val linkId = "find-software-link"
  }

  object GotSoftware {
    val heading = "Your existing accounting software must be compatible with HMRC"
    val title: String = heading + ServiceName.principalSuffix
    val para = "Check the following first before signing up:"
    val bullet1 = "your software connects with HMRC"
    val bullet2 = "if you use an accountant make sure your software works with their software"
    val softwareCompatible = "Check if your software is compatible (opens in a new tab)"
    val linkId = "software-compatible-link"
    val signUp = "Sign up"
  }

  object UseSpreadsheets {
    val heading = "You’ll need bridging software to continue using your spreadsheets"
    val title: String = heading + ServiceName.principalSuffix
    val line1 = "This software connects your spreadsheets to HMRC."
    val line2 = "If you:"
    val bullet1 = "do not have this software, get and set it up on your computer"
    val bullet2 = "do have it, check it is compatible and meets your business needs"
    val findSoftware = "Find out how to get and select bridging software (opens in a new tab)"
    val linkId = "find-software-link"
    val signUp = "Sign up"
  }

  object PrincipalHaveYouGotSoftware {
    val heading: String = "How do you currently keep your VAT records?"
    val title: String = heading + ServiceName.principalSuffix
    val error: String = "You must select an option"
    val accounting_software: String = "I use accounting software"
    val spreadsheets: String = "I use spreadsheets"
    val neither: String = "I use neither"
  }

  object NotGotSoftware {
    val heading: String = "You must get compatible software first"
    val title: String = heading + ServiceName.principalSuffix
    val line_1: String = "Before you sign up you must:"
    val line_2: String = "Set up compatible accounting software which is best suited to your business needs."
    val line_3: String = "Check your chosen software works and connects to HMRC."
    val link_id: String = "mtdGuidance"
    val link = "http://www.gov.uk/guidance/find-software-thats-compatible-with-making-tax-digital-for-vat"
    val link_text = "Get more information about software products and companies"
  }

  object PasscodeNotFound {
    val heading: String = "There is a problem with the passcode"
    val title = heading + ServiceName.principalSuffix
    val line1 = "The code we sent you cannot be found or has expired."
    val line2 = "You will need to get a new code to continue with the registration."
  }

  object MaxEmailPasscodeAttemptsExceeded {
    val heading: String = "You need to start again"
    val title = heading + ServiceName.principalSuffix
    val line1 = "This is because you have entered the wrong code too many times."
    val line2 = "To start again, you need to sign out"
  }
}
