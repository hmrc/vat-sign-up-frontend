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
    val heading: String = "What is your client's company number?"
    val title = heading + ServiceName.agentSuffix
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
    val heading: String = "What type of business is your client registered as?"
    val title = heading + ServiceName.agentSuffix
    val radioSoleTrader: String = "Sole trader"
    val radioLimitedCompany: String = "Limited company"
    val radioGeneralPartnership: String = "General partnership"
    val radioLimitedPartnership: String = "Limited partnership (including limited liability partnerships)"
    val radioVatGroup: String = "VAT group"
    val radioDivision: String = "Administrative Division"
    val radioUnincorporatedAssociation: String = "Unincorporated Association"
    val radioRegisteredSociety: String = "Registered Society (including Community Benefit Societies and Co-operative Societies)"
    val radioTrust: String = "Trust"
    val radioCharity: String = "CIO (charity)"
    val radioGovernmentOrganisation: String = "Government organisations and public sector"
    val radioOther: String = "Other"
  }

  object YesNo {
    val heading: String = "Do you have more than one VAT registered business?"
    val title = heading + ServiceName.principalSuffix
    val radioYes: String = "yes"
    val radioNo: String = "no"
  }

  object CaptureLastReturnMonthPeriod {
    val heading: String = "When was your most recent VAT payment?"
    val title: String = "When was your last VAT return date?" + ServiceName.principalSuffix
    val line: String = "Select the month of your most recent VAT payment. You can find your most recent VAT payment in your online business tax account."
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
    val line1 = "We will only send you an email to let you know if your client can join the trial."
    val hint = "For example, me@me.com"
  }

  object CaptureClientEmail {
    val heading: String = "What is your client's email address?"
    val title = heading + ServiceName.agentSuffix
    val hint = "For example, me@me.com"
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
    val title = "When did you register for VAT?" + ServiceName.principalSuffix
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
    val heading = "Where is the principal place of business?"
    val title = heading + ServiceName.principalSuffix
    val line1 = "This is where the company carries out most of its business activities."
    val line2 = "If the company does business in different places or contract work on client premises, it is where the company keeps its financial and business records."
    val label = "UK postcode"
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

  object PrincipalCaptureBusinessEntity {
    val heading: String = "What type of business are you registered as?"
    val title = heading + ServiceName.principalSuffix
    val radioSoleTrader: String = "Sole trader"
    val radioLimitedCompany: String = "Limited company"
    val radioGeneralPartnership: String = "General partnership"
    val radioLimitedPartnership: String = "Limited partnership (including limited liability partnerships)"
    val radioVatGroup: String = "VAT group"
    val radioDivision: String = "Administrative division"
    val radioUnincorporatedAssociation: String = "Unincorporated association"
    val radioTrust: String = "Trust"
    val radioRegisteredSociety = "Registered society (including community benefit societies and co-operative societies)"
    val radioCharity = "CIO (charity)"
    val radioGovernmentOrganisation: String = "Government organisations and public sector"
    val radioOther: String = "Other"
  }

  object PrincipalCheckYourAnswers {
    val heading = "Check your answers"
    val title = heading + ServiceName.principalSuffix
    val subHeading = "You've told us"
    val yourVatNumber = "What is your VAT number?"
    val vatRegistrationDate = "What is your VAT registration date?"
    val businessPostCode = "What is your business postcode?"
    val businessEntity = "What type of business are you registered as?"
    val box5Figure = "What is your Box 5 amount?"
    val lastReturnMonth = "When was your most recent VAT payment?"
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
    val heading: String = "What is your email address?"
    val title = heading + ServiceName.principalSuffix
    val hint = "For example, me@me.com"
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

  object ConfirmDetails {
    val heading = "Confirm your details"
    val title = heading + ServiceName.principalSuffix
    val subHeading = "You've told us"
    val firstName = "First name"
    val lastName = "Last name"
    val nino = "National Insurance number"
    val dob = "Date of birth"
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
    val heading: String = "What is your company number?"
    val title = heading + ServiceName.principalSuffix
    val line1 = "You received this from Companies House when you set up your company. It's 8 digits and sometimes starts with 2 letters."
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
      val linkText = "Choose relevant third party software (opens in a new window or tab)"

      val heading = "What happens next"
      val line1 = "As this service is currently a pilot, it is only available to some limited companies, sole traders, partnerships and VAT groups."
      val line2 = "We will send you an email to let you know whether you can take part in this trial. You should wait for this email before you choose software."
      val bullet1 = "We will let you know whether you can take part in this trial, usually within 72 hours."
      val bullet2 = s"${linkText}."
      val bullet3 = "Allow your software to submit VAT Returns to HMRC. You might need to sign in with your Government Gateway details."
      val bullet4 = "Use software to record your sales and purchases."
      val bullet5 = "Submit your VAT Returns before your deadlines."
      val line3 = "You can view your VAT Return deadlines in your accounting software or business tax account."
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
    val linkText = "search for your company number (opens in a window or tab)"
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
    val line1 = "There is not enough time for us to set you up with Making Tax Digital for VAT before your next direct debit payment is due."

    def line2(date: String) = s"Please come back and sign up after $date."
  }

  object SignUpBetweenTheseDates {
    val heading = "You must come back later to sign up"
    val title = heading + ServiceName.principalSuffix
    val line1 = "There is not enough time for us to set you up with Making Tax Digital for VAT before your next direct debit payment is due."

    def line2(startDate: String, endDate: String) = s"Please come back and sign up between $startDate and $endDate."
  }

  object AgentSignUpAfterThisDate {
    val heading = "You must come back later to sign up"
    val title = heading + ServiceName.agentSuffix
    val line1 = "There is not enough time for us to set your client up with Making Tax Digital for VAT before their next direct debit payment is due."

    def line2(date: String) = s"Please come back and sign them up after $date."
  }

  object AgentSignUpBetweenTheseDates {
    val heading = "You must come back later to sign up"
    val title = heading + ServiceName.agentSuffix
    val line1 = "There is not enough time for us to set your client up with Making Tax Digital for VAT before their next direct debit payment is due."

    def line2(startDate: String, endDate: String) = s"Please come back and sign them up between $startDate and $endDate."
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
    val heading: String = "What is your client's Unique Taxpayer Reference (UTR)?"
    val title = heading + ServiceName.agentSuffix
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
    val heading = "Where is the client's principal place of business?"
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
  }

  object AgentCheckYourAnswers {
    val heading = "Confirm your client's details"
    val title = heading + ServiceName.agentSuffix
    val yourUtr = "What is your client's Unique Taxpayer Reference (UTR)?"
    val yourCompanyNumber = "What is your client's company number?"
    val yourBusinessPostCode = "Where is your client's principal place of business?"
    val generalPartnership: String = "General partnership"
    val limitedPartnership: String = "Limited partnership (including limited liability partnerships)"
  }

  object UnplannedOutage {
    val heading: String = "Sorry, there is a problem with the service"
    val title = heading + ServiceName.principalSuffix
    val line1 = "Try again later."
    val line2 = "In the meantime:"
    val link1 = "full list of VAT services"
    val link2 = "'Help and support for VAT'"
    val link3 = "how VAT businesses can get ready"
    val bullet1 = s"get our $link1"
    val bullet2 = s"read our $link2"
    val bullet3 = s"find out $link3 for the Making Tax Digital programme"
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
    val heading = "What is your Box 5 amount?"
    val title = heading + ServiceName.principalSuffix
    val line = "Enter the Box 5 amount from your last VAT return. Your Box 5 amount was calculated automatically if you completed your VAT return online."
  }

  object CancelDirectDebit {
    val heading = "If you do not want to use your email address for Direct Debit notifications"
    val title = heading + ServiceName.principalSuffix
    val line1 = "You will need to:"
    val bullet1 = "cancel your Direct Debit with your bank"
    val bullet2 = "notify HMRC that you have cancelled the Direct Debit"
    val bullet3 = "wait 7 to 10 days before trying to sign up again"
    val linkText = "Go back to Direct debit terms and conditions"
    val linkId = "directDebitTerms"
    val link = "/DD-terms-conditions"
    val buttonText = "Logout"
  }

  object PrincipalDirectDebitTermsAndConditions {
    val heading = "Terms and Conditions"
    val title = heading + ServiceName.principalSuffix
    val link1 = "Direct Debit terms and conditions (opens in a new window or tab)"
    val line = s"You will need to read and agree to the $link1 to continue."
    val link2 = "I do not agree"
  }

}
