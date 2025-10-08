package meta.auth

//import grails.plugins.springsecurity.ui.RegisterCommand
import meta.EmailerService
import org.codehaus.groovy.grails.plugins.springsecurity.NullSaltSource
import org.codehaus.groovy.grails.plugins.springsecurity.ui.RegistrationCode
import org.codehaus.groovy.grails.plugins.springsecurity.SpringSecurityUtils

class RegisterController extends grails.plugins.springsecurity.ui.RegisterController {
	
	EmailerService emailerService
	
	def message = {}
	
	def register = { RegisterCommand command ->		
		if (command.hasErrors()) {
			println 'Command Errors:\n'
			command.errors.each { e ->
				println e
			}
			println ''
			
			render view: 'index', model: [command: command]
			return
		}

		String salt = saltSource instanceof NullSaltSource ? null : command.username
		def user = lookupUserClass().newInstance(email: command.email, username: command.username,
				password: command.password,
				organization: params.organization, realname: params.realname, phoneNumber: params.phoneNumber,
				accountLocked: true, enabled: false) // Note: we cannot have users enabled UNTIL they have been approved by MetaArch admins!
		def userRole = Authority.findByAuthority('ROLE_USER')

		// Try to save the user instance - if it fails, return miscError
		User.withTransaction { status ->
			if (!user.save()) {
//				warnErrors user, messageSource
				status.setRollbackOnly()
				
				flash.error = message(code: 'spring.security.ui.register.miscError')
				flash.chainedParams = params
				
				redirect action: 'index'
				return
			} else {
				// Create the default user authority
				if (!user.authorities.contains(userRole)) {
					UserAuthority.create user, userRole, true
				}
			}
		}
		
		RegistrationCode registrationCode = new RegistrationCode(username: user.username)
		//registrationCode = springSecurityUiService.register(user, command.password, salt)
		RegistrationCode.withTransaction { status ->
			if (!registrationCode.save()) {
				//warnErrors registrationCode, messageSource
				status.setRollbackOnly()
			}
		}
		if (registrationCode == null || registrationCode.hasErrors()) {
			// null means problem creating the user
			flash.error = message(code: 'spring.security.ui.register.miscError')
			flash.chainedParams = params	
			
			if (registrationCode != null && registrationCode.hasErrors()) {
				println 'Registration Code Errors:\n'
				registrationCode.errors.each { e ->
					println e
				}
			}
						
			redirect action: 'index'
			return
		}

		String url = generateLink('verifyRegistration', [t: registrationCode.token])

		def conf = SpringSecurityUtils.securityConfig
		def body = conf.ui.register.emailBody
		if (body.contains('$')) {
			body = evaluate(body, [user: user, url: url])
		}
		mailService.sendMail {
			to command.email
			from conf.ui.register.emailFrom
			subject conf.ui.register.emailSubject
			html body.toString()
		}
	
		render view: 'index', model: [emailSent: true]
	}

	def verifyRegistration = {
		def conf = SpringSecurityUtils.securityConfig
		String defaultTargetUrl = '/register/message' //conf.successHandler.defaultTargetUrl

		String token = params.t

		def registrationCode = token ? RegistrationCode.findByToken(token) : null
		if (!registrationCode) {
			flash.error = message(code: 'spring.security.ui.register.badCode')
			redirect uri: conf.ui.register.postRegisterUrl ?: defaultTargetUrl
			return
		}

		def user
		RegistrationCode.withTransaction { status ->
			user = lookupUserClass().findByUsername(registrationCode.username)
			if (!user) {
				return
			}
			// Commented this section out - you need administrative approval!
			/*
			user.accountLocked = false
			user.save(flush:true)
			def UserRole = lookupUserRoleClass()
			def Role = lookupRoleClass()
			for (roleName in conf.ui.register.defaultRoleNames) {
				UserRole.create user, Role.findByAuthority(roleName)
			}
			*/
			registrationCode.delete()
		}

		if (!user) {
			flash.error = message(code: 'spring.security.ui.register.badCode')
			redirect uri: conf.ui.register.postRegisterUrl ?: defaultTargetUrl
			return
		}

//		springSecurityService.reauthenticate user.username
		
		emailerService.sendRegistrationApprovalRequest(user, conf.ui.register.emailFrom)

		flash.message = message(code: 'spring.security.ui.register.complete')
		redirect uri: conf.ui.register.postRegisterUrl ?: defaultTargetUrl
	}
	
	def forgotPassword = {

		if (!request.post) {
			// show the form
			return
		}

		String username = params.username
		if (!username) {
			flash.error = message(code: 'spring.security.ui.forgotPassword.username.missing')
			redirect action: 'forgotPassword'
			return
		}

		def user = User.findWhere(username: username)
		if (!user) {
			flash.error = message(code: 'spring.security.ui.forgotPassword.user.notFound')
			redirect action: 'forgotPassword'
			return
		}

		def registrationCode = new RegistrationCode(username: user.username)
		registrationCode.save(flush: true)

		String url = generateLink('resetPassword', [t: registrationCode.token])

		def conf = SpringSecurityUtils.securityConfig
		def body = conf.ui.forgotPassword.emailBody
		if (body.contains('$')) {
			body = evaluate(body, [user: user, url: url])
		}
		mailService.sendMail {
			to user.email
			from conf.ui.forgotPassword.emailFrom
			subject conf.ui.forgotPassword.emailSubject
			html body.toString()
		}

		[emailSent: true]
	}
	
	def resetPassword = { ResetPasswordCommand command ->
		
		String token = params.t

		def registrationCode = token ? RegistrationCode.findByToken(token) : null
		if (!registrationCode) {
			flash.error = message(code: 'spring.security.ui.resetPassword.badCode')
			redirect uri: SpringSecurityUtils.securityConfig.successHandler.defaultTargetUrl
			return
		}

		if (!request.post) {
			return [token: token, command: new ResetPasswordCommand()]
		}

		command.username = registrationCode.username
		command.validate()

		if (command.hasErrors()) {
			return [token: token, command: command]
		}

		String salt = saltSource instanceof NullSaltSource ? null : registrationCode.username
		RegistrationCode.withTransaction { status ->
			def user = User.findByUsername(registrationCode.username)
			
			// Necessary to check if the password is already encoded (multiple datasources)
//			if ( !dataSource_lookup ) {
//				user.isPasswordEncoded = true // Necessary for multiple datasources
				user.password = command.password //springSecurityUiService.encodePassword(command.password, salt)
//			}			
			
			// Try to save the user instance - if it fails, return resetPassword.failure
			if (!user.save()) {
				status.setRollbackOnly()
				
				flash.error = 'Unable to reset the password for user' + user.id //message(code: 'spring.security.ui.register.miscError')
				flash.chainedParams = params
				
				redirect action: 'index'
				return
			} else {
				registrationCode.delete()
			}
		}

		springSecurityService.reauthenticate registrationCode.username

		flash.message = message(code: 'spring.security.ui.resetPassword.success')

		def conf = SpringSecurityUtils.securityConfig
		String postResetUrl = conf.ui.register.postResetUrl ?: '/register/message' //conf.successHandler.defaultTargetUrl
		redirect uri: postResetUrl
	}
	
}

/* May need to override these classes for multiple datasource encodepassword bug. */
class RegisterCommand {
	
	String username
	String email
	String password
	String password2

	def grailsApplication

	static constraints = {
		username blank: false, nullable: false, validator: { value, command ->
			if (value) {
				def User = command.grailsApplication.getDomainClass(
					SpringSecurityUtils.securityConfig.userLookup.userDomainClassName).clazz
				if (User.findByUsername(value)) {
					return 'registerCommand.username.unique'
				}
			}
		}
		email blank: false, nullable: false, email: true
		password blank: false, nullable: false, validator: RegisterController.passwordValidator
		password2 validator: RegisterController.password2Validator
	}
}

class ResetPasswordCommand {
	String username
	String password
	String password2

	static constraints = {
		username nullable: false
		password blank: false, nullable: false, validator: RegisterController.passwordValidator
		password2 validator: RegisterController.password2Validator
	}
}
/**/
