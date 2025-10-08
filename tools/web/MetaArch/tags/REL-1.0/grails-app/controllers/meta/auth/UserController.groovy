package meta.auth

import grails.plugins.springsecurity.Secured
import meta.CurrentUserService;
import meta.EmailerService;

import org.springframework.dao.DataIntegrityViolationException;

class UserController {
	
	def springSecurityService
	def CurrentUserService currentUserService
	
	EmailerService emailerService

	def scaffold = User
	
    //def index() {  }
	
	// Need a way to make the ROLE_USER only see their own profile?
//	def profile() {
//		def per = currentUserService.lookupUser()
//		def userInstance = User.get(per.id)
//		
//		if (!userInstance) {
//			flash.message = message(code: 'default.not.found.message', args: [message(code: 'meta.auth.user.label', default: 'User'), per.id])
//			redirect(action: "list")
//			return
//		}
//		
//		if ( (!currentUserService.isAdmin()) && (!(per).equals(userInstance)) ) {
//			flash.message = message(code: 'default.unauthorized.message', args: ['view', message(code: 'meta.auth.user.label', default: 'User'), per.id])
//			redirect(action: "list")
//			return
//		}
//
//		[userInstance: userInstance]
//	}
	
	def save() {
		def per = currentUserService.lookupUser()
		def isAdmin = currentUserService.isAdmin()
		def userInstance = new User(params)
		
		log.info "\nUser Save: user is admin? ${isAdmin}\n"
		
		if ( (isAdmin == false) && (per.id != userInstance.id) ) {
			flash.message = message(code: 'default.unauthorized.noId.message', args: ['save', message(code: 'meta.auth.user.label', default: 'User')])
			redirect(action: "list")
			return
		}
		
		// Be sure that the new user will be automatically enabled.
		if (!params.enabled) {
			userInstance.enabled = true
		}
		
		if (!userInstance.save(flush: true)) {
			render(view: "create", model: [userInstance: userInstance])
			return
		}
		
		// Add ROLE_USER by default
		def userRole = Authority.findByAuthority('ROLE_USER')
		if (!userInstance.authorities.contains(userRole)) {
			UserAuthority.create userInstance, userRole, true
		}

		flash.message = message(code: 'default.created.message', args: [message(code: 'meta.auth.user.label', default: 'User'), userInstance.id])
		redirect(action: "show", id: userInstance.id)
	}
	
	def show() {
		def per = currentUserService.lookupUser()
		def isAdmin = currentUserService.isAdmin()
		def userInstance = User.get(params.id)
		
		log.info "\nUser Show: user is admin? ${isAdmin}\n"
		
		if (!userInstance) {
			flash.message = message(code: 'default.not.found.message', args: [message(code: 'meta.auth.user.label', default: 'User'), params.id])
			redirect(action: "list")
			return
		}
		
		if ( (isAdmin == false) && (per.id != userInstance.id) ) {
			flash.message = message(code: 'default.unauthorized.message', args: ['view', message(code: 'meta.auth.user.label', default: 'User'), params.id])
			redirect(action: "list")
			return
		}

		[userInstance: userInstance]
	}
	
	@Secured(['ROLE_ADMIN', 'ROLE_DEVELOP'])
	def create() {
		def isAdmin = currentUserService.isAdmin()
		
		log.info "\nUser Create: user is admin? ${isAdmin}\n"
		
		if( isAdmin == false ) {
			flash.message = message(code: 'default.unauthorized.noId.message', args: ['create', message(code: 'meta.auth.user.label', default: 'User')])
			redirect(action: "list")
			return
		}

		[userInstance: new User(params)]
	}
	
	def edit() {
		def per = currentUserService.lookupUser()
		def isAdmin = currentUserService.isAdmin()
		def userInstance = User.get(params.id)
		
		log.info "\nUser Edit: user is admin? ${isAdmin}\n"
		
		if (!userInstance) {
			flash.message = message(code: 'default.not.found.message', args: [message(code: 'meta.auth.user.label', default: 'User'), params.id])
			redirect(action: "list")
			return
		}
		
		if (isAdmin == true) {
			return [userInstance: userInstance]
		}
		
		if ( per.id != userInstance.id ) {
			flash.message = message(code: 'default.unauthorized.message', args: ['edit', message(code: 'meta.auth.user.label', default: 'User'), params.id])
			redirect(action: "list")
			return
		}

		[userInstance: userInstance]
	}

	def update() {
		def per = currentUserService.lookupUser()
		def isAdmin = currentUserService.isAdmin()
		def userInstance = User.get(params.id)
		
		log.info "\nUser Update: user is admin? ${isAdmin}\n"
		
		if (!userInstance) {
			flash.message = message(code: 'default.not.found.message', args: [message(code: 'meta.auth.user.label', default: 'User'), params.id])
			redirect(action: "list")
			return
		}
		
		if ( (isAdmin == false) && (per.id != userInstance.id) ) {
			flash.message = message(code: 'default.unauthorized.message', args: ['update', message(code: 'meta.auth.user.label', default: 'User'), userInstance.id])
			redirect(action: "list")
			return
		}

		if (params.version) {
			def version = params.version.toLong()
			if (userInstance.version > version) {
				def lowerCaseName = 'user'
				userInstance.errors.rejectValue("version", "default.optimistic.locking.failure",
						  [message(code: 'meta.auth.user.label', default: 'User')] as Object[],
						  "Another user has updated this User while you were editing")
				render(view: "edit", model: [userInstance: userInstance])
				return
			}
		}

		userInstance.properties = params

		if (!userInstance.save(flush: true)) {
			render(view: "edit", model: [userInstance: userInstance])
			return
		}

		flash.message = message(code: 'default.updated.message', args: [message(code: 'meta.auth.user.label', default: 'User'), userInstance.id])
		redirect(action: "show", id: userInstance.id)
	}

	@Secured(['ROLE_ADMIN', 'ROLE_DEVELOP'])
	def delete() {
		def per = currentUserService.lookupUser()
		def isAdmin = currentUserService.isAdmin()
		def userInstance = User.get(params.id)
		
		log.info "\nUser Delete: user is admin? ${isAdmin}\n"
		
		if (!userInstance) {
			flash.message = message(code: 'default.not.found.message', args: [message(code: 'meta.auth.user.label', default: 'User'), params.id])
			redirect(action: "list")
			return
		}
		
		if ( isAdmin == false ) {
			// We only want administrators adding/removing Users!
			flash.message = message(code: 'default.unauthorized.message', args: ['delete', message(code: 'meta.auth.user.label', default: 'User'), params.id])
			redirect(action: "list")
			return
		}

		try {
			def userAuths = UserAuthority.findAllByUser(userInstance)
			if (userAuths) {
				userAuths*.delete()
			}
			userInstance.delete(flush: true)
			flash.message = message(code: 'default.deleted.message', args: [message(code: 'meta.auth.user.label', default: 'User'), params.id])
			redirect(action: "list")
		}
		catch (DataIntegrityViolationException e) {
			flash.message = message(code: 'default.not.deleted.message', args: [message(code: 'meta.auth.user.label', default: 'User'), params.id])
			redirect(action: "show", id: params.id)
		}
	}
	
	@Secured(['ROLE_ADMIN', 'ROLE_DEVELOP'])
	def approveUserReg() {
		def per = currentUserService.lookupUser()
		def isAdmin = currentUserService.isAdmin()
		def userInstance = User.get(params.id)
		
		log.info "\nUser Approve User Registration: user is admin? ${isAdmin}\n"
		
		if (!userInstance) {
			flash.message = message(code: 'default.not.found.message', args: [message(code: 'meta.auth.user.label', default: 'User'), params.id])
			redirect(action: "list")
			return
		}
		
		if ( isAdmin == false ) {
			// We only want administrators adding/removing Users!
			flash.message = message(code: 'default.unauthorized.message', args: ['delete', message(code: 'meta.auth.user.label', default: 'User'), params.id])
			redirect(action: "list")
			return
		}

		try {
			if (userInstance.enabled == false) {
				userInstance.enabled = true
			}
			if (userInstance.accountLocked == true) {
				userInstance.accountLocked = false
			}
			if (!userInstance.save()) {
				throw 'Unable to save userInstance'
			}
			
			emailerService.sendRegistrationApprovedNotification(userInstance)
			
			flash.message = 'User Registration has been approved for user ' + params.id
			redirect(action: "show", id: params.id)
		}
		catch (e) {
			flash.message = 'Unable to approve registration for user ' + params.id + ':\t' + e
			redirect(action: "show", id: params.id)
		}
	}
}
