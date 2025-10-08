package meta

import meta.auth.*

class CurrentUserService {
	
	static boolean transactional = false
	
	def springSecurityService

	def lookupUser() {
		def user
		if (springSecurityService.isLoggedIn()) { user = User.get(springSecurityService.getCurrentUser().id) }
		
		//print "user: ${user}"
		return user
	}
		
	def currentUserProjects() {
		def per = lookupUser()		
		
		// If a project member...
		def List currentProjects = Project.executeQuery("select distinct pm.project from ProjectMember pm where pm.member = ?", [per])
		//print "currentProjects: ${currentProjects}"
		
		if (isAdmin()) {
			currentProjects = Project.list()
		} else if (isDmg()) {
			currentProjects = Project.list()
		}
		
		return currentProjects
	}
	
	def isAdmin() {
		def curUser 
		if (springSecurityService.isLoggedIn()) { curUser = User.get(springSecurityService.getCurrentUser().id) }
		else { return false }
		
		def auths = curUser.getAuthorities()
		def adminStatus = false
		
		auths.each {
			if (it.authority.equals("ROLE_ADMIN")) {
				adminStatus = true
			} else if (it.authority.equals("ROLE_DEVELOP")) {
				adminStatus = true
			}
		}
		
		//log.info("Is user admin? ${adminStatus}")
		
		return adminStatus
	}
	
	def isDmg() {
		def curUser
		if (springSecurityService.isLoggedIn()) { curUser = User.get(springSecurityService.getCurrentUser().id) }
		else { return false }
		
		def auths = curUser.getAuthorities()
		def dmgStatus = false
		
		auths.each {
			if (it.authority.equals("ROLE_DMG")) {
				dmgStatus = true
			}
		}
		
		//log.info("Is user DMG? ${dmgStatus}")
		
		return dmgStatus
	}
}
