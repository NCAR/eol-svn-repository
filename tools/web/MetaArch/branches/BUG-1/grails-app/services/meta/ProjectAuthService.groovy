package meta

import meta.auth.*

class ProjectAuthService {
	
	def springSecurityService
	CurrentUserService currentUserService
	static boolean transactional = false

	def canEditProject(Project project) {
		def authorized = false
		def principal = currentUserService.lookupUser()
		def uname = principal.username
		
		// Check if user is admin
		authorized = currentUserService.isAdmin()
		if (authorized.equals(true)) {
			// println "returning true for project, user is authorized"
			log.info("canEditProject(project ${project.id}):\tUser is admin")
			return true
		}
		
		// Check if user is a DMG person
		authorized = currentUserService.isDmg()
		if (authorized.equals(true)) {
			// println "returning false for project, user is not authorized"
			log.info("canEditProject(project ${project.id}):\tUser is DMG - not allowed to edit")
			return false
		}
		
		// If the user is not an admin...
		log.info("canEditProject(project ${project.id}):\tnot allowed to edit")
		return false
	}
	
	def canViewProject(Project project) {
		def authorized = false
		def principal = currentUserService.lookupUser()
		def uname = principal.username
		
		// Check if user is admin
		authorized = currentUserService.isAdmin()
		if (authorized.equals(true)) {
			log.info("canViewProject(project ${project.id}):\tUser is admin")
			return true
		} else {
			authorized = currentUserService.isDmg()
			if (authorized.equals(true)) {
				log.info("canViewProject(project ${project.id}):\tUser is DMG")
				return true
			}
		}
		
		// If the user is not an admin...
		User contact = User.findByUsername(uname)   // get user id
		
		// If the user is the creator or point of contact for this dataset...
		if (isaPermittedProject(project)) {
			return true
		}
		// ... else...
		log.info("canViewProject(project ${project.id}):\tnot permitted to view this project")
		return false
	}
	
    def permittedProjects() {
		def allProjects = Project.list()
		def authorized = false
		def myProjects = new ArrayList(allProjects.size())
		def auths = []
		def principal = currentUserService.lookupUser()
		def uname = principal.getUsername()
		
		if (currentUserService.isAdmin() == true || currentUserService.isDmg() == true) {
			// println "returning all projects for list, user is authorized"
			return allProjects.sort()
		}
		
		// If the user is not an admin...
		User contact = User.findByUsername(uname)   // get user id
		
		// Add projects to which this researcher is linked
		myProjects = contact.projects()

		log.debug("returning a selected list of my projects for this user")
		return myProjects.sort()
    }
	
	def isaPermittedProject(Project project) {
		//println "\n in isaPermittedProject to check if can add a dataset to this project"
		//println "\n " << project
		def permitted = false
		if (project == null) {
			log.debug("null project here")
			return false
		}
		
		if (currentUserService.isAdmin() == true) {
			log.debug("got a match on admin status")
			return true
		} else if (currentUserService.isDmg() == true) {
			log.debug("got a match on DMG status")
			return true
		}
		
		def allowedProjects = permittedProjects()
		if ( allowedProjects.contains(project) ) {
			log.debug("got a match on user project list")
			permitted = true
		}
		
		return permitted
	}
}
