package meta

import meta.auth.*

class ProjectMemberController {
	
	def scaffold = ProjectMember
	def springSecurityService
	CurrentUserService currentUserService
	
    //def index() { }
}
