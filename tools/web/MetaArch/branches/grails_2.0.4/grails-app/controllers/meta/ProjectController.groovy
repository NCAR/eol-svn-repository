package meta

import meta.auth.*

class ProjectController {

	def scaffold = Project
	def springSecurityService
	CurrentUserService currentUserService
	ProjectAuthService projectAuthService
	
	def show = {
		def project = Project.get( params.id )
		def isAdmin = currentUserService.isAdmin()
		
		//log.info "\nProject Show: user is admin? ${isAdmin}\n"

		if (!projectAuthService.canViewProject(project)) {
			flash.message = 'I\'m sorry, but you are not authorized to view that project.'
			def myList = projectAuthService.permittedProjects()
			if (!myList?.size()) flash.message += ' I\'m sorry, but you are not authorized for any projects.'
			redirect(action:list)
		}
		
		if(!project) {
			flash.message = "Project not found with id ${params.id}"
			redirect(action:list)
		} else { return [ projectInstance : project ] }
	}
	
	def create = {
		def project = new Project()
		def isAdmin = currentUserService.isAdmin()
		
		if (isAdmin == false) {
			flash.message = 'I\'m sorry, but you are not authorized to create a project.'
			redirect(action:list)
		}
		
		project.properties = params
		
		return [projectInstance: project]
	}
	
	def edit = {
		def project = Project.get( params.id )
		if(!project) {
			flash.message = "Project not found with id ${params.id}"
			redirect(action: 'list')
		} else {
			if (!projectAuthService.canEditProject(project)) {
				flash.message = 'I\'m sorry, but you are not authorized to edit this project.'
				redirect(action:'show', id: project.id)
			} else {
				return [ projectInstance : project ]
			}
		}
	}
	
	def list() {
		def curUser = currentUserService.lookupUser()
		def isAdmin = currentUserService.isAdmin()
		
		//log.info "\nProject List: user is admin? ${isAdmin}\n"

		if (!curUser) {
			//redirect controller: 'login'
		}
		if (curUser) {
			def projects
			if ( isAdmin == false ) {
				// Regular users can only see projects they own or are associated with
				projects = currentUserService.currentUserProjects()
			} else {
				// Admins and Developers can see all projects
				projects = Project.all
			}
			
			//print "${projects}!!!!"
			def projectTotal = projects.count(projects)
			
			if (!projects) { flash.message = "No projects currently available." }
			
			return [ projectInstanceList: projects, projectInstanceTotal: projectTotal ]
		}
	}
	
	/*def save() {
		def project = new Project(params)
		// Modify this for ProjectGroup Members
		//def creator = params.creator
		//if(!creator) {
		//	project.creator = currentUserService.lookupUser()
		//	creator = currentUserService.lookupUser()
		//}
		//if(!params.internalContact) {
		//	project.internalContact = currentUserService.lookupUser()
		//}
		
		project.save()
		//permitUser(creator.username, project.id)  // It already adds them to the managedProjects list
		
		redirect (action: 'list')
	}*/
	
	/**/
	// To get a nice button in edit view
	def cancel = {
		redirect(action:show, id:params.id)
	}
	/**/
	
	def modifyGroup = {
		if (currentUserService.isAdmin() == true) {
			def action = params.cmd
			def user = User.findByUsername(params.username)
			def project = Project.get(params.id)
			
			if (!project) {
				project = Project.get(params.projectId)
			}
			
			def mt = MemberType.find{it.name() == params.memberType}
			if (user) {
				if (action == 'add') {
					project.addToProjectGroup(user, mt)
				} else if (action == 'delete') {
					def pm = ProjectMember.findByMemberAndProject(user, project)
					def mList = project.removeFromProjectGroup(pm)
				} else if (action == 'update') {
					// Update the user's memberType here
				}
				//project.save()
				redirect (action: 'show', id: project.id )
			} else {
				flash.message = 'I\'m sorry, but I was unable to find the following user: ' << params.username
				redirect (action: 'list')
				return
			}
		} else {
			flash.message = 'I\'m sorry, but you are not authorized to modify the chosen project group.'
			redirect (action: 'list')
			return
		}
	}
	
	def permitUser = {
		if (currentUserService.isAdmin()) {
			def user = User.findByUsername(params.username)
			def project = Project.get(params.id)
			if (user) {
				user.addToProjectGroup(project)
				user.save()
			}
		} else {
			flash.message = 'I\'m sorry, but you are not authorized to add members to chosen project group.'
			redirect (action: 'list')
			return
		}
	}
}
