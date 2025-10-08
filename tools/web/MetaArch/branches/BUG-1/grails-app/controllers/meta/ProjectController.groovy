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
		} else {
			//println 'show: '+project.beginDate
			//println 'show: '+project.endDate
			return [ projectInstance : project ] 
		}
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
	
	def save = {
		def project = new Project()
		
		if (!currentUserService.isAdmin() && !currentUserService.isDmg()) {
			flash.message = 'I\'m sorry, but you are not authorized to create a project.'
			render(view:'create', model:[projectInstance:project])
		} else {
			TimeZone.setDefault(TimeZone.getTimeZone("UTC"));
			
			project.properties = params
			
			if(!project.hasErrors() && project.save(flush: true)) {
				flash.message = message(code: "project.created.message", args: [project.id])
				redirect(action: 'show', id: project.id)
			}
		}
	}
	
	def update = {
		def project = Project.get( params.id )
		
		if (project) {
			if (!projectAuthService.canEditProject(project)) {
				flash.message = 'I\'m sorry, but you are not authorized to update this project.'
				redirect(action:'show', id: project.id)
			} else {				
				TimeZone.setDefault(TimeZone.getTimeZone("UTC"));
				
				project.properties = params
				
				if(!project.hasErrors() && project.save()) {
					
					flash.message = "Project ${params.id} updated"
					redirect(action: 'show', id: project.id)
				}
			}
		} else {
			flash.message = "Project not found with id ${params.id}"
			redirect(action: 'edit', id: params.id)
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
		def project = Project.get(params.id)
		def per = currentUserService.lookupUser()
			
		if (!project) {
			project = Project.get(params.projectId)
		}
		
		if (currentUserService.isAdmin() == true || isInternalContact(per, project)) {
			def action = params.cmd
			def user = User.findByUsername(params.username)
			
			def mt = MemberType.find{it.name() == params.memberType}
			if (user) {
				if (action == 'add') {
					project.addToProjectGroup(user, mt)
				} else if (action == 'delete') {
					def pm = ProjectMember.findByMemberAndProject(user, project)
					def mList = project.removeFromProjectGroup(pm)
				} else if (action == 'update') {
					// Updates the current project member with a new member type
					/*
					def pm = ProjectMember.findByMemberAndProject(user, project)
					if (pm != null) {
						pm.memberType = mt
						pm.save(flush:true)
					}
					*/
				}
				redirect (action: 'show', id: project.id )
			} else {
				flash.message = 'I\'m sorry, but I was unable to find the following user: ' << params.username
				redirect (action: 'show', id: project.id )
				return
			}
		} else {
			flash.message = 'I\'m sorry, but you are not authorized to modify the chosen project group.'
			redirect (action: 'show', id: project.id )
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
	
	private Boolean isInternalContact(User u, Project p) {
		def memberList = ProjectMember.executeQuery("select pm.member from ProjectMember pm where pm.member = ? and pm.project = ? and pm.memberType = ?", [u, p, MemberType.INTERNAL_CONTACT])
		
		if (memberList != null) {
			 if (memberList.contains(u)) {
				 return true
			 }
		}
		
		return false
	}
}
