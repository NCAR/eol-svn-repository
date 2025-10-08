class ProjectAuthorizationService {

	AuthenticateService authenticateService

    static boolean transactional = false

    def permittedProjects() {
		// println "\n in permittedProjects to get a list of projects for the dropdown on the dataset create page"
        def allProjects = Project.list()
		def authorized = false
        def myProjects = new ArrayList(allProjects.size())
		def auths = []
     	def principal = authenticateService.principal()
      	def uname = principal.getUsername()			// get username
		// check if user is admin
      	auths = principal.getAuthorities()			//get authorities
		auths.each {
			// println (it)
			if ((it.toString()).equals("ROLE_ADMIN")) {
				log.debug("got a match on ROLE_ADMIN")
				authorized = true
			}
		}

        if (authorized.equals(true)) {
			// println "returning all projects for list, user is authorized" 
			return allProjects.sort()
		}

        Researcher contact = Researcher.findByUsername(uname)	// get user id
		// println uname
		// println contact

		// add projects for which this researcher is PI
		// println "in permittedProjects - PIs"
        allProjects.each { project ->
		//	println "checking pi = ${project.pi.id} against project = ${project}"
			if (contact.id == project.pi.id) {
				log.debug("adding ${project}")
            	myProjects.add(project)
			}
		}

		// add projects to which this researcher is linked
		def allProjlinks = Projlink.list()
		allProjlinks.each { projectlink ->
			// println "in each projlink, permittedProjects, projectlink = ${projectlink.id}, researcher = ${projectlink.researcher.id}"
			if (projectlink.researcher.id.equals(contact.id)) {
				log.debug("adding ${projectlink.project}")
            	myProjects.add(projectlink.project)
			}
		}

		log.debug("returning a selected list of my projects for this user")
        return myProjects.sort()
    }


    def isaPermittedProject(Project project) {
		// println "\n in isaPermittedProject to check if can add a dataset to this project"
		def allProjlinks = Projlink.list()
		def auths = []
      	def permitted = false
      	if (project == null) {
			// println "null project here"
			return false
		}
     	def principal = authenticateService.principal()
      	def uname = principal.getUsername()						// get username
      	auths = principal.getAuthorities()			//get authorities

		auths.each {
			// println (it)
			if ((it.toString()).equals("ROLE_ADMIN")) {
				log.debug("got a match on ROLE_ADMIN")
				permitted = true
			}
		}

        Researcher contact = Researcher.findByUsername(uname)	// get user id
		// println uname
		// println contact
		// check if user is PI for the project
		if (contact.id == project.pi.id) {
				log.debug("okay, user is PI of project")
				permitted = true
		}

		// check to see if user is linked to the project
		allProjlinks.each { projectlink ->
			// println "\n in each projectlink, isaPermittedProject, projectlink = ${projectlink.id}, researcher = ${projectlink.researcher.id}"
			if ( (projectlink.project.id.equals(project.id)) && (projectlink.researcher.id.equals(contact.id)) ) {
				permitted = true
			}
		}
      	return permitted
	}

}
