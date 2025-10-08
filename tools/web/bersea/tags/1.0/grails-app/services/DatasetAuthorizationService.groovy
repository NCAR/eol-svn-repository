class DatasetAuthorizationService {

	AuthenticateService authenticateService

    static boolean transactional = false

    def canEditDataset(Dataset dataset) {
		// println "\n in canEditDataset to see if user is authorized for this dataset"
		def authorized = false
		def auths = []
     	def principal = authenticateService.principal()
      	def uname = principal.getUsername()			// get username
      	auths = principal.getAuthorities()			//get authorities
		auths.each {
			// println (it)
			if ((it.toString()).equals("ROLE_ADMIN")) {
				// println "got a match on ROLE_ADMIN"
				authorized = true
			} 
		}
        Researcher contact = Researcher.findByUsername(uname)	// get user id
		// println uname
		// println contact

    	if (isAllowedContact(dataset, contact, authorized)) { 
			return true
    	} else { 
        	log.debug("not a contact who can edit") 
        	return false 
      	} 

		log.debug( "Not supposed to get to this line")
    }


    def isAllowedContact(Dataset dataset, Researcher contact, Boolean authorized) {
		// println "\n in isAllowedContact, with dataset = ${dataset}, researcher = ${contact}, and authorized = ${authorized}"
      	if (!dataset || !contact) {
           log.debug("NO match on the contact id which is ${contact.id}")
           authorized = false 
      	}

      	// println "metadata contact: ${dataset.metadataContact.id}"
	  	// println "contact: ${contact.id}"
      	// println "pi: ${dataset.project.pi.id}"

		// check if user is a named contact for this dataset
      	if ( (dataset.metadataContact.id == contact.id) ||
           (dataset.project.pi.id == contact.id) ) {
       		log.debug("matched the contact id which is ${contact.id}")
			authorized = true
      	}

		// check to see if user is linked to the project
		def allProjlinks = Projlink.list()
		allProjlinks.each { projectlink ->
			// println "\n in each projectlink, projectlink = ${projectlink}"
			if ( (projectlink.project.id.equals(dataset.project.id)) && (projectlink.researcher.id.equals(contact.id)) ) {
				log.debug("user ${contact} is an allowed contact for the dataset")
				authorized = true
			}
		}

      	return authorized
	}

}
