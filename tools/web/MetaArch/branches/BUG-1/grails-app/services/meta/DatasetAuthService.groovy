package meta

import meta.auth.*

class DatasetAuthService {
	
	def springSecurityService
	CurrentUserService currentUserService
	static boolean transactional = false

    def canEditDataset(Dataset dataset) {
		// println "\n in canEditDataset to see if user is authorized for this dataset"
		def authorized = false
		def principal = currentUserService.lookupUser()
		def uname = principal.username
		
		// Check if user is admin
		authorized = currentUserService.isAdmin()
		if (authorized.equals(true)) {
			// println "returning true for dataset, user is authorized"
			log.info("canEditDataset(dataset ${dataset.id}):\tUser is admin")
			return true
		}
		
		// If the user is not an admin...
		User contact = User.findByUsername(uname)   // get user id
		// println uname
		// println contact
		
		// Check if user is a DMG person
		authorized = currentUserService.isDmg()
		if (authorized.equals(true)) {
			// Unless they created/are-contact-for this dataset...
			String logStr = "canEditDataset(dataset ${dataset.id}):\tUser is DMG"
			
			if (dataset.owner.id == contact.id || dataset.pointOfContact.id == contact.id) {
				log.info(logStr+" - editable")
				return true
			}
			
			log.info(logStr+" - not editable")
			return false
		}
		
		// If user is neither admin nor DMG...
		if (isAllowedContact(dataset, contact, authorized)) {
			return true
		} else {
			log.info("canEditDataset(dataset ${dataset.id}):\tnot a contact who can edit")
			return false
		}

		log.debug( "Not supposed to get to this line")
	}
	
	def canViewDataset(Dataset dataset) {
		// println "\n in canEditDataset to see if user is authorized for this dataset"
		def authorized = false
		def principal = currentUserService.lookupUser()
		def uname = principal.username
		
		// Check if user is admin
		authorized = currentUserService.isAdmin()
		if (authorized.equals(true)) {
			log.info("canViewDataset(dataset ${dataset.id}):\tUser is admin")
			return true
		} else {
			authorized = currentUserService.isDmg()
			if (authorized.equals(true)) {
				log.info("canViewDataset(dataset ${dataset.id}):\tUser is DMG")
				return true
			}
		}
		
		// If the user is not an admin...
		User contact = User.findByUsername(uname)   // get user id
		
		// If the user is the creator or point of contact for this dataset...
		if (dataset.owner == contact || dataset.pointOfContact == contact) {
			return true
		}
		
		// If the user is not the creator or point of contact for this dataset...
		def membership = ProjectMember.findByMemberAndProject(contact, dataset.project)
		if (membership != null && (membership.memberType == MemberType.PI || membership.memberType == MemberType.INTERNAL_CONTACT || membership.memberType == MemberType.MANAGER)) {
			// If membership is an internal contact, principal investigator or project manager, all project datasets are viewable
			return true
		} else {
			log.info("canViewDataset(dataset ${dataset.id}):\tnot a contact who can view")
			return false
		}
		
		log.debug( "Not supposed to get to this line")
	}


	def isAllowedContact(Dataset dataset, User contact, Boolean authorized) {
		// println "\n in isAllowedContact, with dataset = ${dataset}, researcher = ${contact}, and authorized = ${authorized}"
		if (!dataset || !contact) {
		   log.debug("NO match on the contact id which is ${contact.id}")
		   authorized = false
		}
		
		// Check if user is a named contact for this dataset
		if ( dataset.owner.id == contact.id ) {
			log.debug("matched the owner id which is ${contact.id}")
			authorized = true
		} else if ( dataset.pointOfContact.id == contact.id ) {
			log.debug("matched the point of contact id which is ${contact.id}")
			authorized = true
		}

		// Check to see if user is linked to the project
		/*
		def membership = ProjectMember.findByMemberAndProject(contact, dataset.project)
		if (membership != null && membership.memberType == MemberType.PI) {
			log.debug("user ${contact} is an allowed contact for the dataset")
			authorized = true
		}
		*/
		
		/*
		allMembers.each { projectMember ->
			// println "\n in each projectlink, projectlink = ${projectMember}"
			if ( (projectMember.project.id.equals(dataset.project.id)) && (projectMember.memberType.id.equals(contact.id)) ) {
				log.debug("user ${contact} is an allowed contact for the dataset")
				authorized = true
			}
		}
		*/

		return authorized
	}
}
