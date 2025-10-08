
class DatasetAuthorizationService {
	
    static boolean transactional = true

    def check(Dataset dataset, String username) {
      Contact me = Contact.findByShortName(username)
      if (!me) return false

      if ( (dataset.metadataContact.id == me.id) ||
           (dataset.datacenterContact.id == me.id) ||
           (dataset.project.piContact.id == me.id)
         )
           return true

      return false
      }

}
