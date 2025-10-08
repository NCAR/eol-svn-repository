
class DatasetAuthorizationService {

    static boolean transactional = false


    def canEditDataset(Dataset dataset, String username, cdpPrivileges) {
      Contact contact = Contact.findByShortName(username)
      if (!contact) return false
      if (contact.isAdmin) return true
      return (isAllowedContact(dataset, contact) && hasCdpPrivileges(dataset,cdpPrivileges))
//    if (isAllowedContact(dataset, contact)) {
//        if (hasCdpPrivileges(dataset, cdpPrivileges)) {
//            log.debug("got privileges from CDP")
//            return true
//        } else {
//            log.debug("no cdpPrivileges")
//            return false
//        }
//    } else {
//        log.debug("not a contact to who can edit")
//        return false
//      }
    }


    def hasCdpPrivileges(Dataset dataset,  cdpPrivileges) {
      def privFlag = false
      if (!dataset) {
        log.debug("not a good dataset; it is ${dataset}")
        return false
      }
      if (cdpPrivileges == null) return false

      cdpPrivileges?.each {
        log.debug("our cdpPrivilege = ${it}")
        log.debug("our dataset entryid = ${dataset.entryID}")
        if (it.equals(dataset.entryID)) {
            log.debug("match on entry ID")
            privFlag = true
            return true // just ends each closure
        } else if (dataset.entryID.startsWith(it + '.')) {
            log.debug("match on beginning of entry ID")
            privFlag = true
            return true // just ends each closure
        } else {
            log.debug("NO match to entry ID for privileges")
          }
      }

      log.debug("priv flag = ${privFlag}")
      return privFlag
    }


    def isAllowedContact(Dataset dataset, Contact contact) {
      if (!dataset || !contact) {
           log.debug("NO match on the contact id which is ${contact.id}")
           return false
      }

      if ( (dataset.metadataContact.id == contact.id) ||
           (dataset.datacenterContact.id == contact.id) ||
           (dataset.project.piContact.id == contact.id)
         ) {
           log.debug("matched the contact id which is ${contact.id}")
           return true
      }

      return false
      }

}
