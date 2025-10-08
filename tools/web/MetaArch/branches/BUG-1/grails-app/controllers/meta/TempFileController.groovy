package meta

import meta.auth.*

class TempFileController {

	def scaffold = TempFile
	
	CurrentUserService currentUserService
	
	def adownload() {
		def tFile = TempFile.get(params.id)
		def per = currentUserService.lookupUser()
		
		if (tFile == null) {
			response << ""
		} else {
			def fname = tFile.name
			def contentType = tFile.cType
			def data = tFile.data
			
			tFile.delete(flush: true) // Try to delete this object now that it's about to be downloaded!
			cleanUpExpiredFiles(per)		
			
			response.contentType = contentType
			response.addHeader("Content-disposition", "attachment; filename=" + fname)
			response.contentLength = (data).length
			response.outputStream << data
			response.outputStream.flush()
			response.outputStream.close()
		}
	}
	
	def cleanUpExpiredFiles(User user) {
		// Find all expired tempFile objects that this user has created and delete them!
		println "Found me!"
		def allFiles = TempFile.findAllByOwner(user)
		def expiredFiles = []
		def now = new Date()
		
		allFiles.each { f ->
			def expireDate = (f.dateCreated).plus(2) // Expires 48 hours after creation
			if ( expireDate <= now ) {
				// If the expiration date has occurred before now, then the file has expired.
				expiredFiles.add(f)
			}
		}
		
		expiredFiles.each { ef ->
			ef.delete(flush: true)
		}
	}
}
