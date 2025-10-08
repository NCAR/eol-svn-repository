package meta

import meta.auth.*
import java.util.Date;

class TempFile {
	
	String name
	String cType // File content-type
	byte[] data
	
	User owner
	Date dateCreated = new Date()
	Date lastUpdated = new Date()

    static constraints = {
		name(maxSize: 255)
		cType(maxSize: 255)
		data(maxSize: 1024 * 1024 * 4) // Limit upload file size to 4MB
    }
}
