package meta

import java.util.Date;

class File {
	
	static belongsTo = [ dataset: Dataset ]
	
	static mapping = {
		index column: "file_index"
	}
	
	int index
	String name
	String directory // Added during transfer-to-disk for admins
	FileType fileType
	Format format
	int	size = 0 // File size in kilobytes
	//java.io.File file
	
	boolean deleted
	static transients = [ 'deleted' ]
	
	Date dateCreated = new Date()
	Date lastUpdated = new Date()

    static constraints = {
		index(blank:false, min:0)
		name(maxSize: 255)
		directory(blank: true) // We can override this behavior in the controller
		fileType(nullable: true)
		format(nullable: true)
		size(minSize: 0)
		//file(nullable: true, maxSize: 1024 * 1024 * 2) // Limit upload file size to 2MB
    }
	
	String toString() {
		return "${name} (${format}, ${size} kB)"
	}
	
	def beforeDelete() {
		// Try to delete the file on disk, if it exists.
		def fullPath = new java.io.File(this.directory + "/" + this.name)
		
		if (fullPath.exists() && fullPath.canWrite()) {
			// If the file exists on disk and we have write permissions, delete it.
			fullPath.delete()
		}
	}
}
