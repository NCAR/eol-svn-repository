package meta

class XmlTemplate {
	
	// Needs to know the original filename (if uploaded)
	// Needs to know the contents of the file (action that reads XML and pretty-prints it to the screen
	
	String filename
	String body
	byte[] file // Optional to upload the file rather than copy-paste the text
	int	size = 0
	
	Date dateCreated = new Date()
	Date lastUpdated = new Date()
	
	static mapping = {
		body type: "text"
	}

    static constraints = {
		filename(maxSize: 255) // Restrict file extensions to ".xml" only
		file(nullable: true, maxSize: 1024 * 1024 * 2) // Limit upload file size to 2MB
		body(blank: true, nullable: true, widget: 'textarea')
		size(minSize: 0)
    }
	
	String toString() {
		return "$filename ($id)"
	}
}
