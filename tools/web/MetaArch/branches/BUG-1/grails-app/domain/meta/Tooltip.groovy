package meta
import java.util.Date;

class Tooltip {
	
	String domain
	String property
	String title
	String description
	
	Date dateCreated = new Date()
	Date lastUpdated = new Date()

    static constraints = {
		domain(maxSize:255, blank:false, nullable:false)
		property(maxSize:255, blank:false, nullable:false)
		
		title(maxSize:255, blank:false, nullable:false)
		description(maxSize:65535, nullable:true)
    }
}
