package meta

import java.util.Date;

class Xlink {
	
	static belongsTo = [ dataset: Dataset ]
	
	static mapping = {
		index column: "xlink_index"
	}
	
	int index
	String href
	String title
	XlinkType type = XlinkType.INFO
	
	boolean deleted
	static transients = [ 'deleted' ]
	
	Date dateCreated = new Date()
	Date lastUpdated = new Date()

    static constraints = {
		//unique:['href', 'type']
		index(blank:false, min:0)
		title(maxSize:255, blank:false)
		type()
		href(maxSize : 4096, widget: 'textField')
    }
	
	String toString() { return "(${type}) ${href}" }
}
