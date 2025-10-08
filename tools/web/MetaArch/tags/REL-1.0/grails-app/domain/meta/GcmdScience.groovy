package meta

class GcmdScience {

    String keyword
	GcmdScience parent

    static constraints = {
      keyword(nullable:false, unique:true, blank:false)
	  parent(nullable:true)
    }
	
	// We want to return the full path of the GCMD Science Keyword, so we recursively build it!
	String toString() {
		def fullKeyword = keyword
		
		if (parent != null) {
			fullKeyword = parent.toString() + ' > ' + fullKeyword
		}
		
		return fullKeyword
	}
}
