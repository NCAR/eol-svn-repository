class Format { 
	String name
	String description

  	String toString() { name }

	static constraints = {
    	name(unique:true,blank:false)
    	description(nullable:true)
  	}

}	
