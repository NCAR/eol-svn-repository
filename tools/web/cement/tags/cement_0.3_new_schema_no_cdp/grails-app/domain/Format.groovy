class Format { 
	String name
	String description

  	static belongsTo = Dataset
  	String toString() { name }

  static constraints = {
    name(unique:true,blank:false)
    description(nullable:true)
  }

}	
