class IsoTopic { 
	String keyword

  	static belongsTo = Dataset
  	String toString() { keyword }
        static constraints = {
	 keyword(nullable:false,unique:true,blank:false)
	}
}	
