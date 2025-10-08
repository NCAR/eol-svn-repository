class Resolution {
	String scale

  	String toString() { scale }

    static constraints = {
	  scale(nullable:false, unique:true, blank:false)
	}
}
