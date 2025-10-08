class Frequency {
	String temporal_resolution

  	String toString() { temporal_resolution }

    static constraints = {
	  temporal_resolution(nullable:false, unique:true, blank:false)
	}
}
