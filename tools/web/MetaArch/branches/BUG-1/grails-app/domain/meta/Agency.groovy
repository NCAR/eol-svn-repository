package meta

import meta.auth.*

class Agency {
	// "NOAA", "NPRB", "NSF", "NASA", "Other"
	
	String name // Abbreviation
	String description // Full name

    static constraints = {
		name(unique: true, blank:false, nullable:false)
		description(nullable:true)
    }
	
	String toString() { name }
}
