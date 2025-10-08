package meta

class Requirement {
	/* Requirements are fields that are enforced as required 
	 * on a by-project basis for individual data set submissions.
	 */
	
	Project project // The project that this requirement is associated with
	String field // The field that is being required with a specific constraint
	//String rule // The rule that must be enforced prior to submission

    static constraints = {
		project(nullable: false)
		field(maxSize:255, blank:false, nullable:false)
		//rule(maxSize:65535)
    }
	
	String toString() { project.name + " requires " + field }
}
