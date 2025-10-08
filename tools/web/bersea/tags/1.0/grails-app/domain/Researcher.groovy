/**
 * Researcher for user account
 */
class Researcher {
	static transients = ["pass"]
	static hasMany = [authorities: Role, metadataDatasets: Dataset, managedProjects: Project, projlinks: Projlink]
	static mappedBy = [metadataDatasets: "metadataContact", managedProjects: "pi"]
	static belongsTo = Role

	String username
	String userRealName
	String passwd
	boolean enabled = false
    String phoneNumber
    String organization

	String email
	boolean email_show = false

	/** description */
	String description=""

	/** plain password to create a MD5 password */
	String pass="[secret]"

	static def constraints = {
		username(blank:false,unique:true)
		userRealName(blank:false)
		passwd(blank:false)
		enabled()
        email(email:true)
        phoneNumber(nullable:true)
        organization(blank:false)
	}

	List projects() {
		return projlinks.collect{it.project}
	}

	List addToProjects(Project project) {
		Projlink.link(this, project)
		return projects()
	}

	List removeFromProjects(Project project) {
		Projlink.unlink(this, project)
		return projects()
	}

	String toString() { userRealName }
}