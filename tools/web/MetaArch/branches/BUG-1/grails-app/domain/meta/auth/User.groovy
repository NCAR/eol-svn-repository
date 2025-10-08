package meta.auth

import meta.*


class User {

	transient springSecurityService
	
	// Users can have access to multiple projects
	//static hasMany = [authorities: UserAuthority, datasets: Dataset, managedProjects: Project, eolProjects: Project, projectGroups: ProjectGroup]
	//static mappedBy = [datasets: "owner", managedProjects: "pi", eolProjects: "internalContact"]
	static hasMany = [authorities: UserAuthority, datasets: Dataset, projectGroups: ProjectMember]
	static mappedBy = [datasets: "owner"]
	
	// -- Multiple datasources handling --
	static transients = ['beforeInsertRunOnce','beforeUpdateRunOnce']
	boolean beforeInsertRunOnce
	boolean beforeUpdateRunOnce
	boolean isPasswordEncoded
	// -- Multiple datasources handling --
	
	//Author contactInfo	// Covers contact information (name, email, phone, organization, etc.)
	String realname			// Send over to contactInfo
	
	String email			// Send over to contactInfo
	boolean email_show = false
	
	String username
	String password
	
	String phoneNumber		// Send over to contactInfo
	String organization		// Send over to contactInfo
	
	// For custom-theming upon login and other preferred webflows
	Project defaultProject// = this?.projects().get(0) // Will save the first project in the list as the default
	
	boolean enabled = false // Added "= false"
	boolean accountExpired
	boolean accountLocked
	boolean passwordExpired

	static constraints = {
		username(blank: false, unique: true)
		password(blank: false, display: false)
		email(blank: false, email: true, unique: true)
		phoneNumber(nullable: true)
		organization(blank:false)
		defaultProject(blank: true, nullable: true)
	}

	static mapping = {
		password column: '`password`'
	}

	Set<Authority> getAuthorities() {
		UserAuthority.findAllByUser(this).collect { it.authority } as Set
	}

	def beforeInsert() {
		if ( !beforeInsertRunOnce ) {
			beforeInsertRunOnce = true // Necessary for multiple datasources
			encodePassword()
		}
	}
	
	def afterInsert() {
		beforeInsertRunOnce = false // Necessary for multiple datasources
	}

	def beforeUpdate() {
		if (isDirty('password') && !beforeUpdateRunOnce) {
			beforeUpdateRunOnce = true // Necessary for multiple datasources
			encodePassword()
		}
	}
	
	def afterUpdate() {
		beforeUpdateRunOnce = false // Necessary for multiple datasources
	}

	protected void encodePassword() {
		password = springSecurityService.encodePassword(password)
		isPasswordEncoded = true
	}
	
	// Begin Project-Group Functions
	List projects() {
		return projectGroups.collect{it.project}
	}

	List addToProjectGroup(Project project, MemberType memberType) {
		ProjectMember.link(this, project, memberType)
		return projects()
	}

	List removeFromProjectGroup(ProjectMember pm) {
		def project = pm.project
		ProjectMember.unlink(this, project)
		return projects()
	}
	// End Project-Group Functions
	
	String toString() {
		return "${username} (${realname})"
	}
}
