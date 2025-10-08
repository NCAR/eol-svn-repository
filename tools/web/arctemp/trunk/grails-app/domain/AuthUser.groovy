/**
 * AuthUser for user account
 * @author 
 */
class AuthUser {
	static transients = ["pass"]
	static hasMany=[authorities: Role, projects: Project]
	static belongsTo = Role

	/** Username */
	String username
	/** User Real Name*/
	String userRealName
	/** MD5 Password */
	String passwd
	/** enabled */
	boolean enabled = false

	String email
	boolean email_show = false
	String phoneNumber
	String organization

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

}