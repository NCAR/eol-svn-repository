package meta

import meta.auth.*

class Project {
	
	static belongsTo = User
	static hasMany = [projectMembers: ProjectMember, datasets: Dataset]

	String name
	String fullName
	String summary
	
	// Funding Information
	String awardNumber
	Integer awardAmount
	Agency fundingAgency
	
	Date beginDate = new Date()
	Date endDate = new Date()
	BigDecimal minLat = new BigDecimal(-90.0D)
	BigDecimal maxLat = new BigDecimal(90.0D)
	BigDecimal minLon = new BigDecimal(-180.0D)
	BigDecimal maxLon = new BigDecimal(180.0D)
	
	//User internalContact // Used only for EOL purposes
	//User pi // The mapping for ProjectGroup
	String cssUrl
	String docGuideUrl
	String logo
	XmlTemplate additionalMetadata // Must be in XML format for the XmlMageService to handle
	
	Date dateCreated = new Date()
	Date lastUpdated = new Date()
	
	// NB: Note that the order in which contstraints are listed is the automatic order of the columns in list/show/edit/create.
    static constraints = {
		name(maxSize:255, blank:false, nullable:false)
		fullName(maxSize:255, blank:false, nullable:false)
		
		beginDate(
			nullable:false, 
			validator: { val, obj ->
				if ( val?.after(obj.endDate) ) return ['invalid.range', obj.endDate]
			}
		)
		endDate(
			nullable:false, 
			validator: { val, obj ->
				if ( val?.before(obj.beginDate) ) return ['invalid.range', obj.beginDate]
			}
		)
		
		fundingAgency(nullable:false)
		awardNumber(unique:false, blank:false, nullable:false)
		awardAmount(nullable:true)
		
		summary(maxSize:65535, nullable:true)
		minLat(min:new BigDecimal(-90.0D),max:new BigDecimal(90.0D), precision:7, scale:5, nullable:true)
		maxLat(min:new BigDecimal(-90.0D),max:new BigDecimal(90.0D), precision:7, scale:5, nullable:true)
		minLon(min:new BigDecimal(-180.0D),max:new BigDecimal(180.0D), precision:8, scale:5, nullable:true)
		maxLon(min:new BigDecimal(-180.0D),max:new BigDecimal(180.0D), precision:8, scale:5, nullable:true)
		
		cssUrl(maxSize:65535, nullable:true)//, url: true)
		docGuideUrl(nullable: true, url: true)
		logo(nullable: true, blank: true, url: true)
		additionalMetadata(nullable: true)
    }
	
	// Begin Project-Group Functions
	List members() {
		return projectMembers.collect{it.member}
	}
	
	List addToProjectGroup(User member, MemberType memberType) {
		ProjectMember.link(member, this, memberType)
		return members()
	}
	
	List removeFromProjectGroup(ProjectMember pm) {
		def member = pm.member
		ProjectMember.unlink(member, this)
		return members()
	}
	// End Project-Group Functions
	
	String toString() { name }
}
