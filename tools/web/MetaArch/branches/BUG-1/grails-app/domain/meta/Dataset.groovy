package meta

import java.util.List;
import java.util.Set;

import org.apache.commons.collections.list.LazyList;
import org.apache.commons.collections.FactoryUtils;

import meta.auth.*

class Dataset {
	
	/*
	 * A clarification on Users:
	 * 	- owner - the creator/owner of the dataset; this field will be renamed to "owner" in the next development phase
	 *  - pointOfContact - the displayed point of contact, whoever is ultimately 
	 *  	responsible for the data and would take credit for it
	 *  
	 * Next Development Phase User Definitions:
	 *  - metadata contact - person filling out the form, automatically entered, since they had to login
     *  - displayed point of contact - the lead PI, or a Co-PI, whoever is ultimately responsible for the data and would take credit for it
     *  - author - all the researchers who would be credited in a citation for the data, and the doi 
	 */
	
	static belongsTo = [ project: Project, owner: User ]
	static hasMany = [ files: File, xlinks: Xlink, datasetCategories: DatasetCategory, datasetPlatforms: DatasetPlatform ]
	
	static mapping = {
		files cascade: "all-delete-orphan" // Be sure to delete all files on disk (if existent)?
		xlinks cascade: "all-delete-orphan"
		//authors cascade: "all-delete-orphan"
	}
	
	String title
	String summary
	
	Integer datasetVersion = 0 // Preliminary datasets are version 0, else first submissions are version 1
	
	User pointOfContact
		
	Date beginDate = new Date()
	Date endDate = new Date()
	BigDecimal minLat = new BigDecimal(-90.0D)
	BigDecimal maxLat = new BigDecimal(90.0D)
	BigDecimal minLon = new BigDecimal(-180.0D)
	BigDecimal maxLon = new BigDecimal(180.0D)
	
	// Funding Information
	String awardNumber
	Integer awardAmount
	Agency fundingAgency
	
	HorizontalResolution horizontalResolution
	VerticalResolution verticalResolution
	
	GcmdScience scienceKeyword
	GcmdLocation locationKeyword
	GcmdPlatform platformKeyword
	GcmdInstrument instrumentKeyword
	IsoTopic topic
	//Format distributionFormat
	
	Long frequency
	
	//Discipline discipline
	String progress = 'complete'
	String spatialType = 'unknown'
	String spatialRepresentation = 'unknown'
	String accessRestrictions = 'No restrictions'
	
	List files = new ArrayList()
	List xlinks = new ArrayList()
	//List authors = new ArrayList()
	
	String language = 'English'
	String metadataVersion = '0.1'
	
	Integer versionNumber
	String dataTags
	
	String comments // For any special instructions regarding these data
	
	String projectMetadata // If there's project-specific metadata, this is where it's stored
	
	boolean isSubmitted = false
	
	// NEED TO ADD A THING TO INDICATE WHEN SOMETHING HAS BEEN ARCHIVED.
	boolean isArchived = false
	
	Date dateCreated = new Date()
	Date lastUpdated = new Date()
	
	// NB: Note that the order in which constraints are listed is the automatic order of the columns in list/show/edit/create.
    static constraints = {
		title(maxSize:255, blank:false, nullable:false)
		summary(maxSize:65535, nullable:true)
		
		datasetVersion(min:0)
		
		owner(nullable:false)
		pointOfContact(nullable:false)
		project(display: false) // Only hides project option in forms
		
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
		minLat(min:new BigDecimal(-90.0D),max:new BigDecimal(90.0D), precision:7, scale:5, nullable:false)
		maxLat(min:new BigDecimal(-90.0D),max:new BigDecimal(90.0D), precision:7, scale:5, nullable:false)
		minLon(min:new BigDecimal(-180.0D),max:new BigDecimal(180.0D), precision:8, scale:5, nullable:false)
		maxLon(min:new BigDecimal(-180.0D),max:new BigDecimal(180.0D), precision:8, scale:5, nullable:false)
		
		fundingAgency(nullable:false)
		awardNumber(nullable:true, blank: true)
		//awardNumber(unique:false, blank:false, nullable:false)
		awardAmount(nullable:true, display: false)
		
		horizontalResolution(nullable:true)
		verticalResolution(nullable:true)
		
		scienceKeyword(nullable:true)
		locationKeyword(nullable:true)
		platformKeyword(nullable:true)
		instrumentKeyword(nullable:true)
		
		topic(nullable:true)
		//distributionFormat(nullable:true)
		
		frequency(nullable:true)
		
		spatialType(inList:["unknown", "grid", "point", "image", "radial", "station", "swath", "trajectory", "transect", "vertical profile", "multiple"], blank:false)
		spatialRepresentation(inList:["unknown", "vector", "grid", "textTable", "tin", "stereoModel", "video"], blank:false)
		progress(inList:["complete", "in work", "planned"], blank:false)
		accessRestrictions(inList:["No restrictions", "Password protected", "EULA required", "Password protected & EULA required"], blank:false)
		dataTags(maxSize:200, nullable:true, blank:true)
		language(nullable:true,blank:false)
		
		comments(nullable:true, blank:true, widget:'textarea', maxSize:65535)
		
		metadataVersion(display:false)
		versionNumber(nullable:true, display:false)
		
		projectMetadata(nullable:true, display:false, maxSize:65535)
    }
	
	def beforeDelete() {
		def cats = DatasetCategory.findAllByDataset(this)
		def plats = DatasetPlatform.findAllByDataset(this)
		def freqs = DatasetFrequency.findAllByDataset(this)
		def dfiles = File.findAllByDataset(this)
		
		cats.each {
			this.removeFromDatasetCategory(it.category)
		}
		
		plats.each {
			this.removeFromDatasetPlatform(it.platform)
		}
		
		// Be sure to delete all author associations!
		DatasetAuthor.removeAll(this)
	}
	
	String toString() { title }
	
	List availableProjects() {
		def curUser = User.get(springSecurityService.principal.id)
		def projects = Project.executeQuery("select distinct pm.project from ProjectMember pm where pm.member = ?", [curUser])
		
		if (curUser.authorities.contains(Authority.findByAuthority('ROLE_ADMIN')) || curUser.authorities.contains(Authority.findByAuthority('ROLE_DEVELOP'))) {
			projects = Project.list()
		}
		println "${projects}"
		
		return projects
	}
	
	// Begin dataset-author Functions
	List<DatasetAuthor> authorList() {
		DatasetAuthor.findAllByDataset(this).collect { it } as List
	}
	
	Set<Author> authors() {
		DatasetAuthor.findAllByDataset(this).collect { it.author } as Set
	}
	
	boolean hasAnyAuthors() {
		return DatasetAuthor.countByDataset(this) > 0
	}
	
	boolean hasAuthor(Author auth) {
		return DatasetAuthor.countByDatasetAndAuthor(this, auth) > 0
	}
	
	/*
	List authors() {
		def dsAuths = authors.sort{it.sortKey}
		def authList = []
		
		dsAuths.each { da ->
			authList.add(da.author)
		}
		
		return authList
	}
	
	List addToAuthors(Author author, int sortKey) {
		DatasetAuthor.link(this, author, sortKey)
		return authors()
	}
	/*
	List removeFromAuthors(Author author) {
		DatasetAuthor.unlink(this, author)
	}*/
	// End dataset-author Functions	
	
	// Begin dataset-category Functions
	List categories() {
		return datasetCategories.collect{it.category}
	}
	
	List addToDatasetCategory(Long category) {
		//Long cat = Category.getId(category)
		DatasetCategory.link(this, category)
		return categories()
	}
	
	List removeFromDatasetCategory(Long category) {
		//Long cat = Category.getId(category)
		DatasetCategory.unlink(this, category)
	}
	// End dataset-category Functions
	
	
	// Begin dataset-platform Functions
	List platforms() {
		return datasetPlatforms.collect{it.platform}
	}
	
	List addToDatasetPlatform(Long platform) {
		//Long plat = Platform.getId(platform)
		DatasetPlatform.link(this, platform)
		return platforms()
	}
	
	List removeFromDatasetPlatform(Long platform) {
		//Long plat = Platform.getId(platform)
		DatasetPlatform.unlink(this, platform)
	}
	// End dataset-platform Functions
}
