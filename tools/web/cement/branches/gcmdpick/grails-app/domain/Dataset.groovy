class Dataset {
  String title
  String entryID
  String summary
  Date beginDate
  Date endDate
  BigDecimal minimumLatitude = new BigDecimal(-90.0D)
  BigDecimal maximumLatitude = new BigDecimal(90.0D)
  BigDecimal minimumLongitude = new BigDecimal(-180.0D)
  BigDecimal maximumLongitude = new BigDecimal(180.0D)

  // now in Project: // Contact piContact // 1: "THE" P.I. that matches 1:1 with nsfAwardNumber ; can edit this
  Contact metadataContact // n: can edit this
  // ??? Contact pointOfContact // 1: displayed POC
  Contact datacenterContact // 1: where data files live

  GcmdScience scienceKeyword
  GcmdLocation locationKeyword
  GcmdPlatform platformKeyword
  GcmdInstrument instrumentKeyword
  IsoTopic topic

  Project project
  String progress = 'complete' 
  String spatialType 
  String discipline
  String dataAccess
  String accessRestrictions = 'Unrestricted access'
  String useConstraints = 'No user constraints'
  Format distributionFormat
  String datasetLanguage = 'English'
  String metadataName = 'CADIS profile'
  String metadataVersion = '0.1'

  Date dateCreated
  Date lastUpdated
  String frequency
  String resolution
  String dataTags

  static constraints = {
    title(maxSize:255,blank:false,nullable:false)
    entryID(unique:true,blank:false,nullable:false)
    summary(maxSize:65535, nullable:true)
    beginDate(nullable:false)
    endDate(nullable:false)
    minimumLatitude(min:new BigDecimal(-90.0D),max:new BigDecimal(90.0D),precision:7,scale:5,nullable:false)
    maximumLatitude(min:new BigDecimal(-90.0D),max:new BigDecimal(90.0D),precision:7,scale:5,nullable:false)
    minimumLongitude(min:new BigDecimal(-180.0D),max:new BigDecimal(180.0D),precision:8,scale:5,nullable:false)
    maximumLongitude(min:new BigDecimal(-180.0D),max:new BigDecimal(180.0D),precision:8,scale:5,nullable:false)
    metadataContact(nullable:false)
    datacenterContact(nullable:false)
    progress(inList:["complete", "in work", "planned"])
    spatialType(inList:["unknown", "grid", "point", "image", "radial", "station", "swath", "trajectory", "multiple"])
    accessRestrictions(nullable:true,blank:false)
    dataAccess(nullable:true,blank:true)
    datasetLanguage(nullable:true,blank:false)
    discipline(inList:["Atmosphere","Human Dimensions","Hydrology and Terrestrial Cryosphere","Ocean and Sea Ice","Terrestrial Ecosystems", "Supporting Data Sets"])
    useConstraints(nullable:true,blank:false)
    frequency(inList:["unknown", "less than 1 second", "1 sec. to 1 min.", "1 min. to 1 hour", "hourly to daily", "daily to weekly", "weekly to monthly", "monthly to annual", "annual", "decadal", "climate normal (30-year climatology)"])
    resolution(inList:["unknown", "point", "less than 1 meter", "1 meter to 30 meters", "30 meters to 100 meters", "100 meters to 250 meters", "250 meters to 500 meters", "500 meters to 1 km", "1 kilometer", "1 km to 10 km", "10 km to 50km", "50 km to 100 km", "100 km to 250 km", "250 km to 500 km", "500 km to 1000 km", "greater than 1000 km"]) 
    dataTags(maxSize:255,nullable:true,blank:true)
  }

  // with these, Grails 1.0 will cascade save/validate from Contact (don't want that)
  //static belongsTo = Contact
  //static belongsTo = Project

  String toString() { title }

// Each PI can have a hierarchy of datasets identified as follows:
//  String toString() { "$piContact: $projectKeyword: $title ($nsfAwardNumber)" }

  String theDisciplineEntryID() {
    this.discipline.replaceAll("[^A-Za-z0-9_-]","_")
  }

  String theTitleEntryID() {
    this.title.replaceAll("[^A-Za-z0-9_-]","_")
  }


}
