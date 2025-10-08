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
  String spatialType = 'point' 
  String discipline
  String dataAccess
  String accessRestrictions = 'Unrestricted access'
  String useConstraints = 'No user constraints'
  Format distributionFormat
  String datasetLanguage = 'English'
  String metadataName = 'CADIS profile'
  String metadataVersion = '0.1'

  Date creationDate
  Date revisionDate

  static constraints = {
    title(maxSize:255,blank:false,nullable:false)
    entryID(unique:true,blank:false,nullable:false)
    summary(maxSize:65535)
    beginDate()
    endDate()
    minimumLatitude(min:new BigDecimal(-90.0D),max:new BigDecimal(90.0D),precision:7,scale:5,nullable:false)
    maximumLatitude(min:new BigDecimal(-90.0D),max:new BigDecimal(90.0D),precision:7,scale:5,nullable:false)
    minimumLongitude(min:new BigDecimal(-180.0D),max:new BigDecimal(180.0D),precision:8,scale:5,nullable:false)
    maximumLongitude(min:new BigDecimal(-180.0D),max:new BigDecimal(180.0D),precision:8,scale:5,nullable:false)
    //piContact(nullable:false)
    metadataContact(nullable:false)
    datacenterContact(nullable:false)
    progress(inList:["complete", "in work", "planned"])
    spatialType(inList:["grid", "point", "image", "radial", "station", "swath", "trajectory", "multiple", "unknown"])
    accessRestrictions(nullable:true,blank:false)
    dataAccess(nullable:true,blank:false)
    datasetLanguage(nullable:true,blank:false)
    discipline(inList:["Atmosphere","Human Dimensions","Hydrology and Cryosphere","Ocean And Sea Ice","Terrestrial Ecosystems"])
    useConstraints(blank:false)

  }

  // with these, Grails 1.0 will cascade save/validate from Contact (don't want that)
  //static belongsTo = Contact
  //static belongsTo = Project

  String toString() { title }

// Each PI can have a hierarchy of datasets identified as follows:
//  String toString() { "$piContact: $projectKeyword: $title ($nsfAwardNumber)" }

  String theDisciplineEntryID() {
    this.discipline.toLowerCase().replaceAll("[^A-Za-z0-9_-]","_")
  }

  String theTitleEntryID() {
    this.title.toLowerCase().replaceAll("[^A-Za-z0-9_-]","_")
  }


}
