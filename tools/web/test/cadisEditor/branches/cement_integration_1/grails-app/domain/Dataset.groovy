class Dataset {

  String title
  Integer nsfAwardNumber 
  String summary
  Date beginDate
  Date endDate
  BigDecimal minimumLatitude = new BigDecimal(-90.0D)
  BigDecimal maximumLatitude = new BigDecimal(90.0D)
  BigDecimal minimumLongitude = new BigDecimal(-180.0D)
  BigDecimal maximumLongitude = new BigDecimal(180.0D)
  Contact piContact
  Contact metadataContact
  Contact datacenterContact
  GcmdScience scienceKeyword
  GcmdLocation locationKeyword
  GcmdProject projectKeyword
  String progress = 'Complete' 
  String spatialType = 'point' 
  String discipline

  static constraints = {
    title(maxSize:255,blank:false,nullable:false)
    summary(maxSize:65535)
    beginDate()
    endDate()
    minimumLatitude(min:new BigDecimal(-90.0D),max:new BigDecimal(90.0D),precision:7,scale:5,nullable:false)
    maximumLatitude(min:new BigDecimal(-90.0D),max:new BigDecimal(90.0D),precision:7,scale:5,nullable:false)
    minimumLongitude(min:new BigDecimal(-180.0D),max:new BigDecimal(180.0D),precision:8,scale:5,nullable:false)
    maximumLongitude(min:new BigDecimal(-180.0D),max:new BigDecimal(180.0D),precision:8,scale:5,nullable:false)
	piContact(nullable:false)
    metadataContact(nullable:false)
    datacenterContact(nullable:true) //XXX
    locationKeyword(nullable:true) //XXX
    projectKeyword(nullable:true) //XXX
    scienceKeyword(nullable:true) //XXX
    progress(inList:["complete", "in work", "planned"])
    spatialType(inList:["grid", "point", "raster", "multiple", "unknown"]) //XXX
  }

  static belongsTo = Contact

  String toString() { title }

// Each PI can have a hierarchy of datasets identified as follows:
//  String toString() { "$piContact: $projectKeyword: $title ($nsfAwardNumber)" }

}
