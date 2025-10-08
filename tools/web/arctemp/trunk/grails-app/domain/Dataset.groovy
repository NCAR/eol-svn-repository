class Dataset {
  static belongsTo = Project

  String title
  String cruise
  String summary
  Date beginDate
  Date endDate
  BigDecimal minimumLatitude = new BigDecimal(-90.0D)
  BigDecimal maximumLatitude = new BigDecimal(90.0D)
  BigDecimal minimumLongitude = new BigDecimal(-180.0D)
  BigDecimal maximumLongitude = new BigDecimal(180.0D)
  Frequency frequency
  Resolution resolution

  GcmdScience scienceKeyword
  GcmdLocation locationKeyword
  GcmdPlatform platformKeyword
  GcmdInstrument instrumentKeyword
  IsoTopic topic

  Project project
  Discipline discipline
  String progress = 'complete' 
  String spatialType 
  String dataAccess
  String accessRestrictions = 'No restrictions'
  Format distributionFormat
  String xlink1
  XlinkType xlink1Purpose
  String xlink2
  XlinkType xlink2Purpose
  String datasetLanguage = 'English'
  String metadataName = 'BEST profile'
  String metadataVersion = '0.3'

  Date dateCreated
  Date lastUpdated
  Integer versionNumber  
  String dataTags

  static constraints = {
	project()
    title(maxSize:255, blank:false, unique:true, nullable:false) 
	cruise(nullable:true, blank:false)
    summary(maxSize:65535, nullable:true)
    beginDate(nullable:false)
    endDate(nullable:false)
    minimumLatitude(min:new BigDecimal(-90.0D), max:new BigDecimal(90.0D), precision:7, scale:5, nullable:false)
    maximumLatitude(min:new BigDecimal(-90.0D), max:new BigDecimal(90.0D), precision:7, scale:5, nullable:false)
    minimumLongitude(min:new BigDecimal(-180.0D), max:new BigDecimal(180.0D), precision:8, scale:5, nullable:false)
    maximumLongitude(min:new BigDecimal(-180.0D), max:new BigDecimal(180.0D), precision:8, scale:5, nullable:false)
	discipline()
	locationKeyword()
	frequency()
    spatialType(inList:["unknown", "grid", "point", "image", "radial", "station", "swath", "trajectory", "transect", "vertical profile", "multiple"])
	resolution()
	platformKeyword()
	instrumentKeyword()
	scienceKeyword()
	topic()
    dataAccess(url:true, nullable:true)
    dataTags(maxSize:255, nullable:true, blank:true)
	versionNumber(nullable:true)
	distributionFormat()
    progress(inList:["complete", "in work", "planned"])
	accessRestrictions(inList:["No restrictions", "Password protected for BEST/BSIERP use"])

    xlink1(validator: { val, obj ->
     if (
     ((val != null) && (obj.properties['xlink1Purpose'] == null))
      ||
     ((val == null) && (obj.properties['xlink1Purpose'] != null))
     ) return ['invalid.correlation'] },
     url:true,nullable:true)

    xlink2(validator: { val, obj ->
     if (
     ((val != null) && (obj.properties['xlink2Purpose'] == null))
      ||
     ((val == null) && (obj.properties['xlink2Purpose'] != null))
     ) return ['invalid.correlation'] },
     url:true,nullable:true)

    xlink1Purpose (validator: { val, obj ->
     if (
     ((val != null) && (obj.properties['xlink1'] == null))
      ||
     ((val == null) && (obj.properties['xlink1'] != null))
     ) return ['invalid.correlation'] },
    nullable:true)

    xlink2Purpose (validator: { val, obj ->
     if (
     ((val != null) && (obj.properties['xlink2'] == null))
      ||
     ((val == null) && (obj.properties['xlink2'] != null))
     ) return ['invalid.correlation'] },
     nullable:true)

    datasetLanguage(nullable:true,blank:false)
    accessRestrictions(nullable:true,blank:false)
  }

  String toString() { title }
}
