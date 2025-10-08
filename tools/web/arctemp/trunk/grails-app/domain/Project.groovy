class Project {
  static hasMany = [pis: AuthUser, datasets: Dataset]
  static belongsTo = AuthUser

  String dataCenter
  String title
  String awardNumber 
  Integer awardAmount
  String fundingAgency
  String summary
  Date beginDate
  Date endDate
  BigDecimal minimumLatitude = new BigDecimal(-90.0D)
  BigDecimal maximumLatitude = new BigDecimal(90.0D)
  BigDecimal minimumLongitude = new BigDecimal(-180.0D)
  BigDecimal maximumLongitude = new BigDecimal(180.0D)

  Date dateCreated
  Date lastUpdated

  static constraints = {
	dataCenter(inList:["BEST", "BSIERP"])
    title(maxSize:255,blank:false, nullable:false)
	fundingAgency(inList:["NOAA", "NPRB", "NSF", "NASA"], nullable:true)
    awardNumber(unique:true, blank:false, nullable:false)
    awardAmount(nullable:true)
    summary(maxSize:65535, nullable:true)
    beginDate(nullable:true)
    endDate(nullable:true)
    minimumLatitude(min:new BigDecimal(-90.0D),max:new BigDecimal(90.0D), precision:7, scale:5, nullable:true)
    maximumLatitude(min:new BigDecimal(-90.0D),max:new BigDecimal(90.0D), precision:7, scale:5, nullable:true)
    minimumLongitude(min:new BigDecimal(-180.0D),max:new BigDecimal(180.0D), precision:8, scale:5, nullable:true)
    maximumLongitude(min:new BigDecimal(-180.0D),max:new BigDecimal(180.0D), precision:8, scale:5, nullable:true)
  }

}
