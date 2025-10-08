class Project {

  String title
  String nsfAwardNumber 
  String summary
  Date beginDate
  Date endDate
  BigDecimal minimumLatitude = new BigDecimal(-90.0D)
  BigDecimal maximumLatitude = new BigDecimal(90.0D)
  BigDecimal minimumLongitude = new BigDecimal(-180.0D)
  BigDecimal maximumLongitude = new BigDecimal(180.0D)

  Contact piContact // 1: "THE" P.I. that matches 1:1 with nsfAwardNumber ; can edit this

  static constraints = {
    title(maxSize:255,blank:false,nullable:false)
    nsfAwardNumber(nullable:true)
    summary(maxSize:65535)
    beginDate()
    endDate()
    minimumLatitude(min:new BigDecimal(-90.0D),max:new BigDecimal(90.0D),precision:7,scale:5,nullable:true)
    maximumLatitude(min:new BigDecimal(-90.0D),max:new BigDecimal(90.0D),precision:7,scale:5,nullable:true)
    minimumLongitude(min:new BigDecimal(-180.0D),max:new BigDecimal(180.0D),precision:8,scale:5,nullable:true)
    maximumLongitude(min:new BigDecimal(-180.0D),max:new BigDecimal(180.0D),precision:8,scale:5,nullable:true)
    piContact(nullable:false)
  }

  static hasMany = [ datasets:Dataset ]

  String toString() { title }

// Each PI can have a hierarchy of datasets identified as follows:
//  String toString() { "$piContact: $title ($nsfAwardNumber)" }

  String theTitleEntryID() {
    this.title.toLowerCase().replaceAll("[^A-Za-z0-9_-]","_")
  }

}
