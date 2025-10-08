class Project implements Comparable {

  String title
  String nsfAwardNumber 
  String entryID
  String summary
  Date beginDate
  Date endDate
  BigDecimal minimumLatitude = new BigDecimal(-90.0D)
  BigDecimal maximumLatitude = new BigDecimal(90.0D)
  BigDecimal minimumLongitude = new BigDecimal(-180.0D)
  BigDecimal maximumLongitude = new BigDecimal(180.0D)
  String discipline
  String cdpName

  Contact piContact // 1: "THE" P.I. that matches 1:1 with nsfAwardNumber ; can edit this

  static constraints = {
    title(maxSize:255,blank:false,nullable:false)
    nsfAwardNumber(unique:true,blank:false,nullable:false)
    entryID(blank:false,nullable:false) // not unique while using only title and not using nsfAwardNumber
    summary(maxSize:65535)
    beginDate()
    endDate()
    minimumLatitude(min:new BigDecimal(-90.0D),max:new BigDecimal(90.0D),precision:7,scale:5,nullable:true)
    maximumLatitude(min:new BigDecimal(-90.0D),max:new BigDecimal(90.0D),precision:7,scale:5,nullable:true)
    minimumLongitude(min:new BigDecimal(-180.0D),max:new BigDecimal(180.0D),precision:8,scale:5,nullable:true)
    maximumLongitude(min:new BigDecimal(-180.0D),max:new BigDecimal(180.0D),precision:8,scale:5,nullable:true)
    discipline(inList:["Atmosphere","Human Dimensions","Hydrology and Terrestrial Cryosphere","Ocean and Sea Ice","Terrestrial Ecosystems", "Supporting Data Sets"])
    piContact(nullable:false)
  }

  static hasMany = [ datasets:Dataset ]

//  String toString() { title }

// Each PI can have a hierarchy of datasets identified as follows:
  String toString() { "$piContact -- $title ($nsfAwardNumber)" }

  static entryIDprefix = "org.nsf.aon.cadis."

  String theTitleEntryID() {
    this.cdpName.replaceAll("[^A-Za-z0-9_-]","_")
 //   this.title.replaceAll("[^A-Za-z0-9_-]","_")  CDP shortened titles, now use cdpName instead
  }

  String theDisciplineEntryID() {
    this.discipline.replaceAll("[^A-Za-z0-9_-]","_")
  }

  int compareTo(obj) {
    if (this.id == obj?.id) return 0

    // first find the "last"/surname of personName
    def i = this.piContact.personName.lastIndexOf(" ") + 1
    if (i > this.piContact.personName.length()) i=0
    def j = obj.piContact.personName.lastIndexOf(" ") + 1
    if (j > obj.piContact.personName.length()) j=0

    def k = 0
    try {
      k = this.piContact.personName.substring(i).compareToIgnoreCase(obj.piContact.personName.substring(j))
      } catch (IndexOutOfBoundsException e) { // from substring()
      k = 0 // skip this test and try the next one
      }
    if (k != 0) return k

    k = this.nsfAwardNumber.compareTo(obj?.nsfAwardNumber)
    if (k!=0) return k

    k = this.title.compareTo(obj?.title)
    if (k!=0) return k

    this.id.compareTo(obj?.id)
  }

}
