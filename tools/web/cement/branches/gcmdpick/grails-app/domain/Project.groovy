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

  Date dateCreated
  Date lastUpdated

  static constraints = {
    title(maxSize:255,blank:false,nullable:false)
    nsfAwardNumber(unique:true,blank:false,nullable:false)
    entryID(blank:false,nullable:false) // not unique while using only title and not using nsfAwardNumber
    summary(maxSize:65535)
    beginDate(nullable:false)
    endDate(nullable:false)
    minimumLatitude(min:new BigDecimal(-90.0D),max:new BigDecimal(90.0D),precision:7,scale:5,nullable:true)
    maximumLatitude(min:new BigDecimal(-90.0D),max:new BigDecimal(90.0D),precision:7,scale:5,nullable:true)
    minimumLongitude(min:new BigDecimal(-180.0D),max:new BigDecimal(180.0D),precision:8,scale:5,nullable:true)
    maximumLongitude(min:new BigDecimal(-180.0D),max:new BigDecimal(180.0D),precision:8,scale:5,nullable:true)
    discipline(inList:["Atmosphere","Human Dimensions","Hydrology and Terrestrial Cryosphere","Ocean and Sea Ice","Terrestrial Ecosystems", "Supporting Data Sets"])
    piContact(nullable:false)
  }

  static hasMany = [ datasets:Dataset ]

  String toString() {
    def elipsis
    def l

     elipsis = false
     def p = this.piContact?.lastName
     if (!p) p = ''
     l = p.length() - 1
     if (l > 19) {
       l = 16
       elipsis = true
       }
     p = p[0..l]
     if (elipsis) p+='...'

    /*
     elipsis = false
     l = this.title.length() - 1
     if (l > 39) {
       l = 36
       elipsis = true
       }
     def t = this.title[0..l]
     if (elipsis) t+='...'

    String.format("%-20s -- %-40s (%s)",
      p, t, this.nsfAwardNumber
      )
    */

    def s = p + ' -- ' + this.title
    if (s.length() > 64) s = s[0..60] + '...'
    String.format("%-64s (%s)", s, this.nsfAwardNumber)

    }

/*
  String getlastName() {
   def s = null
   def elipsis = false
   def l = 0

   if (this.personName) {
     l = this.personName.length() - 1
     def i = this.personName.lastIndexOf(" ") + 1
     if (i > l) i=0
     if (l-i > 28) {
       l = i+26
       elipsis = true
       }
     s = this.personName[i..l]
     }
   else if (this.organizationName) {
     l = this.organizationName.length()
     if (l > 29) {
       l = 26
       elipsis = true
       }
     s = this.organizationName[0..l]
     }
   else return null

   if (elipsis) s+='...'
   s
   }
*/

  static entryIDprefix = "org.nsf.aon.cadis."

  String theTitleEntryID() { this.cdpName?.replaceAll("[^A-Za-z0-9_-]","_") }

  String theDisciplineEntryID() {
    this.discipline.replaceAll("[^A-Za-z0-9_-]","_")
  }

  int compareTo(obj) {
    if (this.id == obj?.id) return 0

    def k = 0

    // could just compare the toString()s but they might have ... so this is a little more complete

    // first compare the "last"/surname of personName
    k = this.piContact.lastName.compareToIgnoreCase(obj.piContact.lastName)
    if (k != 0) return k

    k = this.title.compareTo(obj?.title)
    if (k!=0) return k

    k = this.nsfAwardNumber.compareTo(obj?.nsfAwardNumber)
    if (k!=0) return k

    this.id.compareTo(obj?.id)
  }

}
