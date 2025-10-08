class Contact implements Comparable {

  String personName
  String organizationName
  String department
  String shortName
  String gcmdName
  String position
  String address
  String city
  String state
  String postalCode
  String country
  String phoneNumber
  String faxNumber
  String email
  String homePage

  static constraints = {
    personName(blank:false)
    organizationName(blank:false)
    position(nullable:true)
    shortName(blank:false,unique:true)
    address()
    city()
    state()
	country()
    postalCode()
    email(email:true,unique:true,blank:false,nullable:false)
    homePage(url:true,nullable:true,blank:true)
	phoneNumber(nullable:true)
    faxNumber(nullable:true)
	department()
    gcmdName(nullable:true)
  }

  static hasMany = [ metadataDatasets:Dataset, datacenterDatasets:Dataset, piProjects:Project ]
  static mappedBy = [ metadataDatasets:"metadataContact", datacenterDatasets:"datacenterContact", piProjects:"piContact" ]

  String toString() { "${this.personName}" }
 // String toString() { "${this.personName} < ${this.email} >" }
 // String toString() { "${this.organizationName}/${this.personName} <${this.email}>" }

  String toCdpString() {
   def s = ""
   def org = (this.organizationName && ("" != this.organizationName))
   def per = (this.personName && ("" != this.personName))
   def eml = (this.email && ("" != this.email))

   if (org) s+=this.organizationName
   if (org && per) s+="/"
   if (per) s+=this.personName
   if ((org || per) && eml) s+=" "
   if (eml) {
     s+=this.email.replace("@"," AT ").replace("."," DOT ")    
     }

   s
   }

  int compareTo(obj) {
    if (obj == null) {
      throw new NullPointerException()
      return
      }
    if (this.id == obj?.id) return 0

    // first find the "last"/surname of personName
    def i = this.personName.lastIndexOf(" ") + 1
    if (i > this.personName.length()) i=0
    def j = obj.personName.lastIndexOf(" ") + 1
    if (j > obj.personName.length()) j=0

    def k = 0
    try {
      k = this.personName.substring(i).compareToIgnoreCase(obj.personName.substring(j))
      } catch (IndexOutOfBoundsException e) { // from substring()
      k = 0 // skip this test and try the next one
      }
    if (k != 0) return k

    // now try whole personName
    k = this.personName.compareToIgnoreCase(obj.personName)
    if (k != 0) return k

    // now try email
    k = this.email.compareToIgnoreCase(obj.email)
    if (k != 0) return k

    // give up, compare database id
    this.id.compareTo(obj.id)
  }

}
