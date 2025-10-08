class Contact {

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

  static hasMany = [ metadataDatasets:Dataset, datacenterDatasets:Dataset, piDatasets:Dataset ]
  static mappedBy = [ metadataDatasets:"metadataContact", datacenterDatasets:"datacenterContact", piDatasets:"piContact" ]

  String toString() { "${this.organizationName}/${this.personName} <${this.email}>" }

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

}
