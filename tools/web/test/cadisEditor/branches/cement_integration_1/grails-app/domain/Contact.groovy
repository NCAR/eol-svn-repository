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
    postalCode()
    email(email:true,unique:true,blank:false,nullable:false)
    homePage(url:true,nullable:true,blank:true)
    faxNumber(nullable:true)
    gcmdName(nullable:true)
  }

  static hasMany = [ metadataDatasets:Dataset, datacenterDatasets:Dataset, piDatasets:Dataset ]
  static mappedBy = [ metadataDatasets:"metadataContact", datacenterDatasets:"datacenterContact", piDatasets:"piContact" ]

  String toString() { "${this.organizationName}/${this.personName} <${this.email}>" }
  String toCdpString() { "${this.organizationName}/${this.personName} ${this.email}" }

}
