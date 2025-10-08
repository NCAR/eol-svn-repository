class User { 
  String email
  String password
  Contact myContact

  static belongsTo = Contact

  String toString() { email }

  static constraints = {
    email(maxSize:255,blank:false,nullable:false,unique:true,email:true)
    password(minSize:6,maxSize:255,blank:false,nullable:false)
    myContact(nullable:true)
  }

}	
