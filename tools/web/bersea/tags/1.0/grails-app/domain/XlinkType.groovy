class XlinkType {
  String purpose

  String toString() { purpose }

  static constraints = {
	purpose(nullable:false, unique:true, blank:false)
  }

}
