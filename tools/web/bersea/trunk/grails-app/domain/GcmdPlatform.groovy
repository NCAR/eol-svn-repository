class GcmdPlatform {
    String keyword

    String toString() { keyword }

    static constraints = {
      keyword(nullable:false, unique:true, blank:false)
	}
}
