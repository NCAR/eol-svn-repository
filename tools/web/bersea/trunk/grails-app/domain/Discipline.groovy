class Discipline {
	String name

    String toString() { name }

    static constraints = {
      name(nullable:false, unique:true, blank:false)
    }

}
