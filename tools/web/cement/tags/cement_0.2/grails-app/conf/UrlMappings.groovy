class UrlMappings {
	static mappings = {
	  "/"{
	      controller = "contact"
	      action = "letsgo"
	  }

	  "/$controller/$action?/$id?"{
	      constraints {
			 // apply constraints here
		  }
	  }

	}
}
