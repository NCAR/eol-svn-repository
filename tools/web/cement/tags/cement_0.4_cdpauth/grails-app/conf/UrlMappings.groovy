class UrlMappings {
	static mappings = {
	  "/"{
	      controller = "dataset"
	      action = "review"
	  }

	  "/$controller/$action?/$id?"{
	      constraints {
			 // apply constraints here
		  }
	  }

	}
}
