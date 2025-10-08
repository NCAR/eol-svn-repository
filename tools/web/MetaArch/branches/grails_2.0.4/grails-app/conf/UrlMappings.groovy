class UrlMappings {

	static mappings = {
		"/$controller/$action?/$id?"{
			constraints {
				// apply constraints here
			}
		}

		"/"(view:"/index")
		"500"(view:'/error')
		"/admin"(view:"/admin")
		"/dataSubmissions"(view:"/data_submission")
		"/docFormatGuide"(view:"/doc_format_guide")
		"/register/message"(view:"/register/message")
	}
}
