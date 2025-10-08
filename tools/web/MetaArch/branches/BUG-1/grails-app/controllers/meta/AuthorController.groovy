package meta

import grails.converters.JSON
import groovy.json.JsonBuilder

class AuthorController {

	def scaffold = Author
	
    //def index() { }
	
	// Asynchronous search for adding authors to datasets
	def asearch() {
		def builder
		def results = Author.withCriteria {
			or {
				ilike('firstName', params.term + "%")
				ilike('middleName', params.term + "%")
				ilike('lastName', params.term + "%")
				ilike('organization', "%" + params.term + "%")
			}
		}
		
		builder = "["
			def c = 0
			results.each { r ->
				builder += "{"
					builder += 'value: "'+r.id+'",'
					builder += 'label: "'+r.toString()+'"'
				builder += "}"
				if (c < results.size() - 1) {
					builder += ","
				}
				c++
			}
		builder += "]"
		
		/*
		println "Found " + results.size() + " for term '"+ params.term + "'."
		println "  " + builder
		*/
		
		render (grails.converters.JSON.parse(builder) as JSON)
	}
	
	def acreate() {
		def author = new Author()
		author.properties = params
		author.ajax_added = true
		
		if(!author.hasErrors() && author.save(flush: true)) {
			//println "Author " + author + " was created from AJAX call."
			render ([author.id, author.toString()] as JSON)
		}
	}
}
