package meta

import grails.converters.JSON
import groovy.json.JsonBuilder
//import org.codehaus.groovy.grails.commons.DefaultGrailsDomainClass
//import org.codehaus.groovy.grails.validation.ConstrainedProperty

class RequirementController {

	def scaffold = Requirement
	XmlMageService xmlMageService
	
    //def index() { }
	
	def acreate() {
		def req = new Requirement()
		def prj = meta.Project.get(params.id)
		params.remove('id')
		
		req.properties = params
		req.project = prj
		
		
		if(!req.hasErrors() && req.save(flush: true)) {
			//println "Requirement " + req + " was created from AJAX call."
			render ([req.id, req.toString()] as JSON)
		}
	}
	
	def adelete() {
		def rid = params.id
		def req = Requirement.get(rid)
		params.remove('id')
		
		req.delete(flush: true)
		//println "Requirement " + req + " was deleted from AJAX call."
		render ([rid] as JSON)
	}
	
	def afields() {
		/* This action should find and return all possible data set fields 
		 * available to require that:
		 * 		- aren't already required by the domain object
		 * 		- are defined as fields (i.e. additional project-specific metadata)
		 * 		- a Requirement object doesn't already exist for in this project
		 */
		def project = meta.Project.get((params.project).toInteger())
		def builder
		def results
		def fields = (grailsApplication.getDomainClass(Dataset.class.name)).properties
		def existingReqs = Requirement.findAllByProject(project)
		def existingReqFields = []
		def defaults = ['id', 'version', 'versionNumber', 'dateCreated', 'lastUpdated']
		
		//println "fields are \t" + fields.name
		results = fields.name
		results = ( results - defaults )
		
		// Remove any constraints-required fields from the results (i.e. NULLABLE_CONSTRAINT = false)
		def nullConstraint = org.codehaus.groovy.grails.validation.ConstrainedProperty.NULLABLE_CONSTRAINT
		def existingNonNullables = (meta.Dataset.constraints).findAll { k ->
			(k.value).findAll { it.hasAppliedConstraint(nullConstraint) && it.isNullable() == false }
		}.collect{it.key}
		//println "\n\n" + existingNonNullables
		if ( existingNonNullables.size > 0 ) {
			results = ( results + existingNonNullables ) - results.intersect( existingNonNullables )
		}
		
		// Add any project-specific metadata fields
		def xmlMetadata = []
		if ( xmlMageService.hasXmlTemplate(project) ) {
			xmlMetadata = xmlMageService.getAllFields(project)
			results = ( results + xmlMetadata ) - results.intersect( xmlMetadata )
		}
		
		// Remove any pre-existing required fields from the results list (including project-specific fields)
		if ( existingReqs.size > 0 ) {
			existingReqs.each { r ->
				existingReqFields.add(r.field)
			}
			results = ( results + existingReqFields ) - results.intersect( existingReqFields )
		}
		
		// Filter the results down to only those that match the input term
		if (params.term != null) {
			def pattern = "^.*" + params.term + ".*\$"
			//println "Pattern is: \t" + pattern
			results = results.grep(~/${pattern}/) // Need to check for case insensitivity
		}
		
		// Sort the results alphabetically
		results = results.sort()
		
		// Build out the results as a JSON object
		builder = "["
			def c = 0
			results.each { r ->
				builder += "{"
					builder += 'value: "'+r.toString()+'",'
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
}
