package meta

class XmlTemplateController {

	def scaffold = XmlTemplate
	
    static allowedMethods = [save: "POST", update: "POST", delete: "POST"]
	
	def index = { redirect(action: 'list', params: params) }

	def download() {
		def xmlTemplate = XmlTemplate.get(params.id)
		
		if (xmlTemplate != null) {
		
			def file = xmlTemplate.file
			if (file != null) {
				def fileName = URLEncoder.encode(xmlTemplate.filename)
				response.addHeader("content-disposition", "attachment;filename=$fileName")
				response.contentType = "text/xml"
				response.contentLength = file.size()
				response.outputStream << file
			}
		}
	}
	
	def xml() {
		def xmlTemplate = XmlTemplate.get(params.id)
		if (!xmlTemplate) {
			flash.message = message(code: 'default.not.found.message', args: [message(code: 'XmlTemplate.label', default: 'XmlTemplate'), params.id])
			redirect(action: "list")
			return
		}
		
		if (xmlTemplate.file.size() == 0) {
			flash.message = message(code: '<i>[{0}] {1}</i> does not contain an uploaded file to display', args: [xmlTemplate.id, xmlTemplate.filename])
			redirect(action: "show", id: xmlTemplate.id)
			return
		}
		
		// If this point is reached, it can be assumed that the XmlTemplate's file is not empty, and the xml view can be displayed
		render(view: "xml", model: [xmlTemplateInstance: xmlTemplate], id: xmlTemplate.id)
	}
	
	/*
    def index() {
        redirect(action: "list", params: params)
    }

    def list() {
        params.max = Math.min(params.max ? params.int('max') : 50, 100)
        [xmlTemplateList: XmlTemplate.list(params), xmlTemplateTotal: XmlTemplate.count()]
    }
	
	def create() {		
		[xmlTemplate: new XmlTemplate(params)]
	}
	*/

	def save() {
		def xmlTemplate = new XmlTemplate(params)
		
		if (params.file != null) {
			xmlTemplate.size = xmlTemplate.file.size()
		}
		
		if (!xmlTemplate.save(flush: true)) {
			render(view: "create", model: [xmlTemplate: xmlTemplate])
			return
		}

		flash.message = message(code: 'default.created.message', args: [message(code: 'XmlTemplate.label', default: 'XmlTemplate'), xmlTemplate.id])
		redirect(action: "show", id: xmlTemplate.id)
	}

	/*
	def show() {
		def xmlTemplate = XmlTemplate.get(params.id)
		if (!xmlTemplate) {
			flash.message = message(code: 'default.not.found.message', args: [message(code: 'XmlTemplate.label', default: 'XmlTemplate'), params.id])
			redirect(action: "list")
			return
		}

		[xmlTemplate: xmlTemplate]
	}
	*/

	/*
	def edit() {
		def xmlTemplate = XmlTemplate.get(params.id)
		if (!xmlTemplate) {
			flash.message = message(code: 'default.not.found.message', args: [message(code: 'XmlTemplate.label', default: 'XmlTemplate'), params.id])
			redirect(action: "list")
			return
		}

		[xmlTemplate: xmlTemplate]
	}
	*/

	/**/
	def update() {
		def xmlTemplate = XmlTemplate.get(params.id)
		if (!xmlTemplate) {
			flash.message = message(code: 'default.not.found.message', args: [message(code: 'XmlTemplate.label', default: 'XmlTemplate'), params.id])
			redirect(action: "list")
			return
		}

		if (params.version) {
			def version = params.version.toLong()
			if (xmlTemplate.version > version) { def lowerCaseName = grails.util.GrailsNameUtils.getPropertyName(className)
				xmlTemplate.errors.rejectValue("version", "default.optimistic.locking.failure",
						  [message(code: 'XmlTemplate.label', default: 'XmlTemplate')] as Object[],
						  "Another user has updated this XmlTemplate while you were editing")
				render(view: "edit", model: [xmlTemplate: xmlTemplate])
				return
			}
		}

		xmlTemplate.properties = params
		
		if (params.file != null) {
			xmlTemplate.size = xmlTemplate.file.size()
		}

		if (!xmlTemplate.save(flush: true)) {
			render(view: "edit", model: [xmlTemplate: xmlTemplate])
			return
		}

		flash.message = message(code: 'default.updated.message', args: [message(code: 'XmlTemplate.label', default: 'XmlTemplate'), xmlTemplate.id])
		redirect(action: "show", id: xmlTemplate.id)
	}
	/**/

	/*
	def delete() {
		def xmlTemplate = XmlTemplate.get(params.id)
		if (!xmlTemplate) {
			flash.message = message(code: 'default.not.found.message', args: [message(code: 'XmlTemplate.label', default: 'XmlTemplate'), params.id])
			redirect(action: "list")
			return
		}

		try {
			xmlTemplate.delete(flush: true)
			flash.message = message(code: 'default.deleted.message', args: [message(code: 'XmlTemplate.label', default: 'XmlTemplate'), params.id])
			redirect(action: "list")
		}
		catch (DataIntegrityViolationException e) {
			flash.message = message(code: 'default.not.deleted.message', args: [message(code: 'XmlTemplate.label', default: 'XmlTemplate'), params.id])
			redirect(action: "show", id: params.id)
		}
	}
	*/
}
