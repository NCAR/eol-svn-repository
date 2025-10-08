package meta

import meta.auth.*
import org.codehaus.groovy.grails.commons.DefaultGrailsDomainClass
import org.codehaus.groovy.grails.web.servlet.mvc.GrailsParameterMap
import org.hibernate.FetchMode as FM

// Imports for docWizardDownload
import org.xhtmlrenderer.pdf.ITextRenderer
import javax.servlet.http.HttpServletResponse
import javax.xml.parsers.DocumentBuilder
import groovy.xml.XmlUtil
import groovy.xml.DOMBuilder

// Grapes grab for the PDF format in docWizardDownload
@Grapes(
	@Grab(
	  group='org.xhtmlrenderer',
	  module='core-renderer',
	  version='R8'
	)
)

class DatasetController {
	
	// NEED TO ADD A THING TO INDICATE WHEN SOMETHING HAS BEEN ARCHIVED.
	
	def springSecurityService
	def mailService
	ProjectAuthService projectAuthService
	DatasetAuthService datasetAuthService
	CurrentUserService currentUserService
	EmailerService emailerService
	XmlMageService xmlMageService
	
	def grailsApplication
	
	def scaffold = Dataset
	
	def docWizard = {
		def dataset = Dataset.get( params.id )
		if (!dataset) {
			flash.message = "Dataset not found with id ${params.id}"
			redirect(action: 'list')
		//  redirect(action: 'review')
		} else {
			if (!datasetAuthService.canEditDataset(dataset)) {
				flash.message = 'I\'m sorry, but you are not authorized to edit this dataset.'
				redirect(action:'show', id: dataset.id)
			} else {
				def fileFormats = ''
				
				dataset.files.each { f ->
					def ff = Format.get(f.format.id)
					
					if (fileFormats != '') {
						fileFormats += ', '
					}
					
					fileFormats += '' + ff
				}
			
				def readmeString = generateReadmeString(dataset, fileFormats)	
			
				return [ datasetInstance : dataset, generatedReadme: readmeString ]
			}
		}
	}
	
	def docWizardDownload = {
		StringBuffer sb = new StringBuffer()
		byte[] b
		ByteArrayOutputStream baos = new ByteArrayOutputStream()
		String outputFile = "readme"
		def fmsg = ""
		
		def ftypeNameMap = [PDF:"pdf", HTML:"html", ASCII:"txt", DOC:"docx"]
		// HTML:"text/html" - doesn't work for downloading html files (need force-download).
		def ftypeContentMap = [PDF:"application/pdf", HTML:"application/force-download", ASCII:"text/plain", DOC:"application/msword"]
		def dataset = Dataset.get(params.id)
		def theDOpts = Arrays.asList(params.dopt)
		def theFtype = params.ftype
		
		if (params.filename) {
			//println params.filename
			outputFile = params.filename
			//outputFile = (outputFile =~ /\.pdf\s*$/).replaceAll("")
			outputFile = outputFile + "." + ftypeNameMap.get(theFtype)
		}
		
		if ( params.readme_content ) {
			if ( ["PDF","HTML"].contains(theFtype) ) {
				if ( theFtype == "PDF" ) {
					sb.append("<!DOCTYPE html [\n<!ENTITY nbsp \"&#160;\">\n]>")
				} else if ( theFtype == "HTML" ) {
					sb.append("<!DOCTYPE html>")
				}
				sb.append("<html>")
				sb.append("<head></head>")
				sb.append("<body>")
				sb.append(params.readme_content)
				sb.append("</body>")
				sb.append("</html>")
				
				if ( theFtype == "PDF" ) {
					ITextRenderer renderer = new ITextRenderer()
					// Set up the ITextRenderer with the html input Document
					def doc = DOMBuilder.parse(new StringReader(sb.toString()))
					renderer.setDocument(doc, null)
					
					// Create the output stream to the designated file and create the PDF
					renderer.layout()
					renderer.createPDF(baos)
					
					b = baos.toByteArray()
				} else if ( theFtype == "HTML" ) {
					b = (sb.toString()).getBytes("UTF-8")
				}
			} else {
				if ( theFtype == "ASCII" ) {
					def readmeContent = params.readme_content
					
					readmeContent = convertHTMLtoASCII(readmeContent)
					
					sb.append(readmeContent)
					b = (sb.toString()).getBytes("UTF-8")
				} else if ( theFtype == "DOC" ) {  }
			}
			
			// Now take care of the download options
			if ( theDOpts.contains("add_file") ) {
				// Create the file, then add it to the dataset
				def thisFile = new File()
				thisFile.dateCreated = new Date()
				thisFile.fileVersion = 0
				thisFile.index = dataset.files.size() // +1 not needed for index
				thisFile.fileType = FileType.find { it.name() == "METADATA" }
				thisFile.format = Format.find { name == theFtype.toUpperCase() }
				thisFile.lastUpdated = new Date()

				outputFile = params.filename + "_v" + (thisFile.fileVersion + 1) + "." + ftypeNameMap.get(theFtype) // include the file version number in filename
				log.info("AUTO-README: Creating file " + outputFile)
				
				def userDir = retrieveDestDirectory(dataset.owner.username) // change to dataset.owner.username next dev-cycle
				userDir = new java.io.File(userDir, "/dataset_${dataset.id}")
				userDir.mkdirs()
				
				def destFile = new java.io.File(userDir, outputFile)
				
				// Set group-writeable permissions for the directory and file
//					if (!(userDir.toString() =~ /^C[:][\\]/)) {
//						println "chmod 775 " + userDir.toString() + "\n"
//						Runtime.getRuntime().exec("chmod 775" + userDir.toString())
//					}
				
				try {
					log.info("AUTO-README: Setting bytes to file:    " + destFile.toString())
					destFile.setBytes(b) // Write the bytes from the byte array to the File.
					
//						if (!(destFile.toString() =~ /^C[:][\\]/)) {
//							println "chmod 664 " + destFile.toString() + "\n"
//							Runtime.getRuntime().exec("chmod 664" + destFile.toString())
//						}
					
					// If the upload is successful, update the size, name and directory of the fileInstance!
					thisFile.name = outputFile
					thisFile.directory = userDir.toString()
					thisFile.size = (destFile.size() / 1024).toInteger()
					thisFile.fileVersion = (thisFile.fileVersion + 1) // Update the version since it has been updated
				} catch (e) {
					log.info("AUTO-README: unable to create " + outputFile + " to " + userDir.toString())
				}
				
				// Now add this file to the data set and save it.
				dataset.addToFiles(thisFile)
				
				if (!thisFile.hasErrors() && thisFile.save()) {
					log.info "AUTO-README: ${thisFile} should be saved now."
				} else {
					def fileFormats = ''
					dataset.files.each { f ->
						def ff = Format.get(f.format.id)
						if (fileFormats != '') {  fileFormats += ', '  }
						fileFormats += '${ff}'
					}
				
					//println "AUTO-README: Error saving File:\n ${thisFile.errors.allErrors}"
					log.error "AUTO-README: Error saving File:\n ${thisFile.errors.allErrors}"
					flash.error = "There was a problem adding the generated document \"" + outputFile + "\" to data set with id ${params.id}"
					render(controller: 'dataset', view:'docWizard', model:[datasetInstance: dataset, generatedReadme: params.readme_content])
					return
				}
				dataset.save(flush: true) // We have modified the dataset with this option!
				
				fmsg = outputFile + " has been added to data set with id ${params.id}.  "
			}
			
			if ( theDOpts.contains("download") ) {
				// Send the file as a stream back to the user to download
				//println "Content type:\t"+ftypeContentMap.get(theFtype)
				
				def dFile = downloadFile( (ftypeContentMap.get(theFtype)), outputFile, b )
				if (dFile != null) {
					fmsg += "File \"" + outputFile + "\" has been prepared. Your download should start automatically. "
					fmsg += "If not, <a href=\""
					fmsg += g.createLink(controller:"tempFile", action:"adownload", params:[id :dFile.id])
					fmsg += "\"><strong>click here</strong></a>."
					
					flash.download = dFile.id // Add the download file ID to the flash map
				} else {
					flash.error = "There was a problem preparing the generated document \"" + outputFile + "\" for downloading."
					render(controller: 'dataset', view:'docWizard', model:[datasetInstance: dataset, generatedReadme: params.readme_content])
					return
				}
			}
			
			if (fmsg != "") { flash.message = fmsg }
		}
		
		//println "here there be dragons...  D --- >>>====>"
		return redirect(action: 'show', id: dataset.id)
		//println ">>>==(dragon head here)==>"
	}
	
	def getProject = {
		def prjList = projectAuthService.permittedProjects()
		
		if (prjList.size() == 0) {
			flash.message = 'I\'m sorry, but you are not associated with any project groups.'
			redirect (controller: 'project', action: 'list')
			return
		} else if (prjList.size() == 1) {
			redirect (controller: 'dataset', action: 'create', params: [project: prjList])
			return
		}
		
		[projects: prjList] // Render getProject.gsp with projects
	}

    def index = { redirect(action: 'list', params: params) }
	
	// The delete, save and update actions only accept POST requests
	def allowedMethods = [delete:'POST', save:'POST', update:'POST']
	
	def show = {
		def dataset = Dataset.get( params.id )

		// inserted next 3 lines for debugging
		//log.debug("System.properties = " + System.properties.toString())
		//log.debug("s.gp catalina.base = " + System.getProperty('catalina.base'))
		//log.debug("s.p.gp catalina.base = " + System.properties.getProperty('catalina.base'))

		if(!dataset) {
			flash.message = "Data set not found with id ${params.id}"
			redirect(action: 'list')
		} else {
			if (!projectAuthService.isaPermittedProject(dataset.project) && !datasetAuthService.canViewDataset(dataset)) {
				flash.message = 'I\'m sorry, but you are not authorized to view that data set.'
				def myList = projectAuthService.permittedProjects()
				if (!myList?.size()) flash.message += ' I\'m sorry, but you are not authorized for any projects.'
				redirect(action: 'list')
			}
			
			return [ datasetInstance : dataset ]
		}
	}
	
	def list = {
		def curUser = currentUserService.lookupUser()
		if (!curUser) {
			//redirect controller: 'login'
		}
		if (curUser) {
			def datasets
			if (!currentUserService.isAdmin()) {
				// Regular users can only see projects they own or are associated with
				datasets = Dataset.findAllByOwnerOrPointOfContact(curUser, curUser)
			} else {
				// Admins and Developers can see all projects
				datasets = Dataset.all
			}
			if (params.project) {
				def project = Project.findByName(params.project)
				datasets = Dataset.findAllByProject(project)
			}
			
			//print "${projects}!!!!"
			def datasetTotal = datasets.count(datasets)
			
			if (!datasets) { flash.message = "No datasets currently available." }
			
			return [ datasetInstanceList: datasets, datasetInstanceTotal: datasetTotal ]
			
			//if(!params.max) params.max = 100
			//[ datasetList: Dataset.list( params ) ]
		}
	}
	
	def create = {
		def dataset = new Dataset()

		def notReady = false
		def project = null
		def requiredList = []	// Also try to grab any project-level required fields
		
		
		// If clone exists as a param, then we were redirected from the template action.
		if (params.clone != null) {
			def per = currentUserService.lookupUser()
			def original = Dataset.get(params.clone)
			def ignorable = ["id", "owner", "ownerId", "pointOfContact", "projectMetadata", "datasetCategories", "datasetPlatforms", "files", "xlinks", "isSubmitted", "isArchived", "dateCreated", "lastUpdated"]
			Map theProperties = [:]
			def origProps = original.properties
			
			origProps.each { k,v ->
				if (!ignorable.contains(k) && v != null) {
					theProperties.put(k, v)
				}
			}
			
			theProperties.put("owner", per)
			theProperties.put("pointOfContact", original.pointOfContact)
			theProperties.put("ownerId", per.id)
			theProperties.title = 'COPY -  ' + theProperties.title
			
			theProperties.each { k,v ->
				println "${k}:\t\t${v}"
			}
					
			def duplicate = new Dataset(theProperties)
			
			return [datasetInstance: duplicate]
		}
		
		
		if (params.project != null) {
			project = Project.findByName(params.project)
			params.project = project // Need to override the project (as String) as a Project object
			
			def me = currentUserService.lookupUser() //User.findByUsername(uname)   // get user id
			if (me) { dataset.owner = me }
			
			// Set default spatial/temporal coordinates to the project's.
			dataset.beginDate = project.beginDate
			dataset.endDate = project.endDate
			dataset.minLat = project.minLat
			dataset.maxLat = project.maxLat
			dataset.minLon = project.minLon
			dataset.maxLon = project.maxLon
			
			def myList = projectAuthService.permittedProjects()
			
			if (!myList.contains(project) || myList.isEmpty()) {
				notReady = true
			} else {
				(meta.Requirement.findAllByProject(project)).each { r ->
					requiredList.add(r.field)
				}
			}
		} else {
			notReady = true
		}
		
		if (notReady) {
			redirect(action: 'getProject')
		}
		
		// Since we may have written over params.project, we set the properties right before return
		dataset.properties = params
		
				
		return [datasetInstance: dataset, required: requiredList.collect{ "'$it'" }.join(', ')]
	}
	
	def save = {
		def dataset = new Dataset()
		
		if (!projectAuthService.isaPermittedProject(Project.get(params.project.id))) {
			flash.message = 'I\'m sorry, but you are not authorized to save to chosen project.'
			def myList = projectAuthService.permittedProjects()
			redirect(action: 'getProject')
			//render(view:'create', model:[dataset:dataset, 'myList':myList])
		}
		
		// Save a local copy of the authors
		def authors = params.authors
		
		// Save a local copy of the cat/plat, but remove from params when binding to the dataset.
		def categoryId = params.categoryId
		def platformId = params.platformId
		def frequencyId = params.frequencyId
		def theTimeZone = params.theTimeZone
		def timeZoneType = params.timeZoneType
//		def beginDate = params.beginDate
//		def endDate = params.endDate
		def theParams = params
		params.remove('authors')
		params.remove('categoryId')
		params.remove('platformId')
		params.remove('frequencyId')
		params.remove('theTimeZone')
		params.remove('timeZoneType')
		params.remove('theTimeZoneSelect')
//		params.remove('beginDate')
//		params.remove('endDate')
		
		params.frequency = frequencyId
		
		// Update the changes to the begin/end dates from strings to Date!
		TimeZone.setDefault(TimeZone.getTimeZone("UTC"));
//		dataset.beginDate = new java.text.SimpleDateFormat('yyyy-MM-dd HH:mm Z').parse(beginDate)
//		dataset.endDate = new java.text.SimpleDateFormat('yyyy-MM-dd HH:mm Z').parse(endDate)
		
		// Check the timeZoneType - add a note to the comments if local time
		if (timeZoneType == "localtime") {
			String timeNote = ""
			String endTimeNote = ""

			if (params.comments != "") {
				if (params.comments =~ /NOTE: The date range for this dataset is in "[\w\s,&\-\:\.]*" time./) {
					params.comments = (params.comments =~ /NOTE: The date range for this dataset is in "[\w\s,&\-\:\.]*" time./).replaceAll("")
				} else {
					endTimeNote = "\n\n"
				}
			}
			timeNote += "NOTE: The date range for this dataset is in \"" + theTimeZone + "\" time." + endTimeNote
			
			params.comments = timeNote + params.comments
		}
		
		// Update the Metadata Version!
		def mVersion = grailsApplication.metadata.'app.version'
		params.metadataVersion = mVersion
		
		dataset.properties = params
		
		// Convert the additional project metadata back to XML before saving!
		if (dataset.project != null && dataset != null && (params.projectMetadata)) {
			def newPrjMeta = xmlMageService.convertHTMLtoXML(params.projectMetadata)
			dataset.projectMetadata = newPrjMeta
		}
		
		
		if(!dataset.hasErrors() && dataset.save(flush: true)) {
			
			// Save Authors (add new ones, remove "deleted" ones)
			if (authors != null) {
				authors.sortKeys.each { authorId, sortKey ->
					def author = Author.get(authorId.toInteger())
					def dsAuth = DatasetAuthor.findByAuthorAndDataset(author, dataset)
					
					if (dsAuth != null) {
						dsAuth.sortKey = sortKey as int
						dsAuth.save()
					} else {
						DatasetAuthor.create(dataset, author, sortKey as int, true)
					}
				}
				authors.deleted.each { authorId, deleted ->
					if (deleted.toBoolean() == true) {
						def author = Author.get(authorId.toInteger())
						def dsAuth = DatasetAuthor.findByAuthorAndDataset(author, dataset)
						if (dsAuth != null) {
							println "Deleting ${author}..."
							dsAuth.delete() // May need to be flushed later
						}
					}
				}
			}
			
			// Save Categories (add new ones, remove "deleted" ones)
			if (categoryId) {
				// Find the categories to be deleted
				def catIds = []
				if (categoryId instanceof String) {
					catIds.add(categoryId.toLong())
				} else {
					for (c in categoryId) {
						catIds.add(c.toLong())
					}
				}
				def dsCatIds = DatasetCategory.findAllByDataset(dataset).category
				def commonCats = dsCatIds.intersect(catIds)
				def delCats = dsCatIds
				delCats.removeAll(commonCats)
				
				// Delete the "to be deleted" categories
				delCats.each { cat -> 
					//dataset.removeFromDatasetCategory(cat.toLong())
					DatasetCategory.unlink(dataset, cat.toLong())
				}
				
				// Now add the new categories
				catIds.each { cat ->
					dataset.addToDatasetCategory(cat.toLong())
				}
			}
			
			// Save Platforms (add new ones, remove "deleted" ones)
			if (platformId) {
				// Find the platforms to be deleted
				def platIds = []
				if (platformId instanceof String) {
					platIds.add(platformId.toLong())
				} else {
					for (p in platformId) {
						platIds.add(p.toLong())
					}
				}
				def dsPlatIds = DatasetPlatform.findAllByDataset(dataset).platform
				def commonPlats = dsPlatIds.intersect(platIds)
				def delPlats = dsPlatIds
				delPlats.removeAll(commonPlats)
				
				// Delete the "to be deleted" platforms
				delPlats.each { plat -> 
					//dataset.removeFromDatasetPlatform(plat.toLong())
					DatasetPlatform.unlink(dataset, plat.toLong())
				}
				
				// Now add the new platforms
				platIds.each { plat ->
					dataset.addToDatasetPlatform(plat.toLong())
				}
			}
			
			emailerService.sendDatasetCreationNotification(dataset)
			
			
			flash.message = message(code: "dataset.created.message", args: [dataset.id, dataset.project])
			redirect(action: 'show', id: dataset.id)
		} else {
			def myList = projectAuthService.permittedProjects()
			render(view:'create', model:[datasetInstance: dataset])
		}
	}
	
	// To get a nice button in edit view
	def cancel = {
		redirect(action: 'show', id: params.id)
	}
	
	/*
	def delete = {
		def dataset = Dataset.get( params.id )
		if(!dataset) {
			flash.message = "Dataset not found with id ${params.id}."
			redirect(action: 'list')
		//  redirect(action: 'review')
		} else {
			if (!datasetAuthService.canEditDataset(dataset)) {
				flash.message = 'I\'m sorry, but you are not authorized to delete this dataset (ID=${params.id}).'
				redirect(action:'show', id: dataset.id)
			} else {
				dataset.delete()
				flash.message = "Dataset ${params.id} deleted."
				redirect(action: 'list')
			//  redirect(action: 'review')
			}
		}
	}
	*/
	
	def edit = {
		def dataset = Dataset.get( params.id )
		if(!dataset) {
			flash.message = "Dataset not found with id ${params.id}"
			redirect(action: 'list')
		//  redirect(action: 'review')
		} else {
			if (!datasetAuthService.canEditDataset(dataset)) {
				flash.message = 'I\'m sorry, but you are not authorized to edit this dataset.'
				redirect(action:'show', id: dataset.id)
			} else {
				def myList = projectAuthService.permittedProjects()
				//return ['dataset': dataset, 'myList': myList]
		
				// Also try to grab any project-level required fields
				def requiredList = []
				(meta.Requirement.findAllByProject(dataset.project)).each { r ->
					requiredList.add(r.field)
				}
				
						
				return [datasetInstance: dataset, required: requiredList.collect{ "'$it'" }.join(', ')]
			}
		}
	}
	
	def editData = {
		def dataset = Dataset.get( params.id )
		if(!dataset) {
			flash.message = "Dataset not found with id ${params.id}"
			redirect(action: 'list')
		//  redirect(action: 'review')
		} else {
			if (!datasetAuthService.canEditDataset(dataset)) {
				flash.message = 'I\'m sorry, but you are not authorized to edit this dataset.'
				redirect(action:'show', id: dataset.id)
			} else {
				return ['datasetInstance': dataset]
				//  return [ dataset : dataset ]
			}
		}
	}/*
	
	def editSingleData = {
		def dataset = Dataset.get( params.id )
		if(!dataset) {
			flash.message = "Dataset not found with id ${params.id}"
			redirect(action: 'list')
		//  redirect(action: 'review')
		} else {
			if (!datasetAuthService.canEditDataset(dataset)) {
				flash.message = 'I\'m sorry, but you are not authorized to edit this dataset.'
				redirect(action:'show', id: dataset.id)
			} else {
				return ['datasetInstance': dataset]
				//  return [ dataset : dataset ]
			}
		}
	}*/

	def update = {
		def dataset = Dataset.get( params.id )
		if(dataset) {
			if (!datasetAuthService.canEditDataset(dataset)) {
				flash.message = 'I\'m sorry, but you are not authorized to update this dataset.'
				redirect(action:'show', id: dataset.id)
			} else {
				if (!projectAuthService.isaPermittedProject(dataset.project)) {
					flash.message = 'I\'m sorry, but you are not authorized to add dataset to chosen project.'
					def myList = projectAuthService.permittedProjects()
					if (!myList?.size()) flash.message += ' I\'m sorry, but you are not authorized for any projects.'
					render(view:'edit', model:[datasetInstance: dataset])
					//render(view:'edit', model:[dataset:dataset, 'myList':myList])
				} else {
					// Save a local copy of the authors
					def authors = params.authors
				
					// Save a local copy of the cat/plat, but remove from params when binding to the dataset.
					def categoryId = params.categoryId
					def platformId = params.platformId
					def frequencyId = params.frequencyId
					def theTimeZone = params.theTimeZone
					def timeZoneType = params.timeZoneType
//					def beginDate = params.beginDate
//					def endDate = params.endDate
					def theParams = params
					params.remove('authors')
					params.remove('categoryId')
					params.remove('platformId')
					params.remove('frequencyId')
					params.remove('theTimeZone')
					params.remove('timeZoneType')
					params.remove('theTimeZoneSelect')
//					params.remove('beginDate')
//					params.remove('endDate')
					
					params.frequency = frequencyId
		
					// Update the changes to the begin/end dates from strings to Date!
					TimeZone.setDefault(TimeZone.getTimeZone("UTC"));
//					dataset.beginDate = new java.text.SimpleDateFormat('yyyy-MM-dd HH:mm Z').parse(beginDate)
//					dataset.endDate = new java.text.SimpleDateFormat('yyyy-MM-dd HH:mm Z').parse(endDate)
					
					params.collect{it.equals("") || it == null}.each {
						params.remove(it)
					}
					
					theParams = params
					
					// Check the timeZoneType - add a note to the comments if local time
					if (timeZoneType == 'localtime') {
						String timeNote = ""
						String endTimeNote = ""
			
						if (params.comments != "") {
							if (params.comments =~ /NOTE: The date range for this dataset is in "[\w\s,&\-\:\.]*" time./) {
								params.comments = (params.comments =~ /NOTE: The date range for this dataset is in "[\w\s,&\-\:\.]*" time./).replaceAll("")
							} else {
								endTimeNote = "\n\n"
							}
						}
						timeNote += "NOTE: The date range for this dataset is in \"" + theTimeZone + "\" time." + endTimeNote
												
						params.comments = timeNote + params.comments
					}
		
					// Update the Metadata Version!
					def mVersion = grailsApplication.metadata.'app.version'
					params.metadataVersion = mVersion
					
					dataset.properties = params
					
					// Convert the additional project metadata back to XML before saving!
					if (dataset.project != null && dataset != null && (params.projectMetadata)) {
						def newPrjMeta = xmlMageService.convertHTMLtoXML(params.projectMetadata)
						dataset.projectMetadata = newPrjMeta
					}
					
					// Remove any and all deleted xlinks from the dataset
					def _xlinksToBeDeleted = dataset.xlinks.findAll {(it?.deleted || (it == null))}
					if (_xlinksToBeDeleted) {
						dataset.xlinks.removeAll(_xlinksToBeDeleted)
					}
					// Remove any and all deleted files from the dataset
					def _filesToBeDeleted = dataset.files.findAll {(it?.deleted || (it == null))}
					if (_filesToBeDeleted) {
						dataset.files.removeAll(_filesToBeDeleted)
					}
					
					// Update the indexes
					dataset.files.eachWithIndex() {f, i ->
						f.index = i
					}
					dataset.xlinks.eachWithIndex() {x, i ->
						x.index = i
					}			
					
					
					// Be sure to reset the isSubmitted flag if it's true!
					if (dataset.isSubmitted == true) {
						dataset.isSubmitted = false
					}
					
					if(!dataset.hasErrors() && dataset.save()) {
			
						// Save Authors (add new ones, remove "deleted" ones)
						if (authors != null) {
							authors.sortKeys.each { authorId, sortKey ->
								def author = Author.get(authorId.toInteger())
								def dsAuth = DatasetAuthor.findByAuthorAndDataset(author, dataset)
								
								if (dsAuth != null) {
									dsAuth.sortKey = sortKey as int
									dsAuth.save()
								} else {
									DatasetAuthor.create(dataset, author, sortKey as int, true)
								}
							}
							authors.deleted.each { authorId, deleted ->
								if (deleted.toBoolean() == true) {
									def author = Author.get(authorId.toInteger())
									def dsAuth = DatasetAuthor.findByAuthorAndDataset(author, dataset)
									if (dsAuth != null) {
										println "Deleting ${author}..."
										dsAuth.delete() // May need to be flushed later
									}
								}
							}
						}
						
						
						// Save Categories (add new ones, remove "deleted" ones)
						if (categoryId) {
							// Find the categories to be deleted
							def catIds = []
							if (categoryId instanceof String) {
								catIds.add(categoryId.toLong())
							} else {
								for (c in categoryId) {
									catIds.add(c.toLong())
								}
							}
							def dsCatIds = DatasetCategory.findAllByDataset(dataset).category
							def commonCats = dsCatIds.intersect(catIds)
							def delCats = dsCatIds
							delCats.removeAll(commonCats)
							
							// Delete the "to be deleted" categories
							delCats.each { cat -> 
								//dataset.removeFromDatasetCategory(cat.toLong())
								DatasetCategory.unlink(dataset, cat.toLong())
							}
							
							// Now add the new categories
							catIds.each { cat ->
								dataset.addToDatasetCategory(cat.toLong())
							}
						}
						
						// Save Platforms (add new ones, remove "deleted" ones)
						if (platformId) {
							// Find the platforms to be deleted
							def platIds = []
							if (platformId instanceof String) {
								platIds.add(platformId.toLong())
							} else {
								for (p in platformId) {
									platIds.add(p.toLong())
								}
							}
							def dsPlatIds = DatasetPlatform.findAllByDataset(dataset).platform
							def commonPlats = dsPlatIds.intersect(platIds)
							def delPlats = dsPlatIds
							delPlats.removeAll(commonPlats)
							
							// Delete the "to be deleted" platforms
							delPlats.each { plat -> 
								//dataset.removeFromDatasetPlatform(plat.toLong())
								DatasetPlatform.unlink(dataset, plat.toLong())
							}
							
							// Now add the new platforms
							platIds.each { plat ->
								dataset.addToDatasetPlatform(plat.toLong())
							}
						}
						
						
						flash.message = "Dataset ${params.id} updated"
						redirect(action: 'show', id: dataset.id)
					} else {
						def myList = projectAuthService.permittedProjects()
						//render(view:'edit', model:[datasetInstance:dataset, 'myList':myList])
						render(view:'edit', model:[datasetInstance: dataset])
					}
				}
			}
		} else {
			flash.message = "Dataset not found with id ${params.id}"
			redirect(action: 'edit', id: params.id)
		}
	}
	
	def addToArchive = {
		def dataset = Dataset.get( params.id )
		def per = currentUserService.lookupUser()
		if(dataset) {
			if (!datasetAuthService.canEditDataset(dataset)) {
				flash.message = 'I\'m sorry, but you are not authorized to submit this data set.'
				redirect(action: 'show', id: dataset.id)
			} else {
				// Output the metadata contents of the dataset into a pretty-text format.
				// Metadata contents should be saved as e-mail body.
				def body = '<pre>' + outputDatasetContents(dataset.id) + '</pre>'
				
				def person = per?.realname
				if (person == null) {
					person = per?.username
				}
			
				if ( emailerService.sendArchiveSubmissionNotification(person, dataset, body) ) {
					dataset.isSubmitted = true
					if(!dataset.hasErrors() && dataset.save()) {
						flash.message = "Thank you!  Your data set has been submitted to the " + dataset.project + ' archive.'
					} else {
						flash.message = "I'm sorry, but there was a problem with submitting your data set to the " + dataset.project + ' archive.'
					}
				} else {
					flash.message = "I'm sorry, but there was a problem with submitting your data set to the " + dataset.project + ' archive.'
				}
				
				redirect(action: 'show', id: dataset.id)
			}
		} else {
			flash.message = "Dataset not found with id ${params.id}"
			redirect(action: 'edit', id: params.id)
		}
	}
	
	def template = {
		def original = Dataset.get(params.id)
		
		redirect(action: 'create', params: [clone: original.id, project: original.project.name])
		
	}
	
	/*
	// Extra actions - review and template
	def template = {
		log.debug('template')
		def dataset = null
		def datasets = null
		if (params.id != null) {
			log.debug("Trying params.id = ${params.id}")
			if (params.id =~ /^\d+$/) {
				dataset = Dataset.get( params.id )
				log.debug(" got dataset = ${dataset}")
				def principal = currentUserService.lookupUser()
				def uname = principal.getUsername()         // get username
				def me = User.findByUsername(uname)   // get user id
				if (me) dataset.owner = me
				// found a dataset to use as a template, so pass it to create view
				def myList = projectAuthService.permittedProjects()
				render(view:'create', model:[dataset:dataset, 'myList':myList])
			} else {
				flash.message = 'Dataset not found'
				// redirect to create and let it try its logic to find a project
				if (params.id != null)
					params.remove('id')
				redirect(action: 'create', params: params)
			}
		}
	}
	
	def review = {
		def principal = currentUserService.lookupUser()
		def uname = principal.getUsername()             // get username
		def me = User.findByUsername(uname)       // get user id
		
		if (me) {
			def criteria = Dataset.createCriteria()
			def results = criteria {
				or {
					eq('owner', me)
					project {
						eq('internalContact', me)
					}
				}
				fetchMode('project', FM.EAGER)
			}
			return [ contact: me, datasets: results ]
		} else {
			flash.message = "Our apologies - we can't find your contact info."
		}
	}
	*/
	
	def allMetadata = {
		if (currentUserService.isAdmin() || currentUserService.isDmg()) { 
			def dataset = Dataset.get(params.id)
			def dsContents = outputDatasetContents(params.id)
			
			[datasetInstance: dataset, datasetInstanceOutput: dsContents]
		} else {
			flash.message = "I'm sorry, but you are not allowed to use the formatted view at this time."
			redirect(action: 'show', id: params.id)
		}
	}
	
	
	
	/* -------------------------------------------------------------- *
	 * BEGIN PRIVATE/PROTECTED FUNCTIONS
	 * -------------------------------------------------------------- */
	private String convertHTMLtoASCII(String htmlString) {
		def ascii
		
		ascii = htmlString.replaceAll(/(?m)<.*?>/,"").trim()	// basic simple cases
		ascii = ascii.replaceAll(/\&nbsp\;/, "  ")
		// Handle the img and link cases
		
		
		// use NekoHTMLParser for more complex cases
		
		return ascii
	}
	
	
	private String createCategoryList(Dataset dataset, String delimiter) {
		StringBuilder sb = new StringBuilder()
		def cList = dataset.categories()
		
		cList.each { c ->
			if (sb.length() > 0) {  sb.append(delimiter)  }
			sb.append(meta.Category.get(c).toString())
		}
		
		return sb.toString()
	}
	
	private String createPlatformList(Dataset dataset, String delimiter) {
		StringBuilder sb = new StringBuilder()
		def pList = dataset.platforms()
		
		pList.each { p ->
			if (sb.length() > 0) {  sb.append(delimiter)  }
			sb.append(meta.Platform.get(p).toString())
		}
		
		return sb.toString()
	}
	
	
	private TempFile downloadFile(String cType, String fname, byte[] b) {
		def tFile = new TempFile(
						name: fname,
						cType: cType,
						data: b,
						owner: currentUserService.lookupUser()
					)
			
		if (!tFile.hasErrors() && tFile.save()) {
			return tFile
		}
		
		return null
	}
	
	
	private String generateReadmeString(Dataset dataset, String fileFormats) {
		def rString = '''
<h4 class="header">TITLE</h4>
<p>''' + dataset.title + '''</p>

<h4 class="header">AUTHOR(S)</h4>
<p>
		-&nbsp;&nbsp;&nbsp;''' + dataset.pointOfContact.realname + ''' <br />
		&nbsp;&nbsp;&nbsp;&nbsp;''' + dataset.pointOfContact.organization + ''' <br />
		&nbsp;&nbsp;&nbsp;&nbsp;Email: ''' + dataset.pointOfContact.email + '''
</p>

<h4 class="header">FUNDING SOURCE and GRANT NUMBER</h4>
<p>
		&nbsp;&nbsp;&nbsp;&nbsp;''' + dataset.fundingAgency + ''' ''' + dataset.awardNumber + '''
</p>

<h4 class="header">DATA SET OVERVIEW</h4>
<p>
		''' + dataset.summary + '''
</p>

<h4 class="header">PLATFORM(S)</h4>
<p>
		&nbsp;&nbsp;&nbsp;&nbsp;''' + createPlatformList(dataset, '<br />\n&nbsp;&nbsp;&nbsp;&nbsp;') + ''' <br />
</p>

<h4 class="header">INSTRUMENT(S)</h4>
<p>
		&nbsp;&nbsp;&nbsp;&nbsp;''' + dataset.instrumentKeyword + '''
</p>

<h4 class="header">DATA COLLECTION, PROCESSING and METHODOLOGY</h4>
<p>
		&nbsp;&nbsp;&nbsp;&nbsp;
</p>

<h4 class="header">DATA FORMAT</h4>
<p>
		Data are in ''' + fileFormats + ''' format(s).
</p>

<h4 class="header">DATA REMARKS</h4>
<p>
		&nbsp;&nbsp;&nbsp;&nbsp;''' + dataset.comments + '''
</p>

<h4 class="header">REFERENCES</h4>
<p>
		&nbsp;&nbsp;&nbsp;&nbsp;
</p>'''

		return rString
	}
	
	
	private String outputDatasetContents(datasetId) {
		def datasetInstance = Dataset.get(datasetId)
		String output = ''
		
		output += 'Title:\t' + datasetInstance.title + '\n'
		output += '--------------------------------------------------------------------------------\n'
		output += 'Point of Contact:\n\t' + datasetInstance.pointOfContact + '\n'
		if (datasetInstance.owner.id != datasetInstance.pointOfContact.id) {
			output += 'Owner:\n\t' + datasetInstance.owner + '\n'
		}
		output += 'Summary:\n\n' + datasetInstance.summary + '\n\n\n'
		output += 'Dataset Version:\t' + datasetInstance.datasetVersion + '\n'
		
		output += '\n--------------------------------------------------------------------------------\n'
		output += 'Additional Information \n'
		output += '--------------------------------------------------------------------------------\n'
		output += 'Project:\n\t' + datasetInstance.project + '\n'
		output += 'Frequency:\n\t' + Frequency.get(datasetInstance.frequency).name + '\n'
		output += 'Spatial Type:\n\t' + datasetInstance.spatialType + '\n'
		output += 'Categories:\n\t'
		def i = 0
		datasetInstance.categories().each { c ->
			output += meta.Category.get(c).toString()
			if (i != datasetInstance.categories().size() - 1) { output += '\n\t' }
			i++
		}
		output += '\n'
		output += 'Platforms:\n\t'
		i = 0
		datasetInstance.platforms().each { p ->
			output += meta.Platform.get(p).toString()
			if (i != datasetInstance.platforms().size() - 1) { output += '\n\t' }
			i++
		}
		output += '\n'
		output += 'Science Keyword:\n\t' + datasetInstance.scienceKeyword + '\n'
		output += 'Location Keyword:\n\t' + datasetInstance.locationKeyword + '\n'
		output += 'Platform Keyword:\n\t' + datasetInstance.platformKeyword + '\n'
		output += 'Instrument Keyword:\n\t' + datasetInstance.instrumentKeyword + '\n'
		output += 'ISO Topic:\n\t' + datasetInstance.topic + '\n'
		
		output += '\n--------------------------------------------------------------------------------\n'
		output += 'Temporal Coverage \n'
		output += '--------------------------------------------------------------------------------\n'
		output += 'Begin Date:\n\t' + datasetInstance.beginDate + '\n'
		output += 'End Date:\n\t' + datasetInstance.endDate + '\n'
		
		output += '\n--------------------------------------------------------------------------------\n'
		output += 'Spatial Coverage \n'
		output += '--------------------------------------------------------------------------------\n'
		output += 'Minimum (South) Latitude:\n\t' + datasetInstance.minLat + '\n'
		output += 'Maximum (North) Latitude:\n\t' + datasetInstance.maxLat + '\n'
		output += 'Minimum (West) Longitude:\n\t' + datasetInstance.minLon + '\n'
		output += 'Maximum (East) Longitude:\n\t' + datasetInstance.maxLon + '\n'
		
		output += '\n--------------------------------------------------------------------------------\n'
		output += 'Funding Information \n'
		output += '--------------------------------------------------------------------------------\n'
		output += 'Funding Agency:\n\t' + datasetInstance.fundingAgency + '\n'
		output += 'Award Number:\n\t' + datasetInstance.awardNumber + '\n'
		//output += 'Award Amount:\n\t' + datasetInstance.awardAmount + '\n'
		
		output += '\n--------------------------------------------------------------------------------\n'
		output += 'Resolution \n'
		output += '--------------------------------------------------------------------------------\n'
		output += 'Horizontal Resolution:\n\t' + datasetInstance.horizontalResolution + '\n'
		output += 'Vertical Resolution:\n\t' + datasetInstance.verticalResolution + '\n'
		
		output += '\n--------------------------------------------------------------------------------\n'
		output += 'Status, Restrictions, Tags, etc. \n'
		output += '--------------------------------------------------------------------------------\n'
		output += 'Progress:\n\t' + datasetInstance.progress + '\n'
		output += 'Access Restrictions:\n\t' + datasetInstance.accessRestrictions + '\n'
		output += 'Language:\n\t' + datasetInstance.language + '\n'
		output += 'Comments:\n\t' + datasetInstance.comments + '\n'
		
		output += '\n--------------------------------------------------------------------------------\n'
		output += 'External Link List \n'
		output += '--------------------------------------------------------------------------------\n'
		i = 0
		datasetInstance.xlinks.each { x ->
			output += '\t[' + x.type.toString().toUpperCase() + ']\n\t' + x.title + '\n\t' + x.href + '\n'
			if (i != datasetInstance.xlinks.size() - 1) { output += '    ----------\n' }
			i++
		}
		
		output += '\n--------------------------------------------------------------------------------\n'
		output += 'Data / Documentation File List \n'
		output += '--------------------------------------------------------------------------------\n'
		i = 0
		datasetInstance.files.each { f ->
			output += '\t[' + f.fileType.toString().toUpperCase() + ']\t\t\t\t' + f.size + ' kB\n'
			output += '\t' + f.format.name + '\t(' + f.format.description + ')\n'
			output += '\t' + f.directory + '/' + f.name + '\n'
			if (i != datasetInstance.files.size() - 1) { output += '    ----------\n' }
			i++
		}
		
		output += '\n\n--------------------------------------------------------------------------------\n'
		output += 'Metadata Version ' + datasetInstance.metadataVersion + '\n'
		output += 'Date Created:\t' + datasetInstance.dateCreated + '\n'
		output += 'Last Updated:\t' + datasetInstance.lastUpdated
		
		return output
	}
	
	
	private java.io.File retrieveDestDirectory(String username) {
		// Purpose: Retrieves the destination directory for file uploads.
		// 			If the path doesn't exist, create it!
		
		def config = grailsApplication.config
		def envDestDir = config.grails.fileDestRootLoc
		
		//def webRootDir = new java.io.File("C:/Users/Amanda/workspace/miscellaneous")
		def webRootDir = new java.io.File(envDestDir) //'/scr/MetaArch')
		
		if (!webRootDir.exists()) {
			// If unable to find original directory path, use the current path.
//			webRootDir = new java.io.File('/scr/tmp/orin/MetaArch')
//
//			if (!webRootDir.exists()) {
				webRootDir = new java.io.File(servletContext.getRealPath("/"))
//			}
		}
		
		def userDir = new java.io.File(webRootDir, "/files/${username}")
		//println "userDir: ${userDir}"
		userDir.mkdirs()
		
		return userDir
	}
}
