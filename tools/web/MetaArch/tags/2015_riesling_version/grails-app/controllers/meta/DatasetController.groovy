package meta

import meta.auth.*
import org.codehaus.groovy.grails.commons.DefaultGrailsDomainClass
import org.codehaus.groovy.grails.web.servlet.mvc.GrailsParameterMap
import org.hibernate.FetchMode as FM

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

		if (!projectAuthService.isaPermittedProject(dataset.project) && !datasetAuthService.canViewDataset(dataset)) {
			flash.message = 'I\'m sorry, but you are not authorized to view that data set.'
			def myList = projectAuthService.permittedProjects()
			if (!myList?.size()) flash.message += ' I\'m sorry, but you are not authorized for any projects.'
			redirect(action: 'list')
		}

		if(!dataset) {
			flash.message = "Data set not found with id ${params.id}"
			redirect(action: 'list')
		} else { return [ datasetInstance : dataset ] }
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
				datasets = Dataset.findAllByAuthorOrPointOfContact(curUser, curUser)
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


		// If clone exists as a param, then we were redirected from the template action.
		if (params.clone != null) {
			def per = currentUserService.lookupUser()
			def original = Dataset.get(params.clone)
			def ignorable = ["id", "author", "authorId", "pointOfContact", "projectMetadata", "datasetCategories", "datasetPlatforms", "files", "xlinks", "isSubmitted", "isArchived", "dateCreated", "lastUpdated"]
			Map theProperties = [:]
			def origProps = original.properties

			origProps.each { k,v ->
				if (!ignorable.contains(k) && v != null) {
					theProperties.put(k, v)
				}
			}

			theProperties.put("author", per)
			theProperties.put("pointOfContact", original.pointOfContact)
			theProperties.put("authorId", per.id)

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
			if (me) { dataset.author = me }
			def myList = projectAuthService.permittedProjects()

			if (!myList.contains(project) || myList.isEmpty()) {
				notReady = true
			}
		} else {
			notReady = true
		}

		if (notReady) {
			redirect(action: 'getProject')
		}

		// Since we may have written over params.project, we set the properties right before return
		dataset.properties = params


		return [datasetInstance: dataset]
	}

	def save = {
		def dataset = new Dataset()

		if (!projectAuthService.isaPermittedProject(Project.get(params.project.id))) {
			flash.message = 'I\'m sorry, but you are not authorized to save to chosen project.'
			def myList = projectAuthService.permittedProjects()
			redirect(action: 'getProject')
			//render(view:'create', model:[dataset:dataset, 'myList':myList])
		}

		// Save a local copy of the cat/plat, but remove from params when binding to the dataset.
		def categoryId = params.categoryId
		def platformId = params.platformId
		def frequencyId = params.frequencyId
		def theTimeZone = params.theTimeZone
		def timeZoneType = params.timeZoneType
		def theParams = params
		params.remove('categoryId')
		params.remove('platformId')
		params.remove('frequencyId')
		params.remove('theTimeZone')
		params.remove('timeZoneType')
		params.remove('theTimeZoneSelect')

		params.frequency = frequencyId

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

			// emailerService.sendDatasetCreationNotification(dataset)


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
				return [ datasetInstance : dataset ]
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
	}

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
					// Save a local copy of the cat/plat, but remove from params when binding to the dataset.
					def categoryId = params.categoryId
					def platformId = params.platformId
					def frequencyId = params.frequencyId
					def theTimeZone = params.theTimeZone
					def timeZoneType = params.timeZoneType
					def theParams = params
					params.remove('categoryId')
					params.remove('platformId')
					params.remove('frequencyId')
					params.remove('theTimeZone')
					params.remove('timeZoneType')
					params.remove('theTimeZoneSelect')

					params.frequency = frequencyId

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
				flash.message = 'I\'m sorry, but you are not authorized to submit this dataset.'
				redirect(action: 'show', id: dataset.id)
			} else {
				// Output the metadata contents of the dataset into a pretty-text format.
				// Metadata contents should be saved as e-mail body.
				def body = '<pre>' + outputDatasetContents(dataset.id) + '</pre>'

				def person = per?.realname
				if (person == null) {
					person = per?.username
				}

//				if ( emailerService.sendArchiveSubmissionNotification(person, dataset, body) ) {
       				if ( emailerService.sendArchiveSubmissionNotification(dataset, body) ) {
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
				if (me) dataset.author = me
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
					eq('author', me)
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

	private String outputDatasetContents(datasetId) {
		def datasetInstance = Dataset.get(datasetId)
		String output = ''

		output += 'Title:\t' + datasetInstance.title + '\n'
		output += '--------------------------------------------------------------------------------\n'
		output += 'Point of Contact:\n\t' + datasetInstance.pointOfContact + '\n'
		if (datasetInstance.author.id != datasetInstance.pointOfContact.id) {
			output += 'Author:\n\t' + datasetInstance.author + '\n'
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
}
