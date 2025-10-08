package meta

import meta.auth.*
import javax.servlet.http.HttpServletRequest;
import org.springframework.util.MultiValueMap
import org.springframework.web.multipart.MultipartFile
import org.springframework.web.multipart.MultipartHttpServletRequest

class FileController {

	DatasetAuthService datasetAuthService
	CurrentUserService currentUserService
	
	def grailsApplication
	
	def scaffold = File
	
	static allowedMethods = [save: "POST", update: "POST", delete: "POST", saveMultiple: "POST"]

	/*
	def download() {
		def fileInstance = File.get(params.id)
		
		if (fileInstance != null) {
		
			def file = fileInstance.file
			if (file != null) {
				def fileName = URLEncoder.encode(fileInstance.name)
				response.addHeader("content-disposition", "attachment;filename=$fileName")
				response.contentLength = file.size()
				response.outputStream << file
			}
		}
	}
	*/
	
	def save() {
		def fileInstance = new File(params)
				
		if (!fileInstance.save(flush: true)) {
			render(view: "create", model: [fileInstance: fileInstance])
			return
		}

		flash.message = message(code: 'default.created.message', args: [message(code: 'File.label', default: 'File'), fileInstance.id])
		redirect(action: "show", id: fileInstance.id)
	}
	
	def saveBatch = {
		def dataset = Dataset.get(params.dataset.id)
		def uploadUtils = new meta.UploadUtils()
		def goodFiles = []
		def badFiles = []
		Map fileMap = ((MultipartHttpServletRequest)request).fileMap
		
		if(!dataset) {
			flash.message = "Data set not found with id ${params.id}"
			redirect(controller: 'dataset', action:list)
			return
		}
		if (!datasetAuthService.canEditDataset(dataset)) {
			flash.message = 'I\'m sorry, but you are not authorized to edit this data set.'
			redirect(controller: 'dataset', action:'show', id: dataset.id)
			return
		}
		
		// If we make it this far, then we can continue with the file saving!
		def theFiles = Arrays.asList(params.files)
		def fileListSize = (params.fileListTotal).toLong()
		
		def fcount = 0
		theFiles.each {f ->
			for (fcount = 0; fcount < fileListSize; fcount += 1) {
				def thisFile
				Map fmap = new HashMap()
				
				if (fileListSize == 1) {
					fmap.put("id", f.get("id"))
					fmap.put("index", f.get("index"))
					fmap.put("deleted", f.get("deleted"))
					fmap.put("fileType", f.get("fileType"))
					fmap.put("format", f.get("format.id"))
				} else {
					fmap.put("id", f.id[fcount])
					fmap.put("index", f.index[fcount])
					fmap.put("deleted", f.deleted[fcount])
					fmap.put("fileType", f.fileType[fcount])
					fmap.put("format", f.format.id[fcount])
				}
				
				if ( fileListSize >= 1 && !(fmap.get("id")).equals("") ) {
					thisFile = File.get((fmap.get("id")).toLong())
				}
				boolean isNew = false
				if (!thisFile) {
					// It's not an existing file, so we need to create it!
					isNew = true
					thisFile = new File()
					thisFile.dateCreated = new Date()
					thisFile.fileVersion = 0
					thisFile.index = fmap.get("index")?.toInteger()
					
					thisFile.fileType = FileType.find { it.name() == fmap.get("fileType").toUpperCase() }
					
					thisFile.format = Format.get(fmap.get("format").toLong())
					thisFile.lastUpdated = new Date()
				} else {
					// If File exists, check if it's been deleted.
					if (fmap.get("deleted").toBoolean() == true) {
						thisFile.deleted = fmap.get("deleted").toBoolean()
						continue //return // like a continue in a while loop
					} else {
						thisFile.index = fmap.get("index")?.toInteger()
					
						thisFile.fileType = FileType.find { it.name() == fmap.get("fileType").toUpperCase() }
						
						thisFile.format = Format.get(fmap.get("format").toLong())
						thisFile.lastUpdated = new Date()
					}
				}
				
				// So now that we have a fileInstance to work with, we need to look for a MultipartFile!
				def ffile = "files["+fcount+"].file"
				def fList = ((MultipartHttpServletRequest)request).getFiles(ffile) //fileMap.get(ffile)
				if (fList) {
					def fspot = 0
					for (fspot = 0; fspot < fList.size(); fspot += 1) {
						MultipartFile file = fList.get(fspot)
						log.info("upload: form name " + ffile)
		
						if(!file?.empty) {
							//println "OriginalFileName: ${file.originalFilename}"
							//println "Size: ${file.size}"
							//println "ContentType: ${file.contentType}"
							log.info("OriginalFileName: ${file.originalFilename}")
							log.info("Size: ${file.size}")
							log.info("ContentType: ${file.contentType}")
			
							def originalFilename = file.getOriginalFilename()
							def df = new java.io.File(originalFilename)
							def myFilename = uploadUtils.fixFileBadCharacters(uploadUtils.removeWindowsExtraStuff(df.getName()))
			
							log.info("upload: got file "+myFilename)
							
							myFilename += "_v" + (thisFile.fileVersion + 1) // include the file version number in filename
			
							def userDir = retrieveDestDirectory(dataset.owner.username) // change to dataset.owner.username next dev-cycle
							userDir = new java.io.File(userDir, "/dataset_${dataset.id}")
							userDir.mkdirs()
							
							def destFile = new java.io.File(userDir, myFilename)
							
							// Set group-writeable permissions for the directory and file
		//					if (!(userDir.toString() =~ /^C[:][\\]/)) {
		//						println "chmod 775 " + userDir.toString() + "\n"
		//						Runtime.getRuntime().exec("chmod 775" + userDir.toString())
		//					}
							
							try {
								file.transferTo( destFile )
		//						if (!(destFile.toString() =~ /^C[:][\\]/)) {
		//							println "chmod 664 " + destFile.toString() + "\n"
		//							Runtime.getRuntime().exec("chmod 664" + destFile.toString())
		//						}
								goodFiles += myFilename
								
								// If the upload is successful, update the size, name and directory of the fileInstance!
								thisFile.name = myFilename.toString()
								thisFile.directory = userDir.toString()
								thisFile.size = (file.size / 1024).toInteger()
								thisFile.fileVersion = (thisFile.fileVersion + 1) // Update the version since it has been updated
							} catch (e) {
								badFiles += myFilename
							}
						} else if (file?.empty && isNew == true) {
							log.info("File \""+f.name[fcount]+"\" was empty (file size of 0)")
							badFiles += "\""+f.name[fcount]+"\" was empty (file size of 0)"
						}
					}
				}
				
				if (isNew == true) {
					dataset.addToFiles(thisFile)
				}
				if (!thisFile.hasErrors() && thisFile.save()) {
					log.info "${thisFile} should be saved now."
				} else {
					//println "Error saving File:\n ${thisFile.errors.allErrors}"
					log.error "Error saving File:\n ${thisFile.errors.allErrors}"
					flash.message = "There was a problem saving the following File(s): " + badFiles
					render(controller: 'dataset', view:'editData', model:[datasetInstance: dataset])
					return
				}
			}
		}
		
		log.info("\nMultiple File Upload: \n\tdataset = " + dataset.id + "\n\tgoodFiles = " + goodFiles)
		log.info("\nMultiple File Upload: \n\tdataset = " + dataset.id + "\n\terrorFiles = " + badFiles)
		
		/* WARNING: Dataset modification taking place here! */
			// Remove any and all deleted files from the dataset
			def _filesToBeDeleted = dataset.files.findAll {(it?.deleted || (it == null))}
			if (_filesToBeDeleted) {
				dataset.files.removeAll(_filesToBeDeleted)
			}
			
			// Update the indexes
			dataset.files.eachWithIndex() {f, i ->
				f.index = i
			}
			
			// Be sure to reset the isSubmitted flag if it's true!
			if (dataset.isSubmitted == true) {
				dataset.isSubmitted = false
			}
			
			dataset.save(flush: true)
		/* WARNING: Dataset modification taking place here! */
		
		redirect(controller: "dataset", action: "show", id: dataset.id)
	}
	
	def saveMultiple = {
		def dataset = Dataset.get(params.dataset.id)
		def uploadUtils = new meta.UploadUtils()
		def goodFiles = []
		def badFiles = []
		Map fileMap = ((MultipartHttpServletRequest)request).fileMap
		
		if(!dataset) {
			flash.message = "Data set not found with id ${params.id}"
			redirect(controller: 'dataset', action:list)
			return
		}
		if (!datasetAuthService.canEditDataset(dataset)) {
			flash.message = 'I\'m sorry, but you are not authorized to edit this data set.'
			redirect(controller: 'dataset', action:'show', id: dataset.id)
			return
		}
		
		// If we make it this far, then we can continue with the file saving!
		def theFiles = Arrays.asList(params.files)
		def fileListSize = (params.fileListTotal).toLong()
		
		def fcount = 0
		theFiles.each {f -> 
			for (fcount = 0; fcount < fileListSize; fcount += 1) {
				def thisFile
				Map fmap = new HashMap()
				
				if (fileListSize == 1) {
					fmap.put("id", f.get("id"))
					fmap.put("index", f.get("index"))
					fmap.put("deleted", f.get("deleted"))
					fmap.put("fileType", f.get("fileType"))
					fmap.put("format", f.get("format.id"))
				} else {
					fmap.put("id", f.id[fcount])
					fmap.put("index", f.index[fcount])
					fmap.put("deleted", f.deleted[fcount])
					fmap.put("fileType", f.fileType[fcount])
					fmap.put("format", f.format.id[fcount])
				}
				
				if ( fileListSize >= 1 && !(fmap.get("id")).equals("") ) {
					thisFile = File.get((fmap.get("id")).toLong())
				}
				boolean isNew = false
				if (!thisFile) {
					// It's not an existing file, so we need to create it!
					isNew = true
					thisFile = new File()
					thisFile.dateCreated = new Date()
					thisFile.fileVersion = 0
					thisFile.index = fmap.get("index")?.toInteger()
					
					thisFile.fileType = FileType.find { it.name() == fmap.get("fileType").toUpperCase() }
					
					thisFile.format = Format.get(fmap.get("format").toLong())
					thisFile.lastUpdated = new Date()
				} else {
					// If File exists, check if it's been deleted.
					if (fmap.get("deleted").toBoolean() == true) {
						thisFile.deleted = fmap.get("deleted").toBoolean()
						continue //return // like a continue in a while loop
					} else {
						thisFile.index = fmap.get("index")?.toInteger()
					
						thisFile.fileType = FileType.find { it.name() == fmap.get("fileType").toUpperCase() }
						
						thisFile.format = Format.get(fmap.get("format").toLong())
						thisFile.lastUpdated = new Date()
					}
				}				
				
				// So now that we have a fileInstance to work with, we need to look for a MultipartFile!
				def ffile = "files["+fcount+"].file"
				def fList = ((MultipartHttpServletRequest)request).getFiles(ffile) //fileMap.get(ffile)
				if (fList) {
					def fspot = 0
					for (fspot = 0; fspot < fList.size(); fspot += 1) {
						MultipartFile file = fList.get(fspot)
						log.info("upload: form name " + ffile)
		
						if(!file?.empty) {
							//println "OriginalFileName: ${file.originalFilename}"
							//println "Size: ${file.size}"
							//println "ContentType: ${file.contentType}"
							log.info("OriginalFileName: ${file.originalFilename}")
							log.info("Size: ${file.size}")
							log.info("ContentType: ${file.contentType}")
			
							def originalFilename = file.getOriginalFilename()
							def df = new java.io.File(originalFilename)
							def myFilename = uploadUtils.fixFileBadCharacters(uploadUtils.removeWindowsExtraStuff(df.getName()))
			
							log.info("upload: got file "+myFilename)
							
							myFilename += "_v" + (thisFile.fileVersion + 1) // include the file version number in filename
			
							def userDir = retrieveDestDirectory(dataset.owner.username) // change to dataset.owner.username next dev-cycle
							userDir = new java.io.File(userDir, "/dataset_${dataset.id}")
							userDir.mkdirs()
							
							def destFile = new java.io.File(userDir, myFilename)
							
							// Set group-writeable permissions for the directory and file
		//					if (!(userDir.toString() =~ /^C[:][\\]/)) {
		//						println "chmod 775 " + userDir.toString() + "\n"
		//						Runtime.getRuntime().exec("chmod 775" + userDir.toString())
		//					}
							
							try {
								file.transferTo( destFile )
		//						if (!(destFile.toString() =~ /^C[:][\\]/)) {
		//							println "chmod 664 " + destFile.toString() + "\n"
		//							Runtime.getRuntime().exec("chmod 664" + destFile.toString())
		//						}
								goodFiles += myFilename
								
								// If the upload is successful, update the size, name and directory of the fileInstance!
								thisFile.name = myFilename.toString()
								thisFile.directory = userDir.toString()
								thisFile.size = (file.size / 1024).toInteger()
								thisFile.fileVersion = (thisFile.fileVersion + 1) // Update the version since it has been updated
							} catch (e) {
								badFiles += myFilename
							}
						} else if (file?.empty && isNew == true) {
							log.info("File \""+f.name[fcount]+"\" was empty (file size of 0)")
							badFiles += "\""+f.name[fcount]+"\" was empty (file size of 0)"
						}
					}
				}
				
				if (isNew == true) {
					dataset.addToFiles(thisFile)
				}
				if (!thisFile.hasErrors() && thisFile.save()) {
					log.info "${thisFile} should be saved now."
				} else {
					//println "Error saving File:\n ${thisFile.errors.allErrors}"
					log.error "Error saving File:\n ${thisFile.errors.allErrors}"
					flash.message = "There was a problem saving the following File(s): " + badFiles
					render(controller: 'dataset', view:'editData', model:[datasetInstance: dataset])
					return
				}
			}
		}
		
		log.info("\nMultiple File Upload: \n\tdataset = " + dataset.id + "\n\tgoodFiles = " + goodFiles)
		log.info("\nMultiple File Upload: \n\tdataset = " + dataset.id + "\n\terrorFiles = " + badFiles)
		
		/* WARNING: Dataset modification taking place here! */
			// Remove any and all deleted files from the dataset
			def _filesToBeDeleted = dataset.files.findAll {(it?.deleted || (it == null))}
			if (_filesToBeDeleted) {
				dataset.files.removeAll(_filesToBeDeleted)
			}
			
			// Update the indexes
			dataset.files.eachWithIndex() {f, i ->
				f.index = i
			}			
			
			// Be sure to reset the isSubmitted flag if it's true!
			if (dataset.isSubmitted == true) {
				dataset.isSubmitted = false
			}
			
			dataset.save(flush: true)
		/* WARNING: Dataset modification taking place here! */
		
		redirect(controller: "dataset", action: "show", id: dataset.id)
	}
	
	def update() {
		def fileInstance = File.get(params.id)
		
		if (!fileInstance) {
			flash.message = "File not found with id ${params.id}"
			redirect(action: "list")
			return
		}
		
		fileInstance.properties = params
		
		// Handle the uploaded file, if it exists
		
		if (!fileInstance.save(flush: true)) {
			render(view: "edit", model: [fileInstance: fileInstance])
			return
		}

		flash.message = message(code: 'default.updated.message', args: [message(code: 'File.label', default: 'File'), fileInstance.id])
		redirect(action: "show", id: fileInstance.id)
	}
	
	
	
	/* -------------------------------------------------------------- *
	 * BEGIN PRIVATE/PROTECTED FUNCTIONS
	 * -------------------------------------------------------------- */
	private java.io.File retrieveDestDirectory(String username) {
		// Purpose: Retrieves the destination directory for file uploads.
		// 			If the path doesn't exist, create it!
		
		def config = grailsApplication.config
		def envDestDir = config.grails.fileDestRootLoc
		
		def webRootDir = new java.io.File(envDestDir)
		
		if (!webRootDir.exists()) {
			// If unable to find original directory path, use the current path.
			webRootDir = new java.io.File(servletContext.getRealPath("/"))
		}
		
		def userDir = new java.io.File(webRootDir, "/files/${username}")
		//println "userDir: ${userDir}"
		userDir.mkdirs()
		
		return userDir
	}
	
}