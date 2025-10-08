import org.hibernate.FetchMode as FM

class DatasetController {

    AuthenticateService authenticateService

    DatasetAuthorizationService datasetAuthorizationService
    ProjectAuthorizationService projectAuthorizationService

    def showAuth = {
      def principal = authenticateService.principal()
      // println principal.getUsername()			//get username
      // println principal.getAuthorities()		//get authorities
    }

    def index = { redirect(action:list,params:params) }

    // the delete, save and update actions only accept POST requests
    def allowedMethods = [delete:'POST', save:'POST', update:'POST']

	// to get a nice button in edit view
    def cancel = { 
      redirect(action:show, id:params.id)
    }

    def list = {
        if(!params.max) params.max = 20
        [ datasetList: Dataset.list( params ) ]
    }

    def show = {
        def dataset = Dataset.get( params.id )

		// inserted next 3 lines for debugging 
       log.debug("System.properties = " + System.properties.toString())
       log.debug("s.gp catalina.base = " + System.getProperty('catalina.base'))
       log.debug("s.p.gp catalina.base = " + System.properties.getProperty('catalina.base'))

        if(!dataset) {
            flash.message = "Dataset not found with id ${params.id}"
            redirect(action:list)
        }
        else { return [ dataset : dataset ] }
    }

    def delete = {
        def dataset = Dataset.get( params.id )
        if(!dataset) {
           	flash.message = "Dataset not found with id ${params.id}"
           	redirect(action:list)
        //  redirect(action:review)
       	}
		else {
        	if (!datasetAuthorizationService.canEditDataset(dataset)) {
            	flash.message = 'Not authorized to delete dataset.'
            	redirect(action:'show',id:dataset.id)
        	}
			else {
	            dataset.delete()
    	        flash.message = "Dataset ${params.id} deleted"
        	    redirect(action:list)
        	//  redirect(action:review)
       		}
	  	}
   }

    def edit = {
        def dataset = Dataset.get( params.id )
        if(!dataset) {
            flash.message = "Dataset not found with id ${params.id}"
            redirect(action:list)
        //  redirect(action:review)
        }
        else {
        	if (!datasetAuthorizationService.canEditDataset(dataset)) {
            	flash.message = 'Not authorized to edit dataset.'
            	redirect(action:'show',id: dataset.id)
        	}
			else {
        		def myList = projectAuthorizationService.permittedProjects()
        		return ['dataset': dataset, 'myList': myList]
        		//  return [ dataset : dataset ]
			}
        }
    }

    def update = {
        def dataset = Dataset.get( params.id )
        if(dataset) {
        	if (!datasetAuthorizationService.canEditDataset(dataset)) {
            	flash.message = 'Not authorized to update dataset.'
            	redirect(action:'show',id:dataset.id)
        	}
			else {
            	dataset.properties = params
		        if (!projectAuthorizationService.isaPermittedProject(dataset.project)) {
            		flash.message = 'Not authorized to add dataset to chosen project.'
            		def myList = projectAuthorizationService.permittedProjects()
            		if (!myList?.size()) flash.message += ' Not authorized for any projects.'
            		render(view:'edit', model:[dataset:dataset, 'myList':myList])
          		}
				else {
            		if(!dataset.hasErrors() && dataset.save()) {
                		flash.message = "Dataset ${params.id} updated"
                		redirect(action:show,id:dataset.id)
            		}
            		else {
            			def myList = projectAuthorizationService.permittedProjects()
            			render(view:'edit', model:[dataset:dataset, 'myList':myList])
            		}
				}
			}
        }
        else {
            flash.message = "Dataset not found with id ${params.id}"
            redirect(action:edit,id:params.id)
        }
    }

    def create = {
        def dataset = new Dataset()
        dataset.properties = params
      	def principal = authenticateService.principal()
      	def uname = principal.getUsername()			// get username
        def me = Researcher.findByUsername(uname)	// get user id
        if (me) dataset.metadataContact = me
        def myList = projectAuthorizationService.permittedProjects()
        return ['dataset':dataset, 'myList': myList]
    }

    def save = {
        def dataset = new Dataset(params)
        if (!projectAuthorizationService.isaPermittedProject(dataset.project)) {
            flash.message = 'Not authorized to save to chosen project.'
            def myList = projectAuthorizationService.permittedProjects()
            render(view:'create', model:[dataset:dataset, 'myList':myList])
        }
		else {
        	if(!dataset.hasErrors() && dataset.save()) {
            	flash.message = "Dataset ${dataset.id} created"
            	redirect(action:show, id:dataset.id)
        	}
        	else {
            	def myList = projectAuthorizationService.permittedProjects()
            	render(view:'create', model:[dataset:dataset, 'myList':myList])
        	}
    	}
	}

    def template = {
    	log.debug('template')
    	def dataset = null
    	def datasets = null
    	if (params.id != null) {
        	log.debug("Trying params.id = ${params.id}")
        	if (params.id =~ /^\d+$/) {
            	dataset = Dataset.get( params.id )
          		log.debug(" got dataset = ${dataset}")
      			def principal = authenticateService.principal()
      			def uname = principal.getUsername()			// get username
        		def me = Researcher.findByUsername(uname)	// get user id
        		if (me) dataset.metadataContact = me
      			// found a dataset to use as a template, so pass it to create view
        		def myList = projectAuthorizationService.permittedProjects()
      			render(view:'create', model:[dataset:dataset, 'myList':myList])
			}
			else {
        		flash.message = 'Dataset not found'
        		// redirect to create and let it try its logic to find a project
        		if (params.id != null) 
					params.remove('id')
        		redirect(action:'create', params:params)
        	}
      	}
	}

    def review = {
    	def principal = authenticateService.principal()
    	def uname = principal.getUsername()				// get username
    	def me = Researcher.findByUsername(uname)		// get user id

    	if (me) {
        	def criteria = Dataset.createCriteria()
        	def results = criteria {
          		or {
            		eq('metadataContact', me)
            		project {
              			eq('piContact', me)
              		}
            	}
          		fetchMode('project', FM.EAGER)
        	}
        	return [ contact: me, datasets: results ]
      	}
    	else {
        	flash.message = "Bad error - can't find your contact info."
      	}
    }

}
