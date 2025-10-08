import org.hibernate.FetchMode as FM

class DatasetController {
    
    def index = { redirect(action:list,params:params) }

    // the delete, save and update actions only accept POST requests
    def allowedMethods = [delete:'POST', save:'POST', update:'POST']



    def list = { // view: list
        if(!params.max) params.max = 20
        [ datasetList: Dataset.list( params ) ]
    }



    def show = { // view: show
        [ dataset : Dataset.get( params.id ) ]
    }



    def delete = { // views: from=edit, to=list
        def dataset = Dataset.get( params.id )
        if(dataset) {
            dataset.delete()
            flash.message = "Dataset deleted"
            redirect(action:review)
        }
        else {
            flash.message = "Dataset not found"
            redirect(action:list)
        }
    }



    def edit = { // views: from=(none), display=edit, next_to=save,update,
        log.debug('edit')
        def dataset = null

        if (params.id) {
	  log.debug("Trying params.id = ${params.id}")
          if (params.id =~ /^\d+$/)
            dataset = Dataset.get( params.id )
          else dataset = Dataset.findByEntryID(params.id)
	  log.debug(" got dataset = ${dataset}")
          }

        if (!dataset) {
	  log.debug("no dataset, looking at hash ${session['cdp.hash']}")
	  if (session['cdp.hash']?.datasetId) {
	    log.debug("Trying hash.datasetId ${session['cdp.hash'].datasetId}")
	    dataset = Dataset.findByEntryID(session['cdp.hash'].datasetId)
	    log.debug(" got dataset = ${dataset}")
	    }
	  }

        if(!dataset) {
            flash.message = "Dataset not found"
            redirect(action:review)
        }
        else {
            return [ dataset : dataset ]
        }
    }



    def update = { // view: from=edit ("Update")
        def dataset = Dataset.get( params.id )
        if(dataset) {

	    // keep original entryID (alternative is reset it below)
	    if (params.entryID) params.remove("entryID")

            dataset.properties = params

	    // reset entryID (alternative is keep original above)
	    //dataset.entryID = dataset.project?.entryID + "." + dataset.theTitleEntryID()

            if(!dataset.hasErrors() && dataset.save()) {
                flash.message = "Dataset updated"
                redirect(action:show,id:dataset.id)
            }
            else {
                render(view:'edit',model:[dataset:dataset])
            }
        }
        else {
            flash.message = "Dataset not found"
            redirect(action:edit,id:params.id)
        }
    }



    def create = { // view: create
      // initial call: make a blank dataset and give it to view
      // for redirects on save errors: set up a dataset object from params and give to view

      log.debug("create params = ${params}")

        def dataset = new Dataset()
        def project = null

	// we are creating a new dataset, but CDP might send a THREDDS ID
	//   assume it's for a project
	if (session['cdp.hash']?.datasetId) {
	  log.debug("*** a dataset id from the cdp.hash = (session['cdp.hash']?.datasetId)")
	  project = Project.findByEntryID(session['cdp.hash'].datasetId)
	  }

        dataset.properties = params
	dataset.project = project

	def me = Contact.findByShortName(session.username)
	if (me) dataset.metadataContact = me

        return ['dataset':dataset]
    }



    def save = { // views: from=create,edit, success=show, error=create
        log.debug("save params.id = ${params?.id}")
        if (params.id) params.remove('id') // if coming from edit, or malicious user
        def dataset = new Dataset()
        dataset.properties = params
 	    dataset.entryID = dataset.project?.entryID + "." + dataset.theTitleEntryID()
        if(!dataset.hasErrors() && dataset.save()) {
            flash.message = "Dataset created"
            redirect(action:show, id:dataset.id)
        }
        else {
            render(view:'create', model:[dataset:dataset])
        }
    }



    def cancel = { 	// here so we get a nice button in edit view
      redirect(action:show, id:params.id)
    }



    def template = {
      log.debug('template')
      def dataset = null
      def datasets = null

      if (params.id) {
	log.debug("Trying params.id = ${params.id}")
        if (params.id =~ /^\d+$/)
          dataset = Dataset.get( params.id )
        else dataset = Dataset.findByEntryID(params.id)
	log.debug(" got dataset = ${dataset}")
        }

      if (!dataset) log.debug("no dataset, looking at hash ${session['cdp.hash']}")
      if ((!dataset) && (session['cdp.hash']?.datasetId)) {
	log.debug("Trying hash.datasetId ${session['cdp.hash'].datasetId}")
        dataset = Dataset.findByEntryID(session['cdp.hash'].datasetId)
        if (!dataset) {
	  log.debug('no dataset, trying projects')
	  datasets = Project.findByEntryID(session['cdp.hash'].datasetId)?.datasets
	  if (!datasets?.size) {
	    log.debug('no datasets, trying dataset like')
	    datasets = Dataset.findAllByEntryIDLike(session['cdp.hash'].datasetId + "%")
	    }
	  log.debug("found ${datasets?.size} datasets")
	  switch (datasets?.size) {
	   case 1:
	     dataset = datasets[0]
             break
           case 0:
           case null:
             dataset = null
             break
           default: // multiple
	     log.debug('multiple datasets, rendering review')
             render(view:'review',model:[contact:Contact.findByShortName(session.username), datasets:datasets], 'what':'template')
             return
           }
          }
        }

      if (!dataset) {
        flash.message = "Dataset not found"
	// redirect to create and let it try it's logic to find a project
        if (params.id) params.remove('id')
	redirect(action:'create',params:params)
	return
        }

      def me = Contact.findByShortName(session.username)
      if (me) dataset.metadataContact = me

      // found a dataset to use as a template, so pass it to create view
      render(view:'create',model:['dataset':dataset])
      }



    def review = {
        log.debug("review letsgo")
        log.debug("params ${params}")
        log.debug("request ${request}")
        log.debug("session ${session}")

        def me = null
        if (params.id) me = Contact.get( params.id )
	if (!me) me = Contact.findByShortName(session.username)

	// XXX dig out CDP datasetId for like

	if (me) {
	  log.info("review found me "+me.id+" "+me.email)
	  def criteria = Dataset.createCriteria()
	  def results = criteria {
	  // XXX and { like('entryID',cdpDatasetId+'%')
	     or {
	        eq('metadataContact',me)
	        eq('datacenterContact',me)
	        project {
	          eq('piContact',me)
	          }
	  	}
	     fetchMode('project', FM.EAGER)
	  }
        return [ contact : me, datasets : results ]
	}
	else {
            flash.message = "Bad error - can't find your contact info."
	}
    }



    def metadata = {
        log.debug("metadata: params = ${params}")
        def d = params.id ? Dataset.get( params.id ) : null
        log.debug("metadata: d = ${d}")
        if (!d) {
          render(contentType:"text/xml", encoding:"UTF-8", text:"")
          return
          }
        [dataset:d]
    }


}
