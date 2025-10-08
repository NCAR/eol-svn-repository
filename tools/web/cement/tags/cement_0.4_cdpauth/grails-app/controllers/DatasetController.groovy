import org.hibernate.FetchMode as FM

class DatasetController {
    
    def index = { redirect(action:list,params:params) }

    // the delete, save and update actions only accept POST requests
    def allowedMethods = [delete:'POST', save:'POST', update:'POST']

    def list = {
        if(!params.max) params.max = 10
        [ datasetList: Dataset.list( params ) ]
    }

    def show = {
        [ dataset : Dataset.get( params.id ) ]
    }

    def delete = {
        def dataset = Dataset.get( params.id )
        if(dataset) {
            dataset.delete()
            flash.message = "Dataset deleted"
            redirect(action:list)
        }
        else {
            flash.message = "Dataset not found"
            redirect(action:list)
        }
    }

    def edit = {
        def dataset = null
        if (params.id) {
          if (params.id =~ /^\d+$/)
            dataset = Dataset.get( params.id )
          else dataset = Dataset.findByEntryID(params.id)
          }
        if (!dataset) {
	  if (session['cdp.hash']?.datasetId)
	    dataset = Dataset.findByEntryID(session['cdp.hash'].datasetId)
	  }

        if(!dataset) {
            flash.message = "Dataset not found"
            redirect(action:list)
        }
        else {
            return [ dataset : dataset ]
        }
    }

    def update = {
        def dataset = Dataset.get( params.id )
        if(dataset) {
            dataset.properties = params
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

    def create = {
        def dataset = new Dataset()
        dataset.properties = params
        return ['dataset':dataset]
    }

    def template = {
        def dataset = null
        if (params.id) {
          if (params.id =~ /^\d+$/)
            dataset = Dataset.get( params.id )
          else dataset = Dataset.findByEntryID(params.id)
          }
        if (!dataset) {
	  if (session['cdp.hash']?.datasetId)
	    dataset = Dataset.findByEntryID(session['cdp.hash'].datasetId)
	  }
        if (!dataset) {
          flash.message = "Dataset not found"
          dataset = new Dataset()
          dataset.properties = params
	}
	dataset.id = null
	dataset.entryID = null
        render(view:'create',model:['dataset':dataset])
    }

  private static entryIDprefix = "org.nsf.aon.cadis."

    def save = {
        def dataset = new Dataset(params)
	dataset.entryID = this.entryIDprefix +
	  dataset.theDisciplineEntryID() + "." +
	  dataset.project?.theTitleEntryID() + "." +
	  dataset.theTitleEntryID()
        if(!dataset.hasErrors() && dataset.save()) {
            flash.message = "Dataset created"
            redirect(action:show,id:dataset.id)
        }
        else {
            render(view:'create',model:[dataset:dataset])
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

}
