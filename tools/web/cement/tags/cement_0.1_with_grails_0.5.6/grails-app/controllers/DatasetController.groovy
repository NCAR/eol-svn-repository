class DatasetController extends BaseController {
    def index = { redirect(action:list,params:params) }

    // the delete, save and update actions only
    // accept POST requests
    def allowedMethods = [delete:'POST',
                          save:'POST',
                          update:'POST']

    def list = {
        if(!params.max)params.max = 10
        [ datasetList: Dataset.list( params ) ]
    }

    def show = {
        [ dataset : Dataset.get( params.id ) ]
    }

    def letsgo = {
        def me = Contact.get( params.id )
	if (!me) {
	  me = User.findByEmail(session.email)?.myContact
	}
	if (me) {
	  log.info("contact:letsgo found me "+me.id+" "+me.email)
	  def criteria = Dataset.createCriteria()
	  def results = criteria {
	     or {
	        eq('piContact',me)
	        eq('metadataContact',me)
	        eq('datacenterContact',me)
	  	 }
	  }
      return [ contact : me, datasets : results ]
	}
	else {
            flash.message = "No contact information found for you! Please create it."
            redirect(action:create,params:[email:session.email])
	}
    }

    def delete = {
        def dataset = Dataset.get( params.id )
        if(dataset) {
            dataset.delete()
            flash.message = "Dataset ${params.title} deleted."
            redirect(action:list)
        }
        else {
            flash.message = "Dataset not found with id ${params.id}"
            redirect(action:list)
        }
    }

    def edit = {
        def dataset = Dataset.get( params.id )

        if(!dataset) {
                flash.message = "Dataset not found with id ${params.id}"
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
            if(dataset.save()) {
				flash.message = "${params.title} updated."
                redirect(action:show,id:dataset.id)
            }
            else {
                render(view:'edit',model:[dataset:dataset])
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
        return ['dataset':dataset]
    }

    def save = {
        def dataset = new Dataset()
        dataset.properties = params
        if(dataset.save()) {
			flash.message = "${params.title} saved."
            redirect(action:show,id:dataset.id)
        }
        else {
            render(view:'create',model:[dataset:dataset])
        }
    }

}
