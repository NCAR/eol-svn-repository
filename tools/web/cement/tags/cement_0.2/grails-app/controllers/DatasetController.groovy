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
        log.debug("update params = ${params}")
        def dataset = Dataset.get( params.id )
        if(dataset) {
            log.debug("datacenter = ${dataset.datacenterContact}")
            dataset.properties = params
            log.debug("datacenter = ${dataset.datacenterContact}")
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

    def metadata = {
        log.debug("metadata: params = ${params}")
        def d = params.id ? Dataset.get( params.id ) : null
        log.debug("metadata: d = ${d}")
	if (!d) {
	  render(contentType:"text/xml", encoding:"UTF-8", text:null)
	  return
	  }
	[dataset:d]
    }

}
