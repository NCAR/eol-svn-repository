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
        log.error("dataset/edit: params = "+params)
        def dataset = Dataset.get( params.id )
        log.error("dataset/edit: dataset = "+dataset)

        if(!dataset) {
                flash.message = "Dataset not found with id ${params.id}"
                redirect(action:list)
        }
        else {
            log.error("  again: dataset = "+dataset)
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

    def publish = {
      def dataset = Dataset.get( params.id )
      if (!dataset) {
        flash.message = "Dataset not found with id ${params.id}"
        redirect(action:list)
	return
        }

      params['redir'] = ("development" == System.getProperty("grails.env")) ? 'echo' : 'cdp'

      params['cdpid'] = "org.nsf.aon.cadis.dataset."+dataset.id
      params['cdptext'] = dataset.summary

      params['cdpxml'] = """
<metadata xsi:schemaLocation="http://www.unidata.ucar.edu/namespaces/thredds/InvCatalog/v1.0 http://www.unidata.ucar.edu/schemas/thredds/InvCatalog.1.0.xsd" metadataType="THREDDS" inherited="true">
<documentation type="summary">
${dataset.summary}
</documentation>
<contributor role="principalInvestigator">${dataset.piContact.toCdpString()}</contributor>
</metadata>
""".toString()

      redirect(controller:'work',action:'done',params:params)
    }

}
