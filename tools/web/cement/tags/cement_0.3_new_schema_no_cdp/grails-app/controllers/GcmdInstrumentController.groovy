class GcmdInstrumentController extends BaseController {
    def index = { redirect(action:list,params:params) }

    // the delete, save and update actions only accept POST requests
    def allowedMethods = [delete:'POST', save:'POST', update:'POST']

    def list = {
        if(!params.max) params.max = 10
        [ gcmdInstrumentList: GcmdInstrument.list( params ) ]
    }

    def show = {
        [ gcmdInstrument : GcmdInstrument.get( params.id ) ]
    }

    def delete = {
        def gcmdInstrument = GcmdInstrument.get( params.id )
        if(gcmdInstrument) {
            gcmdInstrument.delete()
            flash.message = "GcmdInstrument ${params.id} deleted"
            redirect(action:list)
        }
        else {
            flash.message = "GcmdInstrument not found with id ${params.id}"
            redirect(action:list)
        }
    }

    def edit = {
        def gcmdInstrument = GcmdInstrument.get( params.id )

        if(!gcmdInstrument) {
            flash.message = "GcmdInstrument not found with id ${params.id}"
            redirect(action:list)
        }
        else {
            return [ gcmdInstrument : gcmdInstrument ]
        }
    }

    def update = {
        def gcmdInstrument = GcmdInstrument.get( params.id )
        if(gcmdInstrument) {
            gcmdInstrument.properties = params
            if(!gcmdInstrument.hasErrors() && gcmdInstrument.save()) {
                flash.message = "GcmdInstrument ${params.id} updated"
                redirect(action:show,id:gcmdInstrument.id)
            }
            else {
                render(view:'edit',model:[gcmdInstrument:gcmdInstrument])
            }
        }
        else {
            flash.message = "GcmdInstrument not found with id ${params.id}"
            redirect(action:edit,id:params.id)
        }
    }

    def create = {
        def gcmdInstrument = new GcmdInstrument()
        gcmdInstrument.properties = params
        return ['gcmdInstrument':gcmdInstrument]
    }

    def save = {
        def gcmdInstrument = new GcmdInstrument(params)
        if(!gcmdInstrument.hasErrors() && gcmdInstrument.save()) {
            flash.message = "GcmdInstrument ${gcmdInstrument.id} created"
            redirect(action:show,id:gcmdInstrument.id)
        }
        else {
            render(view:'create',model:[gcmdInstrument:gcmdInstrument])
        }
    }
}