            
class GcmdScienceController extends BaseController {
    def index = { redirect(action:list,params:params) }

    // the delete, save and update actions only
    // accept POST requests
    def allowedMethods = [delete:'POST',
                          save:'POST',
                          update:'POST']

    def list = {
        if(!params.max)params.max = 10
        [ gcmdScienceList: GcmdScience.list( params ) ]
    }

    def show = {
        [ gcmdScience : GcmdScience.get( params.id ) ]
    }

    def delete = {
        def gcmdScience = GcmdScience.get( params.id )
        if(gcmdScience) {
            gcmdScience.delete()
            flash.message = "GcmdScience ${params.id} deleted."
            redirect(action:list)
        }
        else {
            flash.message = "GcmdScience not found with id ${params.id}"
            redirect(action:list)
        }
    }

    def edit = {
        def gcmdScience = GcmdScience.get( params.id )

        if(!gcmdScience) {
                flash.message = "GcmdScience not found with id ${params.id}"
                redirect(action:list)
        }
        else {
            return [ gcmdScience : gcmdScience ]
        }
    }

    def update = {
        def gcmdScience = GcmdScience.get( params.id )
        if(gcmdScience) {
             gcmdScience.properties = params
            if(gcmdScience.save()) {
                redirect(action:show,id:gcmdScience.id)
            }
            else {
                render(view:'edit',model:[gcmdScience:gcmdScience])
            }
        }
        else {
            flash.message = "GcmdScience not found with id ${params.id}"
            redirect(action:edit,id:params.id)
        }
    }

    def create = {
        def gcmdScience = new GcmdScience()
        gcmdScience.properties = params
        return ['gcmdScience':gcmdScience]
    }

    def save = {
        def gcmdScience = new GcmdScience()
        gcmdScience.properties = params
        if(gcmdScience.save()) {
            redirect(action:show,id:gcmdScience.id)
        }
        else {
            render(view:'create',model:[gcmdScience:gcmdScience])
        }
    }

}
