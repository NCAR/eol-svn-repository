class GcmdProjectController {
    def index = { redirect(action:list,params:params) }

    // the delete, save and update actions only
    // accept POST requests
    def allowedMethods = [delete:'POST',
                          save:'POST',
                          update:'POST']

    def list = {
        if(!params.max)params.max = 10
        [ gcmdProjectList: GcmdProject.list( params ) ]
    }

    def show = {
        [ gcmdProject : GcmdProject.get( params.id ) ]
    }

    def delete = {
        def gcmdProject = GcmdProject.get( params.id )
        if(gcmdProject) {
            gcmdProject.delete()
            flash.message = "GcmdProject ${params.id} deleted."
            redirect(action:list)
        }
        else {
            flash.message = "GcmdProject not found with id ${params.id}"
            redirect(action:list)
        }
    }

    def edit = {
        def gcmdProject = GcmdProject.get( params.id )

        if(!gcmdProject) {
                flash.message = "GcmdProject not found with id ${params.id}"
                redirect(action:list)
        }
        else {
            return [ gcmdProject : gcmdProject ]
        }
    }

    def update = {
        def gcmdProject = GcmdProject.get( params.id )
        if(gcmdProject) {
             gcmdProject.properties = params
            if(gcmdProject.save()) {
                redirect(action:show,id:gcmdProject.id)
            }
            else {
                render(view:'edit',model:[gcmdProject:gcmdProject])
            }
        }
        else {
            flash.message = "GcmdProject not found with id ${params.id}"
            redirect(action:edit,id:params.id)
        }
    }

    def create = {
        def gcmdProject = new GcmdProject()
        gcmdProject.properties = params
        return ['gcmdProject':gcmdProject]
    }

    def save = {
        def gcmdProject = new GcmdProject()
        gcmdProject.properties = params
        if(gcmdProject.save()) {
            redirect(action:show,id:gcmdProject.id)
        }
        else {
            render(view:'create',model:[gcmdProject:gcmdProject])
        }
    }

}
