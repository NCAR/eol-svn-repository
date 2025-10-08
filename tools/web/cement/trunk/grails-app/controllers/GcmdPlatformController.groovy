class GcmdPlatformController {
    
    def index = { redirect(action:list,params:params) }

    // the delete, save and update actions only accept POST requests
    def allowedMethods = [delete:'POST', save:'POST', update:'POST']

    def list = {
        if(!params.max) params.max = 10
        [ gcmdPlatformList: GcmdPlatform.list( params ) ]
    }

    def show = {
        [ gcmdPlatform : GcmdPlatform.get( params.id ) ]
    }

    def delete = {
        def gcmdPlatform = GcmdPlatform.get( params.id )
        if(gcmdPlatform) {
            gcmdPlatform.delete()
            flash.message = 'GcmdPlatform deleted'
            redirect(action:list)
        }
        else {
            flash.message = 'GcmdPlatform not found'
            redirect(action:list)
        }
    }

    def edit = {
        def gcmdPlatform = GcmdPlatform.get( params.id )

        if(!gcmdPlatform) {
            flash.message = 'GcmdPlatform not found'
            redirect(action:list)
        }
        else {
            return [ gcmdPlatform : gcmdPlatform ]
        }
    }

    def update = {
        def gcmdPlatform = GcmdPlatform.get( params.id )
        if(gcmdPlatform) {
            gcmdPlatform.properties = params
            if(!gcmdPlatform.hasErrors() && gcmdPlatform.save()) {
                flash.message = 'GcmdPlatform updated'
                redirect(action:show,id:gcmdPlatform.id)
            }
            else {
                render(view:'edit',model:[gcmdPlatform:gcmdPlatform])
            }
        }
        else {
            flash.message = 'GcmdPlatform not found'
            redirect(action:edit,id:params.id)
        }
    }

    def create = {
        def gcmdPlatform = new GcmdPlatform()
        gcmdPlatform.properties = params
        return ['gcmdPlatform':gcmdPlatform]
    }

    def save = {
        def gcmdPlatform = new GcmdPlatform(params)
        if(!gcmdPlatform.hasErrors() && gcmdPlatform.save()) {
            flash.message = "GcmdPlatform ${gcmdPlatform.id} created"
            redirect(action:show,id:gcmdPlatform.id)
        }
        else {
            render(view:'create',model:[gcmdPlatform:gcmdPlatform])
        }
    }
}
