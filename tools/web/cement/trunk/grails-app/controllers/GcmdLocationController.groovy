class GcmdLocationController {
    def index = { redirect(action:list,params:params) }

    // the delete, save and update actions only
    // accept POST requests
    def allowedMethods = [delete:'POST',
                          save:'POST',
                          update:'POST']

    def list = {
        if(!params.max)params.max = 10
        [ gcmdLocationList: GcmdLocation.list( params ) ]
    }

    def show = {
        [ gcmdLocation : GcmdLocation.get( params.id ) ]
    }

    def delete = {
        def gcmdLocation = GcmdLocation.get( params.id )
        if(gcmdLocation) {
            gcmdLocation.delete()
            flash.message = 'GcmdLocation deleted'
            redirect(action:list)
        }
        else {
            flash.message = 'GcmdLocation not found'
            redirect(action:list)
        }
    }

    def edit = {
        def gcmdLocation = GcmdLocation.get( params.id )

        if(!gcmdLocation) {
                flash.message = 'GcmdLocation not found'
                redirect(action:list)
        }
        else {
            return [ gcmdLocation : gcmdLocation ]
        }
    }

    def update = {
        def gcmdLocation = GcmdLocation.get( params.id )
        if(gcmdLocation) {
             gcmdLocation.properties = params
            if(gcmdLocation.save()) {
                redirect(action:show,id:gcmdLocation.id)
            }
            else {
                render(view:'edit',model:[gcmdLocation:gcmdLocation])
            }
        }
        else {
            flash.message = 'GcmdLocation not found'
            redirect(action:edit,id:params.id)
        }
    }

    def create = {
        def gcmdLocation = new GcmdLocation()
        gcmdLocation.properties = params
        return ['gcmdLocation':gcmdLocation]
    }

    def save = {
        def gcmdLocation = new GcmdLocation()
        gcmdLocation.properties = params
        if(gcmdLocation.save()) {
            redirect(action:show,id:gcmdLocation.id)
        }
        else {
            render(view:'create',model:[gcmdLocation:gcmdLocation])
        }
    }

}
