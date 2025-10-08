class FormatController {
    
    def index = { redirect(action:list,params:params) }

    // the delete, save and update actions only accept POST requests
    def allowedMethods = [delete:'POST', save:'POST', update:'POST']

    def list = {
        if(!params.max) params.max = 10
        [ formatList: Format.list( params ) ]
    }

    def show = {
        [ format : Format.get( params.id ) ]
    }

    def delete = {
        def format = Format.get( params.id )
        if(format) {
            format.delete()
            flash.message = 'Format deleted'
            redirect(action:list)
        }
        else {
            flash.message = 'Format not found'
            redirect(action:list)
        }
    }

    def edit = {
        def format = Format.get( params.id )

        if(!format) {
            flash.message = 'Format not found'
            redirect(action:list)
        }
        else {
            return [ format : format ]
        }
    }

    def update = {
        def format = Format.get( params.id )
        if(format) {
            format.properties = params
            if(!format.hasErrors() && format.save()) {
                flash.message = 'Format updated'
                redirect(action:show,id:format.id)
            }
            else {
                render(view:'edit',model:[format:format])
            }
        }
        else {
            flash.message = 'Format not found'
            redirect(action:edit,id:params.id)
        }
    }

    def create = {
        def format = new Format()
        format.properties = params
        return ['format':format]
    }

    def save = {
        def format = new Format(params)
        if(!format.hasErrors() && format.save()) {
            flash.message = "Format ${format.id} created"
            redirect(action:show,id:format.id)
        }
        else {
            render(view:'create',model:[format:format])
        }
    }
}
