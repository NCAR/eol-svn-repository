class IsoTopicController {
    
    def index = { redirect(action:list,params:params) }

    // the delete, save and update actions only accept POST requests
    def allowedMethods = [delete:'POST', save:'POST', update:'POST']

    def list = {
        if(!params.max) params.max = 10
        [ isoTopicList: IsoTopic.list( params ) ]
    }

    def show = {
        [ isoTopic : IsoTopic.get( params.id ) ]
    }

    def delete = {
        def isoTopic = IsoTopic.get( params.id )
        if(isoTopic) {
            isoTopic.delete()
            flash.message = 'IsoTopic deleted'
            redirect(action:list)
        }
        else {
            flash.message = 'IsoTopic not found'
            redirect(action:list)
        }
    }

    def edit = {
        def isoTopic = IsoTopic.get( params.id )

        if(!isoTopic) {
            flash.message = 'IsoTopic not found'
            redirect(action:list)
        }
        else {
            return [ isoTopic : isoTopic ]
        }
    }

    def update = {
        def isoTopic = IsoTopic.get( params.id )
        if(isoTopic) {
            isoTopic.properties = params
            if(!isoTopic.hasErrors() && isoTopic.save()) {
                flash.message = 'IsoTopic updated'
                redirect(action:show,id:isoTopic.id)
            }
            else {
                render(view:'edit',model:[isoTopic:isoTopic])
            }
        }
        else {
            flash.message = 'IsoTopic not found'
            redirect(action:edit,id:params.id)
        }
    }

    def create = {
        def isoTopic = new IsoTopic()
        isoTopic.properties = params
        return ['isoTopic':isoTopic]
    }

    def save = {
        def isoTopic = new IsoTopic(params)
        if(!isoTopic.hasErrors() && isoTopic.save()) {
            flash.message = "IsoTopic ${isoTopic.id} created"
            redirect(action:show,id:isoTopic.id)
        }
        else {
            render(view:'create',model:[isoTopic:isoTopic])
        }
    }
}
