class ProjectController {
    
    // the delete, save and update actions only accept POST requests
    def allowedMethods = [delete:'POST', save:'POST', update:'POST']

    def threddsXmlService



    def index = { redirect(action:list,params:params) }



    def list = {
        if(!params.max) params.max = 10
        [ projectList: Project.list( params ) ]
    }



    def show = { // view: show
        def project = Project.get( params.id )
        if (!project) {
            flash.message = 'Project not found'
            redirect(action:list)
            return
        }
        else return [ project : project ]
    }



    def delete = {
        def project = Project.get( params.id )
        if(project) {
            project.delete()
            flash.message = 'Project deleted'
            redirect(action:list)
        }
        else {
            flash.message = 'Project not found'
            redirect(action:list)
        }
    }



    def edit = {
        def project = Project.get( params.id )

        if(!project) {
            flash.message = 'Project not found'
            redirect(action:list)
        }
        else {
            return [ project : project ]
        }
    }



    def update = {
        def project = Project.get( params.id )
        if(project) {
            project.properties = params
            project.entryID = Project.entryIDprefix +
              project.theDisciplineEntryID() + '.' +
              project.theTitleEntryID()
            if(!project.hasErrors() && project.save()) {
                flash.message = 'Project updated'
                redirect(action:show,id:project.id)
            }
            else {
                render(view:'edit',model:[project:project])
            }
        }
        else {
            flash.message = 'Project not found'
            redirect(action:edit,id:params.id)
        }
    }



    def create = {
        def project = new Project()
        project.properties = params
        def me = Contact.findByShortName(session.username)
        if (me) project.piContact = me
            return ['project':project]
    }



    def save = {
        def project = new Project(params)
        project.entryID = Project.entryIDprefix +
            project.theDisciplineEntryID() + '.' +
            project.theTitleEntryID()
        if(!project.hasErrors() && project.save()) {
            flash.message = "Project ${project.id} created"
            redirect(action:show,id:project.id)
        }
        else {
            render(view:'create',model:[project:project])
        }
    }



    def metadata = {
        log.debug("metadata: params = ${params}")
        def project = null
        if (params.id) {
            if (params.id =~ /^\d+$/)
                project = Project.get( params.id )
            else project = Project.findByEntryID(params.id)
        }
        log.debug("metadata: project = ${project}")
        if (!project) {
            render(contentType:'text/xml', encoding:'UTF-8', text:'<metadata></metadata>')
            return
        }
        [project:project]
    }



    def validateXml = {
        def project = Project.get( params.id )
        if(!project) {
            flash.message = 'Project not found'
            redirect(action:list)
            return
        }

        def xml = g.render(template:'metadata', bean:project)
        response.contentType = 'text/html' // reset after metadata template, in case of error
        //log.debug("project xml = ${xml}")
        def errs = threddsXmlService.validate(xml)
        if (errs) {
            flash.message = 'Project does not produce valid XML. Please edit. ' + errs
            redirect(action:edit,id:project.id)
            return
        }
        else {
            flash.message = 'Project produces valid XML.'
            redirect(action:show,id:project.id)
        }
    }

}
