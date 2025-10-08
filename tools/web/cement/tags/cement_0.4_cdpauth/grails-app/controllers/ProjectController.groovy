class ProjectController {
    
    def index = { redirect(action:list,params:params) }

    // the delete, save and update actions only accept POST requests
    def allowedMethods = [delete:'POST', save:'POST', update:'POST']

    def list = {
        if(!params.max) params.max = 10
        [ projectList: Project.list( params ) ]
    }

    def show = {
        [ project : Project.get( params.id ) ]
    }

    def delete = {
        def project = Project.get( params.id )
        if(project) {
            project.delete()
            flash.message = "Project ${params.id} deleted"
            redirect(action:list)
        }
        else {
            flash.message = "Project not found with id ${params.id}"
            redirect(action:list)
        }
    }

    def edit = {
        def project = Project.get( params.id )

        if(!project) {
            flash.message = "Project not found with id ${params.id}"
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
            if(!project.hasErrors() && project.save()) {
                flash.message = "Project ${params.id} updated"
                redirect(action:show,id:project.id)
            }
            else {
                render(view:'edit',model:[project:project])
            }
        }
        else {
            flash.message = "Project not found with id ${params.id}"
            redirect(action:edit,id:params.id)
        }
    }

    def create = {
        def project = new Project()
        project.properties = params
        return ['project':project]
    }

    def save = {
        def project = new Project(params)
        if(!project.hasErrors() && project.save()) {
            flash.message = "Project ${project.id} created"
            redirect(action:show,id:project.id)
        }
        else {
            render(view:'create',model:[project:project])
        }
    }
}
