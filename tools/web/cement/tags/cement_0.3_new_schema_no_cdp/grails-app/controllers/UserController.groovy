class UserController extends BaseController {
    def beforeInterceptor = [action:this.&auth,except:['login','create','save']]

    def index = { redirect(action:login) }

    // the delete, save and update actions only
    // accept POST requests
    def allowedMethods = [delete:'POST',
                          save:'POST',
                          update:'POST']

    def list = {
        if(!params.max)params.max = 10
        [ userList: User.list( params ) ]
    }

    def show = {
        [ user : User.get( params.id ) ]
    }

    def delete = {
        def user = User.get( params.id )
        if(user) {
            user.delete()
            flash.message = "User ${params.id} deleted."
            redirect(action:list)
        }
        else {
            flash.message = "User not found with id ${params.id}"
            redirect(action:list)
        }
    }

    def edit = {
        def user = User.get( params.id )

        if(!user) {
                flash.message = "User not found with id ${params.id}"
                redirect(action:list)
        }
        else {
            return [ user : user ]
        }
    }

    def update = {
        def user = User.get( params.id )
        if(user) {
             user.properties = params
            if(user.save()) {
                redirect(action:show,id:user.id)
            }
            else {
                render(view:'edit',model:[user:user])
            }
        }
        else {
            flash.message = "User not found with id ${params.id}"
            redirect(action:edit,id:params.id)
        }
    }

    def create = {
        def user = new User()
        user.properties = params
        return ['user':user]
    }

    def save = {
        def user = new User()
        user.properties = params
        if(user.save()) {
	    flash.message = "${user.email} saved."
            session.email = user.email  // login for them
	    def userContact = Contact.findByEmail(user.email)
	    if (userContact) {
	        user.myContact = userContact
		user.save()
                flash['message'] = 'I already have contact information. Please verify.'
                redirect(controller:'contact',action:'show',id:userContact.id)
	    }
	    else {
                flash['message'] = 'Please provide your contact information'
                redirect(controller:'contact',action:'create',params:[user:user.id])
	    }
        }
        else {
            render(view:'create',model:[user:user])
        }
    }

    def login = { 
        log.debug("login")
        log.debug("originalRequestParams ${session.originalRequestParams}")
        log.debug("params "+params.toString())
        log.debug("request\n"+request.toString())
        log.debug(session.toString())

        if (request.method == "GET") { 
            session.email = null 
            def user = new User() 
        } 
        else { 
            def user = User.findByEmailAndPassword(params.email, params.password) 
            if (user) { 
                session.email = user.email 
		if (!user.myContact) {
                    flash['message'] = 'Please provide your contact information' 
                    redirect(controller:'contact',action:'create',params:[user:user.id])
		}
		def redirectParams = session.originalRequestParams ?  
		    session.originalRequestParams :
		    [controller:'contact', action:'letsgo', id:user.myContact?.id] 
                session.originalRequestParams = null
            	redirect(redirectParams) 
            } 
            else { 
               flash['message'] = 'Please enter a valid email and password' 
            } 
        } 
    } 

    def logout = {
        session.email = null
	session.originalRequestParams = null
        flash['message'] = 'Successfully logged out'
        redirect(controller:'user', action:'login')
    }


}
