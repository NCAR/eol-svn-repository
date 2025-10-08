class ContactController {

    // the delete, save and update actions only
    // accept POST requests
    def allowedMethods = [delete:'POST',
                          save:'POST',
                          update:'POST']


        // double-check authorization in case filter gets a null actionName
        //  (e.g. in the case of forms submitted to the dispatcher)
    def beforeInterceptor = [action:this.&auth, only:['delete','edit','update','show']]

    def auth() {
        if (session.username && session.isAdmin) return true
        if ('delete'.equals(actionName)) {
            flash.message = 'Not authorized'
            if (params.id)
                redirect(action:show,id:params.id)
            else redirect(uri:'/')
            return false
        }
        return true // (edit/update handled internally)
    }


    def index = { redirect(action:list,params:params) }



    def list = {
        if(!params.max)params.max = 10
        [ contactList: Contact.list( params ) ]
    }



    def show = { // view: show
        def contact = Contact.get( params.id )

        if (!contact) {
            flash.message = 'Contact not found'
            redirect(action:list)
            return
        }

          // ugh, security code in the middle of our logic
        if ( !session.isAdmin && (contact.shortName != session.username) ) {
            flash.message = 'Not authorized to view contact.'
            redirect(uri:'/')
            return
        }

        return [ contact : contact ]
    }



    def myinfo = {
        def contact = Contact.findByShortName(session.username)
        if(!contact) {
            flash.message = "Contact not found"
            redirect(action:list)
        }
        else {
            render(view:'show',model:[ contact : contact ])
        }
    }



    def delete = {
        def contact = Contact.get( params.id )
        if(contact) {
            contact.delete()
            flash.message = "Contact ${contact.personName} deleted."
            redirect(action:list)
        }
        else {
            flash.message = 'Contact not found'
            redirect(action:list)
        }
    }



    def edit = {
        def contact = Contact.get( params.id )

        if(!contact) {
            flash.message = 'Contact not found'
            redirect(action:list)
            return
        }

          // ugh, security code in the middle of our logic
        if ( !session.isAdmin && (contact.shortName != session.username) ) {
            flash.message = 'Not authorized to edit contact.'
            redirect(uri:'/')
            return
        }

        return [ contact : contact ]
    }



    def update = {
        def contact = Contact.get( params.id )

        if (!contact) {
            flash.message = 'Contact not found'
            redirect(action:edit,id:params.id)
            return
        }

          // ugh, security code in the middle of our logic
        if ( !session.isAdmin && (contact.shortName != session.username) ) {
            flash.message = 'Not authorized to edit contact.'
            redirect(uri:'/')
            return
        }

        if (params.isAdmin) params.remove('isAdmin') // malicious escalation
        contact.properties = params
        if(contact.save()) {
            flash.message = "Contact updated."
            redirect(action:show,id:contact.id)
        }
        else {
            render(view:'edit',model:[contact:contact])
        }

    }



    def create = {
        def contact = new Contact()
        contact.properties = params
        if ('me' == params.forwhom) {
            log.debug("create: params forwhom ${params.forwhom}")
            contact.shortName = session.username
            contact.email = session.email
            contact.personName = session['cdp.hash'].fullName
            contact.organizationName = session['cdp.hash'].userInstitution
            session.forwhom = 'me'
            }
        [contact:contact]
    }



    def save = {
        def contact = new Contact()
        if (params.isAdmin) params.remove('isAdmin') // malicious escalation
        contact.properties = params

        if ('me' == session.forwhom) {
            log.debug("save: session forwhom ${session.forwhom}")
            contact.shortName = session.username
            contact.email = session.email
            session.removeAttribute('forwhom')
            log.debug("after removeAttribute ${session.forwhom}")
            }

        if(!contact.save()) {
            render(view:'create',model:['contact':contact])
            return
            }

        flash.message = "Contact saved."
        redirect(action:show,id:contact.id)
    }



    def cancel = {  // here so we get a nice button in edit view
         redirect(action:myinfo)
    }

}
