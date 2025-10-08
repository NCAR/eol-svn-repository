class ContactController {
    def index = { redirect(action:list,params:params) }

    // the delete, save and update actions only
    // accept POST requests
    def allowedMethods = [delete:'POST',
                          save:'POST',
                          update:'POST']

    def list = {
        if(!params.max)params.max = 10
        [ contactList: Contact.list( params ) ]
    }

    def show = {
        [ contact : Contact.get( params.id ) ]
    }

    def delete = {
        def contact = Contact.get( params.id )
        if(contact) {
            contact.delete()
            flash.message = "Contact ${params.personName} deleted."
            redirect(action:list)
        }
        else {
            flash.message = "Contact not found with id ${params.id}"
            redirect(action:list)
        }
    }

    def edit = {
        def contact = Contact.get( params.id )

        if(!contact) {
                flash.message = "Contact not found with id ${params.id}"
                redirect(action:list)
        }
        else {
            return [ contact : contact ]
        }
    }

    def update = {
        def contact = Contact.get( params.id )
        if(contact) {
             contact.properties = params
            if(contact.save()) {
				flash.message = "Contact ${params.personName} updated."
                redirect(action:show,id:contact.id)
            }
            else {
                render(view:'edit',model:[contact:contact])
            }
        }
        else {
            flash.message = "Contact not found with id ${params.id}"
            redirect(action:edit,id:params.id)
        }
    }

    def create = {
        def contact = new Contact()
        contact.properties = params
	if ("me" == params.forwhom) {
	  contact.shortName = session.username
          contact.email = session['cdp.hash'].email
	  }
	session.forwhom = "me"
        ['contact':contact]
    }

    def save = {
        def contact = new Contact()
        contact.properties = params

	if ("me" == session.forwhom) {
	  contact.shortName = session.username
	  contact.email = session.email
	  session.removeAttribute("forwhom")
	  }

        if(!contact.save()) {
            render(view:'create',model:['contact':contact])
	    return
        }

        flash.message = "Contact ${params.personName} saved."
        redirect(action:show,id:contact.id)
    }

}
