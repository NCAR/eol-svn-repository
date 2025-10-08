class ContactController extends BaseController {
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

    def letsgo = {
        log.debug("contact letsgo")
        log.debug("params "+params)
        log.debug("request\n"+request.toString())
        log.debug(session.toString())

        def me = null
        if (params.id) {
	  me = Contact.get( params.id )
	}

	if (!me) {
	  me = User.findByEmail(session.email)?.myContact
	}
	if (me) {
	  log.info("contact:letsgo found me "+me.id+" "+me.email)
	  def criteria = Dataset.createCriteria()
	  def results = criteria {
	     or {
	        eq('piContact',me)
	        eq('metadataContact',me)
	        eq('datacenterContact',me)
	  	 }
	  }
     return [ contact : me, datasets : results ]
	}
	else {
            flash.message = "No contact information found for you! Please create it."
            redirect(action:create,params:[email:session.email])
	}
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

	def user = null
	if (params.user) {
	  user = User.get(params.user)
	  if (user) {
	      contact.email = user.email
	  }
	}

        return ['contact':contact]
    }

    def save = {
        def contact = new Contact()
        contact.properties = params

        if(!contact.save()) {
            render(view:'create',model:['contact':contact])
	    return
        }

	def contactUser = User.findByEmail(contact.email)
	if (contactUser) {
	  if (contactUser.myContact) {
	    if (contactUser.myContact.id != contact.id) {
	      flash.message = "Ouch- I found an internal data integrity error for this contact/user"
              redirect(action:show,id:contact.id)
	      return
	    }
	  }
	  else {
	    contactUser.myContact = contact
	    contactUser.save()
	  }
	}

        flash.message = "Contact ${params.personName} saved."
        redirect(action:show,id:contact.id)
    }

}
