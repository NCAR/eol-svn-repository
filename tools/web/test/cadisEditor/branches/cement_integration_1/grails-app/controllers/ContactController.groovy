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
	log.info("params "+params)
	log.info("params.id "+params.id)

        def me = null
	if (params.id) {
	  me = Contact.get( params.id )
	  }

	if ((!me) && (session['cdp.hash']?.username)) {
	  me = Contact.findByShortName(session['cdp.hash']?.username)
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
            redirect(action:create,params:[ shortName:session['cdp.hash']?.username, email:session['cdp.hash']?.email ])
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
        log.info('update params = '+params)
        log.info('update params[homePage] = '+params['homePage'])
        log.info(' length = '+params['homePage'].length())
        log.info(' trim.length = '+params['homePage'].trim().length())
	/* needed in 0.6/1.0-RC1-SNAPSHOT but maybe not in 1.0-RC1
        if (params['homePage']) {
	  if (!params['homePage'].trim().length()) {
	    //params.remove('homePage')
	    params['homePage']=null
	  }
	}
	*/
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
        return ['contact':contact]
    }

    def save = {
        def contact = new Contact()
        contact.properties = params

	if (session['cdp.hash']?.username != params.shortName) {
	    flash.message = "I can't find or you changed the shortName/username!"
	    contact.shortName = session['cdp.hash']?.username
            render(view:'create',model:['contact':contact])
	    return
	}

        if(!contact.save()) {
            render(view:'create',model:['contact':contact])
	    return
        }

        flash.message = "Contact ${params.personName} saved."
        redirect(action:show,id:contact.id)
    }

}
