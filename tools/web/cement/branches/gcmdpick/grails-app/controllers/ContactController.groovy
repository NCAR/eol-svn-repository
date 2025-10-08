class ContactController {

    // the delete, save and update actions only
    // accept POST requests
    def allowedMethods = [delete:'POST',
                          save:'POST',
                          update:'POST']



    def index = { redirect(action:list,params:params) }



    def list = {
        if(!params.max)params.max = 10
        [ contactList: Contact.list( params ) ]
    }



    def show = {
        [ contact : Contact.get( params.id ) ]
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
        if ('me' == params.forwhom) {
            log.debug("create: params forwhom ${params.forwhom}")
            contact.shortName = session.username
            contact.email = session.email
            session.forwhom = 'me'
            }
        [contact:contact]
    }



    def save = {
        def contact = new Contact()
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

        flash.message = "Contact ${params.personName} saved. (Use back button to return to metadata editing if you just added a new contact.)"
        redirect(action:show,id:contact.id)
    }



    def cancel = {  // here so we get a nice button in edit view
         redirect(action:myinfo)
    }

}
