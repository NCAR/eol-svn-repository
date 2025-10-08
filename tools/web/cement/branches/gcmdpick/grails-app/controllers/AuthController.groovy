class AuthController { // simulate the CDP authentication for standalone testing

    def index = { redirect(action:login) }

    def login = {
        log.debug("login")
        log.debug("params ${params}")
        //log.debug("request\n${request}")
        log.debug(session.toString())

        if (request.method == "GET") {
            cleanup()
        }
        else {
            def contact = null
            log.debug("params username = ${params.username} email = ${params.email} entryid = ${params.entryid}")
            if (params.username) contact = Contact.findByShortName(params.username)
            if (!contact &&
                params.username && params.username != "" &&
                params.email && params.email != "") {
                // make a fake contact for setup() - we just need username/email to mock the CDP
                contact = new Contact()
                contact.shortName = params.username
                contact.email = params.email
            }
            if (contact) {
                log.debug("found contact ${contact.id} ${contact}")
                setup(contact,params.entryid)
                log.debug("still have contact ${contact.id} ${contact}")
                if (session.originalControllerName && session.originalActionName) {
                    // redirect to original request
                    redirect(controller:session.originalControllerName,
                             action:session.originalActionName,
                             params:session.originalParams)
                    session.removeAttribute('originalControllerName')
                    session.removeAttribute('originalActionName')
                    session.removeAttribute('originalParams')
                }
                else redirect( controller:'dataset', action:'review', id:contact?.id ) // params:[sid:session.id]
                return
            }
        flash['message'] = 'Could not find your contact info. Please provide a CDP username and email.'
        contact = new Contact()
        contact.shortName = params.username
        contact.email = params.email
        log.debug("returning login error with ${contact.shortName} ${contact.email}")
        return ['contact':contact,'entryid':params.entryid]
        }
    }

    def cleanup() {
      // clean up leftovers
      session.removeAttribute('cdp.hash')
      session.removeAttribute('cdp.key')
      session.removeAttribute('cdp.name')
      session.removeAttribute('username')
      }

    def setup(contact,entryid) { // setup servlet context like CDP would
      log.debug("setup contact ${contact}")
      if (!contact) return // error XXX

      cleanup()

      def hh = new Hashtable<String,String>()
      log.debug("hh = ${hh}")
      if (hh==null) return // error XXX

      log.debug("contact shortName = ${contact.shortName} email = ${contact.email}")
      hh.username = contact.shortName
      hh.email = contact.email
      if (entryid) hh.datasetId = entryid

      def ctxname = servletContext?.getContextPath()
      def attr = "session.cadisEditor"
      def ht = servletContext?.getContext(ctxname)?.getAttribute(attr)
      if (ht == null) ht = new Hashtable<String,Hashtable>()
      if (ht == null) return // error XXX

      log.debug("session.id = ${session.id}")
      ht.remove(session.id)
      ht[session.id] = hh
      servletContext?.getContext(ctxname)?.setAttribute(attr,ht)

      return
      }

    def logout = {
        cleanup()
        flash['message'] = 'Successfully logged out'
        redirect(controller:'auth', action:'login')
    }

    def show = {
      def ctxname = servletContext?.getContextPath()
      def attr = "session.cadisEditor"
      def ht = servletContext?.getContext(ctxname)?.getAttribute(attr)
      [bigstring:"params = ${params}\n\nsession = ${session}\n\nht = ${ht}\n"]
    }

}
