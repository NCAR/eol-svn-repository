class AuthController { // simulate the CDP authentication for standalone testing

    def index = { redirect(action:login) }

    def login = {
        log.debug("login")
        log.debug("params "+params.toString())
        //log.debug("request\n"+request.toString())
        log.debug(session.toString())

        if (request.method == "GET") {
            cleanup()
        }
        else {
            def contact = null
	    log.debug("params.username = ${params.username}")
	    if (params.username) contact = Contact.findByShortName(params.username)
            if (contact) {
	        log.debug("found contact ${contact.id} ${contact}")
		setup(contact)
	        log.debug("still have contact ${contact.id} ${contact}")
            	redirect( controller:'contact', action:'letsgo', id:contact?.id )
		// params:[sid:session.id]
            }
            else {
               flash['message'] = 'Could not find that username.'
            }
        }
    }

    def cleanup() {
      // clean up leftovers
      session.removeAttribute('cdp.hash')
      session.removeAttribute('cdp.key')
      session.removeAttribute('cdp.name')
      session.removeAttribute('username')
      }

    def setup(contact) { // setup servlet context like CDP would
      log.debug("setup contact ${contact}")
      if (!contact) return // error XXX

      cleanup()

      def hh = new Hashtable<String,String>()
      log.debug("hh = ${hh}")
      if (hh==null) return // error XXX

      log.debug("contact shortName = ${contact.shortName} email = ${contact.email}")
      hh.username = contact.shortName
      hh.email = contact.email
      //hh.datasetId = null

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
