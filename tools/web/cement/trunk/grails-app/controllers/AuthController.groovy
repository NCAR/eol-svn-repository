import grails.util.GrailsUtil

class AuthController { // simulate the CDP authentication for standalone testing

    // double-check authentication in case filter gets a null actionName
    def beforeInterceptor = [action:this.&auth, only:'show']

    def auth() {
        if (!session.username || !GrailsUtil.isDevelopmentEnv()) return false
    }

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
            log.debug("params username = ${params.username} fullName = ${params.fullName} email = ${params.email} userInstitution = ${params.userInstitution} datasetId = ${params.datasetId} cadisPrivilegesStr = ${params.cadisPrivilegesStr}")
            if (params.username) contact = Contact.findByShortName(params.username)
            if (!contact &&
                params.username && params.username != "" &&
                params.email && params.email != ""  &&
				params.fullName && params.fullName != "" &&
				params.userInstitution && params.userInstitution != "") {
                // make a fake contact for setup() - we just need username/email to mock the CDP
                contact = new Contact()
                contact.shortName = params.username
				contact.personName = params.fullName
                contact.email = params.email
				contact.organizationName = params.userInstitution
            }
            if (contact) {
                log.debug("found contact ${contact.id} ${contact}")
                setup(contact,params.datasetId,params.cadisPrivilegesStr)
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
        contact.personName = params.fullName
        contact.email = params.email
		contact.organizationName = params.userInstitution
        log.debug("returning login error with ${contact.shortName} ${contact.personName} ${contact.email} ${contact.organizationName}")
        return ['contact':contact,'datasetId':params.datasetId,'cadisPrivilegesStr':params.cadisPrivilegesStr]
        }
    }

    def cleanup() {
      // clean up leftovers
      session.removeAttribute('cdp.hash')
      session.removeAttribute('cdp.key')
      session.removeAttribute('cdp.name')
      session.removeAttribute('username')
      session.removeAttribute('isAdmin')
	  session.removeAttribute('cadisPrivileges')
      }

    def setup(contact,datasetId,cadisPrivilegesStr) { // setup servlet context like CDP would
      log.debug("setup contact ${contact} datasetId ${datasetId} cadisPrivilegesStr ${cadisPrivilegesStr}")
      if (!contact) return // do something about this error?

      cleanup()

      def hh = new Hashtable<String,String>()
      log.debug("hh = ${hh}")
      if (hh==null) return // do something about this error?

      log.debug("contact shortName = ${contact.shortName} email = ${contact.email}")
      hh.username = contact.shortName
      hh.fullName = contact.personName
      hh.email = contact.email
	  hh.userInstitution = contact.organizationName
      if (datasetId) hh.datasetId = datasetId

      def al = new ArrayList()
      if (al != null) {
		if (cadisPrivilegesStr) {
			def strTok = new StringTokenizer(cadisPrivilegesStr, ',')
			strTok.each {
  			  log.debug("token ${it}")
			  al.add(it)
			  }
		}
        hh.cadisPrivileges = al
      }
      log.debug("hh.cPriv = ${hh.cadisPrivileges}")

      def ctxname = servletContext?.getContextPath()
      def attr = "session.cadisEditor"
      def ht = servletContext?.getContext(ctxname)?.getAttribute(attr)
      if (ht == null) ht = new Hashtable<String,Hashtable>()
      if (ht == null) return // do something about this error?

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
