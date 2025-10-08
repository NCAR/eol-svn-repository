class MyFilters {
  def filters = {

     auth(controller:'*', action:'*') {
       before = {
         if (session.username) return true
	 if (controllerName.equals('cdp')) return true
	 if (controllerName.equals('auth')) return true

         // save the original request info
	 session.originalActionName = actionName
	 session.originalControllerName = controllerName
	 session.originalParams = params
	 //session.originalURL = request.requestURL
	 //session.originalURI = request.requestURI

	 // try to find auth info from the CDP
         redirect(controller:'cdp', action:'auth', params:params)

	 // don't complete original request
         return false
         }
       }

  }
}
