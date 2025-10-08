class MyFilters {
  def filters = {

    authentication(controller:'*', action:'*') {
      before = {
          // already authenticated
        if (session.username) return true

          // the stuff that does the authentication is always okay
          //   but publish and show are special
        if ('cdp'.equals(controllerName) && !'publish'.equals(actionName)) return true
        if ('auth'.equals(controllerName) && !'show'.equals(actionName)) return true

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

    authorization(controller:'*', action:'*') {
      before = {
          // superuser
        if (session.isAdmin) return true

          // these are always okay for authenticated users
        if ('auth'.equals(controllerName)) return true
        if (['index','list','show'].contains(actionName)) return true

          // these are handled by fine-grained code inside the closures
        if ('cdp'.equals(controllerName)) return true
        if ('dataset'.equals(controllerName) && !'delete'.equals(actionName) )
            return true
        if ('contact'.equals(controllerName) && !'delete'.equals(actionName) )
            return true

          // don't complete original request
        flash.message = 'Not authorized'
        redirect(uri:'/')
        return false
        }
      }

    }
}
