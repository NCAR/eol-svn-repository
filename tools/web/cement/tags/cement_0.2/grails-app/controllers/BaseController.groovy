abstract class BaseController {
    def beforeInterceptor = [action:this.&auth,except:'login']

    def auth() {
        if(!session.email) {
            def originalRequestParams = [controller:controllerName, action:actionName]
            originalRequestParams.putAll(params)
            session.originalRequestParams = originalRequestParams

            redirect(controller:'user',action:'login')
            return false
        }
    }

}
