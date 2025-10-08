class WorkController {

 def index = { redirect(action:doit,params:params) }

 def doit = {
  def cdpid = null
  def cdptext = null
  if (session['cdp.data']) {
    cdpid = session['cdp.data'].username
    }
  [cdpid:cdpid,cdptext:cdptext]
  }

 def done = {
  log.debug("done")
  log.debug("params = "+params)
  if ( (params.cdpid == null) || (params.cdptext == null) ||
       (params.cdpid == '') || (params.cdptext == '') ) {
   flash.message = 'Please fill out both fields.'
   render(view:doit,model:[cdpid:params.cdpid,cdptext:params.cdptext])
   }
  redirect(controller:'cdp',action:'exit',params:params)
  }

}
