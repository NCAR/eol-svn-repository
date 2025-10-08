
import cadis.*

class WorkController {

 def index = { redirect(action:doit,params:params) }

 def doit = {
  log.debug("doit")
  def username = null
  def cdpid = null

  def hh = session['cdp.hash']
  if (hh) log.debug("doit found hh = ${hh}")

  //def h2 = session['cdp.hash2']
  //if (h2) log.debug("doit found h2 = ${h2}")

  if (hh) {
   log.debug("looking at hh = ${hh}")
   username = hh['username']
   cdpid = hh['datasetId']
   log.debug("returning username ${username} cdpid ${cdpid}")
   return [username:username,cdpid:cdpid,cdptext:null]
   }
 /*
  else if (h2) {
   log.debug("looking at h2 = ${h2}")
   username = h2['username']
   cdpid = h2['datasetId']
   log.debug("returning username ${username} cdpid ${cdpid}")
   return [username:username,cdpid:cdpid,cdptext:null]
   }
  */

  []
  }

 def done = {
  log.debug("done")
  log.debug("params = "+params)
  if ( (params.cdpid == null) || (params.cdptext == null) ||
       (params.cdpid == '') || (params.cdptext == '') ) {
   flash.message = 'Please fill out both fields.'
   render(view:doit,model:[cdpid:params.cdpid,cdptext:params.cdptext])
   return
   }
  redirect(controller:'cdp',action:'exit',params:params)
  }

}
