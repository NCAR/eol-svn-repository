class CdpController {
  def index = { redirect(action:auth,params:params) }

 private static wantcookies = ["JSESSIONIDSSO","JSESSIONID"]
 private static fromcontexts = ["","ROOT","/","/cdp","/portal","/cadisEditor","/cement"]
 //private static defctxpfx = "/cdp/"
 private static defctxpfx = "/"

 def auth = { // try to find auth info from CDP

  log.debug("auth")
  log.debug("requestURL ${request.requestURL}")
  log.debug("queryString ${request.queryString}")
  log.debug("params = ${params}")
  log.debug("wantcookies="+wantcookies)
  log.debug("fromcontexts="+fromcontexts)
  log.debug("session = ${session}")

  def ssids = []

  if (params.sid) {
    ssids += params.sid
    log.debug("params.sid = ${params.sid}")
    }
  ssids += session.id

  request.cookies.each {
   if ((it.name in wantcookies) && !(it.value in ssids)) ssids += it.value
   }

  log.debug("ssids="+ssids)

  def found = false
  def attr = "session.cadisEditor"
  def ht = null
  def hh = null

  fromcontexts.each { ctxname ->
   log.debug("trying ${ctxname} ${attr}")
   ht = servletContext?.getContext(ctxname)?.getAttribute(attr)
   if (ht) {
    log.debug("found in ${ctxname} obj ${ht} isa ${ht.getClass()}")
    ssids.each { ssid ->
     log.debug("looping on ssid ${ssid} with found = ${found}")
     if (!found) {
      hh = (Hashtable<String,String>)ht[ssid]
      if (hh) {
       log.debug("found hh=${hh} isa ${hh.getClass()}")

       session['cdp.hash'] = hh
       session['cdp.key'] = ssid
       session['cdp.name'] = ctxname
       session['username'] = hh['username']
       session['email'] = hh['email']

       log.debug("ssid ${ssid} isa "+ssid.getClass())
       log.debug("ctxname ${ctxname} isa "+ctxname.getClass())
       //log.debug("cdp.key isa "+session['cdp.key'].getClass())
       //log.debug("cdp.name isa "+session['cdp.name'].getClass())

       found=true
       }
      }
     }
    }
   }

  if (hh == null) {
   if ("development" == System.getProperty("grails.env"))
     redirect(controller:'auth',action:'login')
   else
     redirect(url:"${defctxpfx}security/loginout.htm?redirect=/cadisEditor/")
   return
   }

  // we found CDP's info, let's make sure we have the username lined up to a Contact
        def me = Contact.findByShortName(session.username)
	if (!me) {
	  if (session['cdp.hash']?.email)
	    me = Contact.findByEmail(session['cdp.hash'].email)
	  if (me) {
	    me.shortName = session.username
	    me.save()
	    flash['message'] = 'I already have contact information. Please verify.'
	    redirect(controller:'contact',action:'show',id:me.id)
	    return
	    }
	  else {
            flash.message = "No contact information found for you! Please create it."
            redirect(controller:'contact',action:'create',params:[forwhom:'me'])
	    return
	    }
	  }

  if (session.originalControllerName && session.originalActionName) {
    redirect(controller:session.originalControllerName,action:session.originalActionName,params:session.originalParams)
    session.removeAttribute('originalControllerName')
    session.removeAttribute('originalActionName')
    session.removeAttribute('originalParams')
    }
  else redirect(controller:'dataset',action:'review',id:me.id)
  return
  }

 def publish = {
  log.debug("publish")
  log.debug("params = "+params)

  def ctxname = session['cdp.name']
   log.debug("context name = ${ctxname}")
  def ssid = session['cdp.key']
   log.debug("ssid = ${ssid}")
  def hh = session['cdp.hash']
   log.debug("session hash = ${hh}")

  def d = null
  if (params.id) {
    if (params.id =~ /^\d+$/)
      d = Dataset.get( params.id )
    else d = Dataset.findByEntryID(params.id)
    }
  log.debug("dataset by params.id = ${d}")

  if (!d) {
    d = Dataset.findByEntryID(hh.datasetId)
    log.debug("dataset by hh.id = ${d}")
    }

  def xml = d ? g.render(template:"/dataset/metadata", bean:d) : ""
  //log.debug("xml = ${xml}")

  if (hh == null)
    hh = new Hashtable<String,String>()
  log.debug("hh (inner hash) before updates = ${hh}")

  if (hh == null) { // still !?!?
    flash.message = 'Cannot find or build a hashtable'
    log.debug('Cannot find or build a hashtable')
    redirect(controller:'dataset',action:'show',id:params.id)
    return
    }

  hh['datasetId'] = d?.entryID
  hh['datasetName'] = d?.title
  hh['metadata.xml'] = xml
  log.debug("hh (inner hash) after updates = ${hh}")

  if ("development" == System.getProperty("grails.env"))
    redirect(controller:'auth',action:'show',params:[sid:ssid])
  else {
    def ctxpfx = (ctxname && (ctxname != "ROOT")) ? ctxname : defctxpfx
    if (ctxpfx[-1] != '/') ctxpfx+='/'
    log.debug("redir to ${ctxpfx}cadisEditorStoreThredds.do?sid=${ssid}")
    redirect(url:"${ctxpfx}cadisEditorStoreThredds.do?sid=${ssid}")
    }
  return
  }

 def exit = {
  if ("development" == System.getProperty("grails.env"))
    redirect(controller:'auth',action:'login',params:[sid:ssid])
  else {
    def ctxname = session['cdp.name']
    def ctxpfx = (ctxname && (ctxname != "ROOT")) ? ctxname : defctxpfx
    if (ctxpfx[-1] != '/') ctxpfx+='/'
    redirect(url:"${ctxpfx}browse/browse.htm?uri=http://dataportal.ucar.edu/metadata/cadis/cadis.thredds.xml")
    }
  }

}
