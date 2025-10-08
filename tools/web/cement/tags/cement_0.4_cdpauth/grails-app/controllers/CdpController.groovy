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
  ssids += session.id

  if (params.sid) {
    ssids += params.sid
    log.debug("params.sid = ${params.sid}")
    }

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
    log.debug("found in ${ctxname} obj ${ht} isa ${ht.class}")
    ssids.each { ssid ->
     log.debug("looking in hash for ${ssid}")
     if (!found) {
      hh = (Hashtable<String,String>)ht[ssid]
      if (hh) {
       log.debug("found hh=${hh} isa ${hh.class}")

       session['cdp.hash'] = hh
       session['cdp.key'] = ssid
       session['cdp.name'] = ctxname
       session['username'] = hh['username']

       log.debug("ssid isa "+ssid.getClass())
       log.debug("ctxname isa "+ctxname.getClass())
       log.debug("cdp.key isa "+session['cdp.key'].getClass())
       log.debug("cdp.name isa "+session['cdp.name'].getClass())

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

  // XXX redirect to the original request controller/action
  //chain(controller:'work',action:'doit',model:[params:params,username:hh['username'],cdpid:hh['datasetId'],cdptext:null])
  redirect(controller:'dataset',action:'review',id:me.id)
  return
  }



 def publish = {
  log.debug("exit")
  log.debug("params = "+params)

  def d = null
  if (params.id) {
    if (params.id =~ /^\d+$/)
      d = Dataset.get( params.id )
    else d = Dataset.findByEntryID(params.id)
    }

  def xml = d ? g.render(template:"/dataset/metadata", bean:d) : ""

  def ctxname = session['cdp.name']
  def ssid = session['cdp.key']
  def hh = session['cdp.hash']
  log.debug("session hash = ${hh}")

  if (hh == null)
    hh = new Hashtable<String,String>()

  if (hh == null) { // still !?!?
    flash.message = 'Cannot find or build a hashtable'
    redirect(controller:'dataset',action:'show',id:params.id)
    return
    }

  hh['datasetId'] = d?.entryID
  hh['datasetName'] = d?.title
  hh['metadata.xml'] = xml

  if ("development" == System.getProperty("grails.env"))
    redirect(controller:'auth',action:'show',params:[sid:ssid])
  else {
    def ctxpfx = (ctxname && (ctxname != "ROOT")) ? ctxname : defctxpfx
    if (ctxpfx[-1] != '/') ctxpfx+='/'
    redirect(url:"${ctxpfx}cadisEditorStoreThredds.do?sid="+ssid)
    }
  return
  }

}
