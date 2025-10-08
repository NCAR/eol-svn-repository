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
   //if hh is null then we don't know what context the cdp is in
   //redirect(url:"${defctxpfx}security/loginout.htm?redirect=/cadisEditor/cdp/enter")
   redirect(controller:'auth',action:'login')
   return
   }

  // XXX redirect to the original request controller/action
  //chain(controller:'work',action:'doit',model:[params:params,username:hh['username'],cdpid:hh['datasetId'],cdptext:null])
  //redirect(controller:'work',action:'doit')
  redirect(controller:'contact',action:'letsgo')
  return
  }



 def publish = {
  log.debug("exit")
  log.debug("params = "+params)

  def d = params.id ? Dataset.get( params.id ) : null
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

  hh['datasetId'] = d?.id
  hh['datasetName'] = d?.title
  hh['metadata.xml'] = xml

  /*
  def ctxpfx = (ctxname && (ctxname != "ROOT")) ? ctxname : defctxpfx
  if (ctxpfx[-1] != '/') ctxpfx+='/'
  redirect(url:"${ctxpfx}cadisEditorStoreThredds.do?sid="+ssid)
  */
  redirect(controller:'auth',action:'show',params:[sid:ssid])

  return
  }

}
