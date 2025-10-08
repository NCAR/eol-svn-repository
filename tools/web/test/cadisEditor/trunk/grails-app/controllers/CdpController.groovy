class CdpController {

 def index = { redirect(action:enter,params:params) }
 def entry = { redirect(action:enter,params:params) }

 private static wantcookies = ["JSESSIONIDSSO","JSESSIONID"]
 private static fromcontexts = ["","ROOT","/","/cdp","/portal"]
 //private static defctxpfx = "/cdp/"
 private static defctxpfx = "/"

 def enter = {
  log.debug("enter")
  log.debug("wantcookies="+wantcookies)
  log.debug("fromcontexts="+fromcontexts)

  def ssids = []
  ssids += session.id

  if (params.sid) ssids += params.sid
  if (params.sid2) ssids += params.sid2

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
   if (ht) log.debug("found in ${ctxname} obj ${ht}")
   //if (ht) log.debug("found in ${ctxname} obj ${ht} isa ${ht.class} loaded by ${ht.class.classLoader}")
   //if ((ht != null) && (ht.class.toString() =~ /Hash/)) {
   if (ht) {
   //if (ht.class.toString() =~ /Hash/) {
    ssids.each { ssid ->
     log.debug("looking in hash for ${ssid}")
     if (!found) {
      hh = (Hashtable<String,String>)ht[ssid]
      if (hh) {
       //log.debug("found hh = ${hh}")
       log.debug("found hh="+hh+" isa "+hh.class)
       //log.debug("found hh="+hh+" isa "+hh.class+" loaded by "+hh.class.classLoader)

       //GroovyShell shell = new GroovyShell()
       //session['cdp.hash2'] = shell.evaluate("${hh}")

       session['cdp.hash'] = hh
       session['cdp.key'] = ssid
       session['cdp.name'] = ctxname

       log.debug("ssid isa "+ssid.getClass())
       log.debug("ctxname isa "+ctxname.getClass())
       log.debug("cdp.key isa "+session['cdp.key'].getClass())
       log.debug("cdp.name isa "+session['cdp.name'].getClass())

       found=true
       }
      }
     }
    //}
    }
   }

  if (hh == null) {
   //if hh is null then we don't know what context the cdp is in
   //def ctxpfx = session['cdp.name'] ? session['cdp.name'] : defaultctxname
   //if (ctxpfx[-1] != '/') ctxpfx+='/'
   redirect(url:"${defctxpfx}security/loginout.htm?redirect=/cadisEditor/cdp/enter")
   return
   }

  //chain(controller:'work',action:'doit',model:[params:params,username:hh['username'],cdpid:hh['datasetId'],cdptext:null])
  redirect(controller:'work',action:'doit')
  return
  }

 def exit = {
  log.debug("exit")
  log.debug("params = "+params)

  def xml = """
  <metadata xsi:schemaLocation="http://www.unidata.ucar.edu/namespaces/thredds/InvCatalog/v1.0 http://www.unidata.ucar.edu/schemas/thredds/InvCatalog.1.0.xsd" metadataType="THREDDS" inherited="true">
  <documentation type="summary">
  ${params.cdptext}
  </documentation>
  </metadata>
  """.toString()

  def ctxname = session['cdp.name']
  def ssid = session['cdp.key']
  def hh = session['cdp.hash']
  log.debug("session hash = ${hh}")

  if (hh == null)
    hh = new Hashtable<String,String>()

  if (hh == null) { // still !?!?
    flash.message = 'Cannot find or build a hashtable'
    redirect(controller:'work',action:'doit')
    return
    }

  hh['datasetId'] = params.cdpid
  hh['metadata.xml'] = xml

  log.debug("cdpid isa "+params.cdpid.getClass())
  log.debug("xml isa "+xml.getClass())
  log.debug("datasetId isa "+hh['datasetId'].getClass())
  log.debug("metadata.xml isa "+hh['metadata.xml'].getClass())

  if (params.redir == "echo") {
    log.debug("echo")
    ctxname = servletContext?.getContextPath()
    log.debug("ctxname = ${ctxname}")
    ssid = 'echokey'

    def attr = "session.cadisEditor"
    def ht = servletContext?.getContext(ctxname)?.getAttribute(attr)
    if (ht == null)
      ht = new Hashtable<String,Hashtable>()

    ht.remove(ssid)
    ht[ssid] = hh

    servletContext?.getContext(ctxname)?.setAttribute(attr,ht)
    redirect(action:echo)
    return
    }

  def ctxpfx = (ctxname && (ctxname != "ROOT")) ? ctxname : defctxpfx
  if (ctxpfx[-1] != '/') ctxpfx+='/'
  redirect(url:"${ctxpfx}cadisEditorStoreThredds?sid="+ssid)
  return
  }

  def echo = {}

}
