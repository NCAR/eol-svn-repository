class CdpController {

 def index = { redirect(action:entry,params:params) }

 private static wantcookies = ["JSESSIONIDSSO","JSESSIONID"]
 private static fromcontexts = ["/","/cdp","/portal"]
 private static defctxpfx = "/cdp/"

 def entry = {
  log.debug("entry")

  def ssids = []
  ssids += session.id

  if (params.sid) ssids += params.sid
  if (params.sid2) ssids += params.sid2

  request.cookies.each {
   if ((it.name in wantcookies) && !(it.value in ssids)) ssids += it.value
   }

  log.debug("ssids="+ssids)

  def sd = null
  def found = false
  def attr = null
  fromcontexts.each { ctxname ->
   ssids.each { ssid ->
    if (!found) {
     attr = "session.data_"+ssid
     log.debug("trying ${ctxname} ${ssid} ${attr}")
     sd = servletContext?.getContext(ctxname)?.getAttribute(attr)
     if ((sd != null) && (sd.class.toString() == "class ncar.scd.security.SessionData")) {
     //if ((sd != null) && ("class ncar.scd.security.SessionData".equals(sd.class.toString()))) {
     //if (sd != null) {
      log.debug("found sd="+sd+" isa "+sd.class+" loaded by "+sd.class.classLoader)
      if (sd.sessionId != ssid)
        log.warn("SessionData.sessionId=${sd.sessionId} but attribute named with ssid=${ssid}")
      session['cdp.data'] = sd
      session['cdp.key'] = ssid
      session['cdp.name'] = ctxname
      found=true
      }
     }
    }
   }

  //if (sd == null) redirect(uri:"/error.html")
  if (sd == null) {
   //if sd is null then we don't know what context the cdp is in
   //def ctxpfx = session['cdp.name'] ? session['cdp.name'] : defaultctxname
   //if (ctxpfx[-1] != '/') ctxpfx+='/'
   redirect(url:"${defctxpfx}security/loginout.htm?redirect=/cadisEditor/cdp/entry")
   }

  //chain(controller:'work',action:'doit',model:[params:params,cdpid:null,cdptext:null])
  redirect(controller:'work',action:'doit')
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
  """

  def ctxname = session['cdp.name']
  def ssid = session['cdp.key']

  if (params.redir == "echo") {
    ctxname = servletContext?.getContextPath()
    ssid = 'echokey'
    }

  log.debug("setAttribute cadis.xml.name_${ssid} on ${ctxname} to ${params.cdpid}")
  servletContext?.getContext(ctxname)?.setAttribute("cadis.xml.name_${ssid}",params.cdpid+".metadata.xml")

  log.debug("setAttribute cadis.xml.data_${ssid} on ${ctxname} to ${xml}")
  servletContext?.getContext(ctxname)?.setAttribute("cadis.xml.data_${ssid}",xml)

  if (params.redir == "echo")
    redirect(action:echo)   

  def ctxpfx = session['cdp.name'] ? session['cdp.name'] : defctxpfx
  if (ctxpfx[-1] != '/') ctxpfx+='/'
  redirect(url:"${ctxpfx}cadisEditorStoreThredds?sid="+session['cdp.key'])
  }

  def echo = {}

}
