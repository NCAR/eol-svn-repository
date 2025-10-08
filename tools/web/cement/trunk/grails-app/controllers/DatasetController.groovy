import org.hibernate.FetchMode as FM
import grails.util.GrailsUtil

class DatasetController {
    
    // the delete, save and update actions only accept POST requests
    def allowedMethods = [delete:'POST', save:'POST', update:'POST']

    // double-check authorization in case filter gets a null actionName
    //  (e.g. in the case of forms submitted to the dispatcher)
    def beforeInterceptor = [action:this.&auth, only:['delete','metadata','validateXml']]

    def datasetAuthorizationService
    def projectAuthorizationService

    def threddsXmlService


    def auth() {
        if (session.username && session.isAdmin) return true
        flash.message = 'Not authorized'
        if (params.id != null)
          redirect(action:show,id:params.id)
        else redirect(uri:'/')
        return false
    }


    def index = { redirect(action:list,params:params) }



    def cancel = {  // here so we get a nice button in edit view
      redirect(action:show, id:params.id)
    }



    def list = { // view: list
        if(!params.max) params.max = 20
        [ datasetList: Dataset.list( params ) ]
    }



    def show = { // view: show
        def dataset = (params.id != null) ? Dataset.get( params.id ) : null
        if (!dataset) {
            flash.message = 'Dataset not found'
            redirect(action:list)
            return
        }
        else return [ dataset : dataset ]
    }



    def delete = { // views: from=edit, to=list
        def dataset = (params.id != null) ? Dataset.get( params.id ) : null
        if(dataset) {
            dataset.delete()
            flash.message = 'Dataset deleted'
            redirect(action:review)
        }
        else {
            flash.message = 'Dataset not found'
            redirect(action:list)
        }
    }



    def edit = {               // views: from=(none), display=edit, next_to=save,update,
        log.debug('edit')
        def dataset = null

        if (params.id != null) {
          log.debug("Trying params.id = ${params.id}")
          if (params.id =~ /^\d+$/)
            dataset = Dataset.get( params.id )
          else dataset = Dataset.findByEntryID(params.id)
          log.debug(" got dataset = ${dataset}")
          log.debug(" got dataset ID = ${dataset.entryID}")
          }

        if (!dataset) {
          log.debug("no dataset, looking at hash ${session['cdp.hash']}")
          if (session['cdp.hash']?.datasetId && session['cdp.hash'].datasetId != "''") {
            log.debug("Trying hash.datasetId ${session['cdp.hash'].datasetId}")
            dataset = Dataset.findByEntryID(session['cdp.hash'].datasetId)
            log.debug(" got dataset = ${dataset}")
           }
        }

        if(!dataset) {
            flash.message = 'Dataset not found'
            redirect(action:review)
            return
        }

          // ugh, security code in the middle of our logic
        if (!datasetAuthorizationService.canEditDataset(dataset, session.username, session.cadisPrivileges)) {
            flash.message = 'Not authorized to edit dataset.'
            redirect(action:'show',id:dataset.id)
            return
        }

        def myList = projectAuthorizationService.permittedProjects(session.isAdmin, session.cadisPrivileges)
        return ['dataset':dataset, 'myList':myList]
    }



    def update = { // view: from=edit ("Update")
        def dataset = (params.id != null) ? Dataset.get( params.id ) : null

        if (!dataset) {
            flash.message = 'Dataset not found'
            redirect(action:review)
            return
        }

          // ugh, security code in the middle of our logic
        if (!datasetAuthorizationService.canEditDataset(dataset, session.username, session.cadisPrivileges)) {
            flash.message = 'Not authorized to update dataset.'
            redirect(action:'show',id:dataset.id)
            return
        }


        // keep original entryID (alternative is reset it below)
        //if (params.entryID) params.remove('entryID')

        dataset.properties = params

        // in order to keep in sync with Project entryID
        // reset entryID (alternative is keep original above)
        dataset.entryID = dataset.project?.entryID + '.' + dataset.theTitleEntryID()

        if (!session.isAdmin && !projectAuthorizationService.isaPermittedProject(dataset.project,session.cadisPrivileges)) {
            flash.message = 'Not authorized to add dataset to chosen project.'
            def myList = projectAuthorizationService.permittedProjects(session.isAdmin, session.cadisPrivileges)
            if (!myList?.size()) flash.message += ' Not authorized for any projects.'
            render(view:'edit', model:[dataset:dataset, 'myList':myList])
            return
          }

        if(!dataset.hasErrors() && dataset.save()) {
            flash.message = 'Dataset updated'
            redirect(action:show,id:dataset.id)
            return
        }
        else {
            def myList = projectAuthorizationService.permittedProjects(session.isAdmin, session.cadisPrivileges)
            render(view:'edit', model:[dataset:dataset, 'myList':myList])
            return
        }

    }



    def create = { // view: create
        // initial call: make a blank dataset and give it to view
        // for redirects on save errors: set up a dataset object from params and give to view

        log.debug("create params = ${params}")

        def dataset = new Dataset()
        dataset.properties = params

        // we are creating a new dataset, but CDP might send a THREDDS ID
        //   assume it's for a project
        if (session['cdp.hash']?.datasetId && session['cdp.hash'].datasetId != "''") {
          log.debug("*** a dataset id from the cdp.hash = ${session['cdp.hash']?.datasetId}")
          def project = Project.findByEntryID(session['cdp.hash'].datasetId)
          if (project) dataset.project = project
          }

        def me = Contact.findByShortName(session.username)
        if (me) dataset.metadataContact = me

        def cadis_contact  = Contact.findByShortName('stott')
        if (cadis_contact) dataset.datacenterContact = cadis_contact

        def myList = projectAuthorizationService.permittedProjects(session.isAdmin, session.cadisPrivileges)
        return ['dataset':dataset, 'myList': myList]
    }



    def save = { // views: from=create,edit, success=show, error=create
        log.debug("save params.id = ${params?.id}")
        log.debug(" session = ${session}")
        if (params.id != null) params.remove('id') // if coming from edit, or malicious user
        def dataset = new Dataset()
        dataset.properties = params
        dataset.entryID = dataset.project?.entryID + '.' + dataset.theTitleEntryID()
        log.debug("entryID in save = ${dataset.entryID}")

        if(dataset.hasErrors()) {
            log.debug("save dataset hasErrors")
            def myList = projectAuthorizationService.permittedProjects(session.isAdmin, session.cadisPrivileges)
            render(view:'create', model:[dataset:dataset, 'myList':myList])
            return
        }

        if (dataset.project == null) {
            log.debug("save project is null")
            flash.message = 'Dataset requires a project.'
            def myList = projectAuthorizationService.permittedProjects(session.isAdmin, session.cadisPrivileges)
            if (!myList?.size()) flash.message += ' Not authorized for any projects.'
            render(view:'create', model:[dataset:dataset, 'myList':myList])
            return
        }

        if (!session.isAdmin && !projectAuthorizationService.isaPermittedProject(dataset.project,session.cadisPrivileges)) {
            log.debug("save not isaPermittedProject ${dataset.project?.entryID} -- ${session.cadisPrivileges}")
            flash.message = 'Not authorized to save to chosen project.'
            def myList = projectAuthorizationService.permittedProjects(session.isAdmin, session.cadisPrivileges)
            render(view:'create', model:[dataset:dataset, 'myList':myList])
            return
          }

          // ugh, security code in the middle of our logic
        if (!session.isAdmin && !datasetAuthorizationService.hasCdpPrivileges(dataset, session.cadisPrivileges)) {
          flash.message = 'Not authorized to create dataset on the portal.'
          def myList = projectAuthorizationService.permittedProjects(session.isAdmin, session.cadisPrivileges)
          render(view:'create', model:[dataset:dataset, 'myList':myList])
          return
          }

        if(dataset.save()) {
            flash.message = 'Dataset created'
            redirect(action:show, id:dataset.id)
            return
        }
        else {
          def myList = projectAuthorizationService.permittedProjects(session.isAdmin, session.cadisPrivileges)
          render(view:'create', model:[dataset:dataset, 'myList':myList])
          return
        }
    }



    def template = {
      log.debug('template')
      def dataset = null
      def datasets = null

      if (params.id != null) {
          log.debug("Trying params.id = ${params.id}")
          if (params.id =~ /^\d+$/)
              dataset = Dataset.get( params.id )
          else dataset = Dataset.findByEntryID(params.id)
          log.debug(" got dataset = ${dataset}")
          }

      if (!dataset) log.debug("no dataset, looking at hash ${session['cdp.hash']}")
      if ( (!dataset) &&
           (session['cdp.hash']?.datasetId) && (session['cdp.hash'].datasetId != "''")
           ) {
        log.debug("Trying hash.datasetId ${session['cdp.hash'].datasetId}")
        dataset = Dataset.findByEntryID(session['cdp.hash'].datasetId)
        if (!dataset) {
          log.debug('no dataset, trying projects')
          datasets = Project.findByEntryID(session['cdp.hash'].datasetId)?.datasets
          log.debug("found ${datasets?.size()} datasets isa ${datasets.getClass()}")
          if (!datasets?.size()) {
            log.debug('no datasets, trying dataset like')
            datasets = Dataset.findAllByEntryIDLike(session['cdp.hash'].datasetId + '%')
            }
          log.debug("found ${datasets?.size()} datasets isa ${datasets.getClass()}")
          switch (datasets?.size()) {
            case 1:
              dataset = datasets[0]
              break
            case 0:
            case null:
              dataset = null
              break
            default: // multiple
              log.debug('multiple datasets, rendering review')
              render(view:'review',
                     model:[contact:Contact.findByShortName(session.username),
                            datasets:datasets, what:'template'])
              return
            }
          }
        }

      if (!dataset) {
        flash.message = 'Dataset not found'
        // redirect to create and let it try its logic to find a project
        if (params.id != null) params.remove('id')
        redirect(action:'create',params:params)
        return
        }

      def me = Contact.findByShortName(session.username)
      if (me) dataset.metadataContact = me

      // found a dataset to use as a template, so pass it to create view
      def myList = projectAuthorizationService.permittedProjects(session.isAdmin, session.cadisPrivileges)
      render(view:'create',model:[dataset:dataset, 'myList':myList])
      }



    def review = {
      log.debug('review letsgo')
      log.debug("params ${params}")
      log.debug("request ${request}")
      log.debug("session ${session}")

      def me = null
      if (params.id != null) me = Contact.get( params.id )
      if (!me) me = Contact.findByShortName(session.username)

      if (me) {
        log.info("review found me ${me.id} ${me.email}")
        def criteria = Dataset.createCriteria()
        def results = criteria {
          // maybe add: and { like('entryID',cdpDatasetId+'%') ... }
          or {
            eq('metadataContact',me)
            eq('datacenterContact',me)
            project {
              eq('piContact',me)
              }
            }
          fetchMode('project', FM.EAGER)
        }
        return [ contact : me, datasets : results ]
      }
      else {
            flash.message = "Bad error - can't find your contact info."
      }
    }



    def upload = { // same logic as review, so just chain there with a parameter for the view
        log.debug("upload params = ${params}")
        chain(action:'review', model:[what:'upload'], params:params)
    }



    def uploadto = { // dummy action to simplify view code
        def dataset = (params.id != null) ? Dataset.get( params.id ) : null
        if(!dataset) {
            flash.message = 'Dataset not found'
            redirect(action:review)
            return
        }

          // ugh, security code in the middle of our logic
        if (!session.isAdmin && !datasetAuthorizationService.hasCdpPrivileges(dataset, session.cadisPrivileges)) {
          flash.message = 'Not authorized to upload to dataset.'
          redirect(action:show,id:dataset.id)
          return
          }

        def uri = 'http://dataportal.ucar.edu/metadata/cadis/' +
                  dataset.entryID.  // THREDDS/CDP ID
                    replaceFirst('^'+Project.entryIDprefix,''). // remove org.nsf.aon.cadis.
                    replaceAll('\\.','/') + // dot to slash
                    '/' + dataset.theTitleEntryID() + '.thredds.xml'

        def ctxname = session['cdp.name']
        def ctxpfx = (ctxname && (ctxname != "ROOT")) ? ctxname : ''
        if (ctxpfx[-1] != '/') ctxpfx+='/'
        redirect(url:ctxpfx+'browse/browse.htm?uri='+uri.encodeAsURL());
    }



    def metadata = {
        log.debug("metadata: params = ${params}")
        def dataset = null
        if (params.id != null) {
            if (params.id =~ /^\d+$/)
                dataset = Dataset.get( params.id )
            else dataset = Dataset.findByEntryID(params.id)
        }
        log.debug("metadata: dataset = ${dataset}")
        if (!dataset) {
            render(contentType:'text/xml', encoding:'UTF-8', text:'<metadata></metadata>')
            return
        }
        [dataset:dataset]
    }



    def validateXml = {
        def dataset = (params.id != null) ? Dataset.get( params.id ) : null
        if(!dataset) {
            flash.message = 'Dataset not found'
            redirect(action:review)
            return
        }

        def xml = g.render(template:'metadata', bean:dataset)
        response.contentType = 'text/html' // reset after metadata template, in case of error
        //log.debug("dataset xml = ${xml}")
        def errs = threddsXmlService.validate(xml)
        if (errs) {
            flash.message = 'Dataset does not produce valid XML. Please edit. ' + errs
            redirect(action:edit,id:dataset.id)
            return
        }
        else {
            flash.message = 'Dataset produces valid XML.'
            redirect(action:show,id:dataset.id)
            return
        }

    }



 def submit = {
  log.debug("submit")
  log.debug("params = ${params}")

  def ctxname = session['cdp.name']
   log.debug("context name = ${ctxname}")
  def ssid = session['cdp.key']
   log.debug("ssid = ${ssid}")
  def hh = session['cdp.hash']
   log.debug("session hash = ${hh}")

  def d = null
  if (params.id != null) {
    if (params.id =~ /^\d+$/)
      d = Dataset.get( params.id )
    else d = Dataset.findByEntryID(params.id)
    }
  log.debug("dataset by params.id = ${d}")

  if (!d) {
    d = Dataset.findByEntryID(hh.datasetId)
    log.debug("dataset by hh.id = ${d}")
    }

  if (!d) {
    flash.message = 'No dataset found.'
    redirect(action:review)
    return
    }

    // ugh, security code in the middle of our logic
  if (!session.isAdmin && !datasetAuthorizationService.hasCdpPrivileges(d, session.cadisPrivileges)) {
    flash.message = 'Not authorized to submit dataset to the portal.'
    redirect(action:show,id:d.id)
    return
    }

  def xml = g.render(template:"metadata", bean:d)
  response.contentType = 'text/html' // reset after metadata template, in case of error
  //log.debug("xml = ${xml}")
  def errs = threddsXmlService.validate(xml)
  if (errs) {
    flash.message = 'Dataset does not produce valid XML. Please edit. ' + errs
    redirect(action:edit,id:d.id)
    return
    }

  if (hh == null) {
    log.debug('cdp hash was null, creating new')
    hh = new Hashtable<String,String>()
    ht = servletContext?.getContext(ctxname)?.getAttribute(cdpattr)
    if (ht == null) {
      log.debug('cannot find cdp outer hash; trying new and set')
      ht = new Hashtable<String,Hashtable>()
      if (ht) servletContext?.getContext(ctxname)?.setAttribute(cdpattr,ht)
      }
    if (ht) {
      ht[ssid] = hh
      }
    else {
      log.debug('cannot find or build cdp outer hash')
      hh = null
      }
    }
  log.debug("hh (inner hash) before updates = ${hh}")

  if (hh == null) { // still !?!?
    flash.message = 'Cannot find or build a way to talk to the CDP'
    log.debug('Cannot find or build a hashtable')
    redirect(action:show,id:params.id)
    return
    }

  hh['datasetId'] = d?.entryID
  hh['datasetName'] = d?.title
  hh['metadata.xml'] = xml
  log.debug("hh (inner hash) after updates = ${hh}")

  if (GrailsUtil.isDevelopmentEnv())
    redirect(controller:'auth',action:'show',params:[sid:ssid])
  else {
    def ctxpfx = (ctxname && (ctxname != "ROOT")) ? ctxname : ''
    if (ctxpfx[-1] != '/') ctxpfx+='/'
    log.debug("redir to ${ctxpfx}cadisEditorStoreThredds.do?sid=${ssid}")
    redirect(url:"${ctxpfx}cadisEditorStoreThredds.do?sid=${ssid}")
    }
  return
  }


}
