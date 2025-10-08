class UploadController {
    def index = { redirect(action:upload,params:params) }

    // the delete, save and update actions only
    // accept POST requests
    def allowedMethods = [UploadMultipleFiles:'POST']

    def upload = {
        [ dataset : Dataset.get( params.id ) ]
    }

    def UploadMultipleFiles = {
        log.info("upload: params = "+params)
        def dataset = Dataset.get( params['dataset.id'] )
	log.info("upload: dataset = "+dataset)

        def goodFiles = []
        def badFiles = []
        request.getFileNames().each {
	    def file = request.getFile(it)
	    log.info("upload: form name " + it)
            if(!file?.empty) {
	      def originalFilename = file.getOriginalFilename()
	      def f = new File(originalFilename)
	      def myFilename = UploadUtils.fixFileBadCharacters(UploadUtils.removeWindowsExtraStuff(f.getName()))
	      log.info("upload: got file "+myFilename)
	      try {
                file.transferTo( new File("/tmp/cementstorage",myFilename) )
		goodFiles+=myFilename
	      } catch (e) {
	        badFiles+=myFilename
	      }
            }
	}
	log.info("upload: myfiles = "+goodFiles)
	log.info("upload: errorfiles = "+badFiles)
	chain(action:uploadCompleted,model:['goodFiles':goodFiles,'badFiles':badFiles])
    }

    def uploadCompleted = {
    }

}
