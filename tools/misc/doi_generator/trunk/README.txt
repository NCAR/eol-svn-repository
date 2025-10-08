--------------------------------------------------------------------------------
CREATING DOIs FOR DATASETS IN EMDAC

  Date: May 6, 2014
  Author: Amanda Orin
--------------------------------------------------------------------------------

Database currently being used to generate DOI metadata:
    zith9_doi on farskol.eol.ucar.edu
    -- desc doi;
    

Directory Structure:
    mkdoi.gr
      --> Script to be executed to generate DOIs. Currently requires manual 
          modification to include dataset archive IDs and/or project names.

    How_to_get_doi.txt
      --> Supplies the EZID login information for test/real DOIs.

    zinc/
      --> Contains the Datacite XML marshaller and core Zinc functionality

    grails-emdac-gorm-plugin/
      --> Grails plugin that supports Zinc. Contains GORM mappings for several 
          Zinc domains and controllers.

    grails-ncareol-extensions-plugin/
      --> Grails plugin that supports Zinc

    groovy-ncareol-extensions/
      --> Grails plugin that supports Zinc


About the Datacite XML marshaller:
  - Pulls together the various metadata fields for a project or dataset to 
    comply with Datacite XML format. See John (jja@ucar.edu) for full details.
    

Notes on executing the mkdoi.gr file:
  - Make sure you are calling it from inside of a grails application (otherwise 
    it will throw an error)
  - to call:
        grails run-script <path/to/script>/mkdoi.gr


		
Additional Information:
  - EZID Service is a library that the mkdoi.gr file utilizes. The URL to view 
    the source code for it is located at:
    https://github.com/mbjones/ezid/blob/master/src/main/java/edu/ucsb/nceas/ezid/EZIDService.java


----------------------------------------
Notes from Don & John:
    git clone file:///net/cds/git/zinc.git
    git checkout feature-2.x

    Using grails 2.3.7:
      grails upgrade

    depends on some hardcoded plugin paths, also need from git,
    cloned next to zinc (so ../ works):
      file:///net/cds/git/grails-emdac-gorm-plugin.git
      file:///net/cds/git/grails-ncareol-extensions-plugin.git
      file:///net/cds/git/groovy-ncareol-extensions.git

    copy:
    /h/eol/jja/work/ingcopies/git/zinc/grails-app/conf/DataSource.groovy

    ------------
    To view to see what is needed for DataCite submissions.
        DataCite metadata input form:
            http://www.miidi.org:8020/datacite/

        Latest ver 3 DataCite Schema: 
            http://schema.datacite.org/meta/kernel-3/index.html