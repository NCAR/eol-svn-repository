// locations to search for config files that get merged into the main config
// config files can either be Java properties files or ConfigSlurper scripts

// grails.config.locations = [ "classpath:${appName}-config.properties",
//                             "classpath:${appName}-config.groovy",
//                             "file:${userHome}/.grails/${appName}-config.properties",
//                             "file:${userHome}/.grails/${appName}-config.groovy"]

// if (System.properties["${appName}.config.location"]) {
//    grails.config.locations << "file:" + System.properties["${appName}.config.location"]
// }


grails.project.groupId = appName // change this to alter the default package name and Maven publishing destination
grails.mime.file.extensions = true // enables the parsing of file extensions from URLs into the request format
grails.mime.use.accept.header = false
grails.mime.types = [ html: ['text/html','application/xhtml+xml'],
                      xml: ['text/xml', 'application/xml'],  // creates fatal error: Premature end of file in FireFox 3
                      text: 'text/plain',
                      js: 'text/javascript',
                      rss: 'application/rss+xml',
                      atom: 'application/atom+xml',
                      css: 'text/css',
                      csv: 'text/csv',
                      all: '*/*',
                      json: ['application/json','text/json'],
                      form: 'application/x-www-form-urlencoded',
                      multipartForm: 'multipart/form-data'
                    ]

// The default codec used to encode data with ${}
grails.views.default.codec = "none" // none, html, base64
grails.views.gsp.encoding = "UTF-8"
grails.converters.encoding = "UTF-8"

// enabled native2ascii conversion of i18n properties files
grails.enable.native2ascii = true

// enable Sitemesh preprocessing of GSP pages
grails.views.gsp.sitemesh.preprocess = true

// scaffolding templates configuration
grails.scaffolding.templates.domainSuffix = 'Instance'

// Set to false to use the new Grails 1.2 JSONBuilder in the render method
grails.json.legacy.builder = false

// whether to install the java.util.logging bridge for sl4j. Disable fo AppEngine!
grails.logging.jul.usebridge = true

// packages to include in Spring bean scanning
grails.spring.bean.packages = []



/* */
// URL Mapping Cache Max Size, defaults to 5000
//grails.urlmapping.cache.maxsize = 1000

// What URL patterns should be processed by the resources plugin
grails.resources.adhoc.patterns = ['/images/*', '/css/*', '/js/*', '/plugins/*']


// whether to disable processing of multi part requests
grails.web.disable.multipart=false

// request parameters to mask when logging exceptions
grails.exceptionresolver.params.exclude = ['password']

// enable query caching by default
grails.hibernate.cache.queries = true
/* */



// Grails Mail Service Configuration
/**/
grails {
	mail {
	  host = "smtp.eol.ucar.edu"
	  username = "localhost"
	  password = "xxxxx"
	}
 }
grails.mail.default.from = "metaarch@eol.ucar.edu"
/**/
// Grails Mail Service Configuration



// set per-environment serverURL stem for creating absolute links
environments {
    development {
		// File Location:	/data/orin/MetaArch
		grails.fileDestRootLoc = "/data/orin/MetaArch"
		
//		greenmail.disabled=true
		grails.mail.port = com.icegreen.greenmail.util.ServerSetupTest.SMTP.port
		grails.mail.host = 'localhost' // Comment out when on data.dev
		
//		grails.serverURL = "https://data.dev.eol.ucar.edu/${appName}"
		grails.serverURL = "https://localhost:8443/${appName}"
    }
	test {
		// File Location:	/data/orin/MetaArch
		grails.fileDestRootLoc = "/data/orin/MetaArch"
		
		grails.mail.port = com.icegreen.greenmail.util.ServerSetupTest.SMTP.port
		grails.mail.host = 'localhost'
		
		grails.serverURL = "http://localhost:8080/${appName}"
	}
    production {
		// File Location:	/scr/MetaArch
		grails.fileDestRootLoc = "/scr/MetaArch"
		
		greenmail.disabled=true
		
//		grails.serverURL = "https://data.dev.eol.ucar.edu/${appName}"
        grails.serverURL = "https://data.eol.ucar.edu/${appName}"
    }
}

environments {
//	development {
//		log4j = {
//			// in development mode, let's see all my log messages
//			debug 'grails.app'
//
//			error  'org.hibernate', 'net.sf.ehcache.hibernate'
//
//			error 'org.hibernate.type'
//			error 'org.hibernate.SQL'
//			error 'org.codehaus.groovy.grails.orm.hibernate' // hibernate integration
//
//			debug 'MetaArch'
//
//			debug  'org.springframework.security.authentication'
//		}
//	}

		def catalinaBase = System.properties.getProperty('catalina.base')
		if (!catalinaBase) catalinaBase = '.'   // just in case
		def logDirectory = "${catalinaBase}/logs"
        println "logDirectory = ${logDirectory}"
        def logFile = "${logDirectory}/${appName}/${appName}.log".toString()
        println "logFile = ${logFile}"
	
	development {

		log4j = {
			appenders {
				// set up a log file in the standard tomcat area; be sure to use .toString() with ${}
				rollingFile name:'tomcatLog', file:"${logFile}".toString(), maxFileSize:'1MB', maxBackupIndex:10

				// turn off stacketrace.log
				'null' name:'stacktrace'
			}

			root {
				// change the root logger to my tomcatLog file
				error 'tomcatLog'
				additivity = true
			}

			// example for sending stacktraces to my tomcatLog file
			//error tomcatLog:'StackTrace'

			// default grails levels
			error  'org.codehaus.groovy.grails.web.servlet',  //  controllers
				'org.codehaus.groovy.grails.web.pages', //  GSP
				'org.codehaus.groovy.grails.web.sitemesh', //  layouts
				'org.codehaus.groovy.grails.web.mapping.filter', // URL mapping
				'org.codehaus.groovy.grails.web.mapping', // URL mapping
				'org.codehaus.groovy.grails.commons', // core / classloading
				'org.codehaus.groovy.grails.plugins', // plugins
				'org.codehaus.groovy.grails.orm.hibernate', // hibernate integration
				'org.springframework',
				'org.hibernate',
				'net.sf.ehcache.hibernate'
			warn   'org.mortbay.log'
				'grails.app.services.grails.plugins.springsecurity.ui.SpringSecurityUiService' // 2.0
			//	'grails.app.service.grails.plugins.springsecurity.ui.SpringSecurityUiService' // pre-2.0

			// set level for my messages; this uses the root logger (and thus the tomcatLog file)
			info 'grails.app'
		}
	}
   

	production {
		log4j = {
			appenders {
				// set up a log file in the standard tomcat area; be sure to use .toString() with ${}
				rollingFile name:'tomcatLog', file:"${logFile}".toString(), maxFileSize:'1MB', maxBackupIndex:10

				// turn off stacketrace.log
				'null' name:'stacktrace'
			}

			root {
				// change the root logger to my tomcatLog file
				error 'tomcatLog'
				additivity = true
			}

			// example for sending stacktraces to my tomcatLog file
			//error tomcatLog:'StackTrace'

			// default grails levels
			error  'org.codehaus.groovy.grails.web.servlet',  //  controllers
				'org.codehaus.groovy.grails.web.pages', //  GSP
				'org.codehaus.groovy.grails.web.sitemesh', //  layouts
				'org.codehaus.groovy.grails.web.mapping.filter', // URL mapping
				'org.codehaus.groovy.grails.web.mapping', // URL mapping
				'org.codehaus.groovy.grails.commons', // core / classloading
				'org.codehaus.groovy.grails.plugins', // plugins
				'org.codehaus.groovy.grails.orm.hibernate', // hibernate integration
				'org.springframework',
				'org.hibernate',
				'net.sf.ehcache.hibernate'
			warn   'org.mortbay.log'
				'grails.app.services.grails.plugins.springsecurity.ui.SpringSecurityUiService' // 2.0
			//	'grails.app.service.grails.plugins.springsecurity.ui.SpringSecurityUiService' // pre-2.0

			// set level for my messages; this uses the root logger (and thus the tomcatLog file)
			info 'grails.app'
		}
	}
}


// Added by the Spring Security Core plugin:
grails.plugins.springsecurity.userLookup.userDomainClassName = 'meta.auth.User'
grails.plugins.springsecurity.userLookup.authorityJoinClassName = 'meta.auth.UserAuthority'
grails.plugins.springsecurity.authority.className = 'meta.auth.Authority'


// Added from zinc configurations of Spring Security Core
grails.plugins.springsecurity.rememberMe.cookieName = 'metaarch_cinnamonraisin'
grails.plugins.springsecurity.rememberMe.key = 'metaarch_iforget'
grails.plugins.springsecurity.rememberMe.tokenValiditySeconds = 86400 // 24 hours
grails.plugins.springsecurity.rememberMe.useSecureCookie = true
grails.plugins.springsecurity.rememberMe.parameter = 'xxx_disabled' // '_spring_security_remember_me'
// END - Added from zinc configurations of Spring Security Core

grails.plugins.springsecurity.useSessionFixationPrevention = true
grails.plugins.springsecurity.sessionFixationPrevention.migrate = false
grails.plugins.springsecurity.sessionFixationPrevention.alwaysCreateSession = true


// Additional security
grails.plugins.springsecurity.errors.login.fail = "Unable to find a user with that username and password. Is caps lock on?"
grails.plugins.springsecurity.errors.login.locked = "None shall pass."
// Additional security

// Spring Security Core UI Configuration
grails.plugins.springsecurity.ui.register.defaultRoleNames = ['ROLE_USER']
grails.plugins.springsecurity.ui.encodePassword = false  // Because User domain handles encryption
//grails.plugins.springsecurity.ui.password.validationRegex = "^.*(?=.*\\d)(?=.*[a-zA-Z])(?=.*[!@#$%^&\-_]).*$"
grails.plugins.springsecurity.ui.register.emailSubject = '[MetaArch] New Account'
grails.plugins.springsecurity.ui.forgotPassword.emailSubject = '[MetaArch] Password Reset'
grails.plugins.spring.security.ui.register.complete = 'Your registration is complete - please wait for administrative approval'
grails.plugins.spring.security.ui.register.postRegisterUrl = '/register/message'
// Spring Security Core UI Configuration

// CKEditor Configuration
ckeditor {
	config = "/js/ckeditor/config.js"
}
// CKEditor Configuration


// Added these lines manually
grails.plugins.springsecurity.securityConfigType = 'InterceptUrlMap'
grails.plugins.springsecurity.interceptUrlMap = [
	'/index.gsp':                ['IS_AUTHENTICATED_ANONYMOUSLY'],
	'/index':                    ['IS_AUTHENTICATED_ANONYMOUSLY'],
	'/index*':                   ['IS_AUTHENTICATED_ANONYMOUSLY'],
	'/login/auth':               ['IS_AUTHENTICATED_ANONYMOUSLY'],
	'/login/**':                 ['IS_AUTHENTICATED_ANONYMOUSLY'],
	'/logout/**':                ['IS_AUTHENTICATED_ANONYMOUSLY'],
	'/js/**':                    ['IS_AUTHENTICATED_ANONYMOUSLY'],
	'/css/**':                   ['IS_AUTHENTICATED_ANONYMOUSLY'],
	'/images/**':                ['IS_AUTHENTICATED_ANONYMOUSLY'],
	'/register/**':              ['IS_AUTHENTICATED_ANONYMOUSLY'],	
	'/admin':                    ['ROLE_ADMIN', 'ROLE_DEVELOP'], // Only admins/developers can see this page
	'/agency/**':                ['ROLE_ADMIN', 'ROLE_DEVELOP'], // Added to restrict all views to administrators/developers
	'/author/a*/**':             ['IS_AUTHENTICATED_FULLY'],
	'/author/**':                ['ROLE_ADMIN', 'ROLE_DEVELOP'], 
	'/authority/**':             ['ROLE_ADMIN', 'ROLE_DEVELOP'], 
	'/category/**':              ['ROLE_ADMIN', 'ROLE_DEVELOP'], 
	'/datasetAuthor/**':         ['ROLE_ADMIN', 'ROLE_DEVELOP'], 
	'/datasetCategory/**':       ['ROLE_ADMIN', 'ROLE_DEVELOP'], 
	'/datasetFrequency/**':      ['ROLE_ADMIN', 'ROLE_DEVELOP'], 
	'/datasetPlatform/**':       ['ROLE_ADMIN', 'ROLE_DEVELOP'], 
	'/file/saveMultiple':        ['IS_AUTHENTICATED_FULLY'],
	'/file/**':                  ['ROLE_ADMIN', 'ROLE_DEVELOP'], 
	'/fileType/**':              ['ROLE_ADMIN', 'ROLE_DEVELOP'], 
	'/format/**':                ['ROLE_ADMIN', 'ROLE_DEVELOP'], 
	'/frequency/**':             ['ROLE_ADMIN', 'ROLE_DEVELOP'], 
	'/gcmdInstrument/**':        ['ROLE_ADMIN', 'ROLE_DEVELOP'], 
	'/gcmdLocation/**':          ['ROLE_ADMIN', 'ROLE_DEVELOP'], 
	'/gcmdPlatform/**':          ['ROLE_ADMIN', 'ROLE_DEVELOP'], 
	'/gcmdScience/**':           ['ROLE_ADMIN', 'ROLE_DEVELOP'], 
	'/horizontalResolution/**':  ['ROLE_ADMIN', 'ROLE_DEVELOP'], 
	'/isoTopic/**':              ['ROLE_ADMIN', 'ROLE_DEVELOP'], 
	'/memberType/**':            ['ROLE_ADMIN', 'ROLE_DEVELOP'], 
	'/platform/**':              ['ROLE_ADMIN', 'ROLE_DEVELOP'], 
	'/projectMember/**':         ['ROLE_ADMIN', 'ROLE_DEVELOP'], 
	'/requirement/a*/**':        ['IS_AUTHENTICATED_FULLY'], 
	'/requirement/**':           ['ROLE_ADMIN', 'ROLE_DEVELOP'], 
	'/tempFile/adownload/**':    ['IS_AUTHENTICATED_FULLY'],
	'/tempFile/**':              ['ROLE_ADMIN', 'ROLE_DEVELOP'], 
	'/tooltip/**':               ['ROLE_ADMIN', 'ROLE_DEVELOP'], 
	'/userAuthority/**':         ['ROLE_ADMIN', 'ROLE_DEVELOP'], 
	'/verticalResolution/**':    ['ROLE_ADMIN', 'ROLE_DEVELOP'], 
	'/xlink/**':                 ['ROLE_ADMIN', 'ROLE_DEVELOP'], 
	'/xlinkType/**':             ['ROLE_ADMIN', 'ROLE_DEVELOP'], 
	'/xmlTemplate/**':           ['ROLE_ADMIN', 'ROLE_DEVELOP'], 
	'/**':                       ['IS_AUTHENTICATED_FULLY'], // Added to always be logged in when playing
]

// Added to autoFlush on save(), delete(), and merge()
//grails.gorm.autoFlush = true
//grails.gorm.failOnError = true
