// locations to search for config files that get merged into the main config
// config files can either be Java properties files or ConfigSlurper scripts

// grails.config.locations = [ "classpath:${appName}-config.properties",
//                             "classpath:${appName}-config.groovy",
//                             "file:${userHome}/.grails/${appName}-config.properties",
//                             "file:${userHome}/.grails/${appName}-config.groovy"]

if(System.properties["${appName}.config.location"]) {
    grails.config.locations << "file:" + System.properties["${appName}.config.location"]
}
// enabled native2ascii conversion of i18n properties files
grails.enable.native2ascii = true

// log4j configuration
log4j {
    appender.stdout = "org.apache.log4j.ConsoleAppender"
    appender.'stdout.layout'="org.apache.log4j.PatternLayout"
    appender.'stdout.layout.ConversionPattern'='[%r] %c{2} %m%n'
    //appender.telnet = "org.apache.log4j.net.TelnetAppender"
    //appender.'telnet.port'=5678
    //appender.'telnet.layout'="org.apache.log4j.PatternLayout"
    //appender.'telnet.layout.ConversionPattern'='[%r] %c{2} %m%n'
    //appender.syslog = "org.apache.log4j.net.SyslogAppender"
    //appender.'syslog.syslogHost'="128.117.85.3"
    //appender.'syslog.facility'="LOCAL5"
    //appender.'syslog.header'=true
    //appender.'syslog.layout'="org.apache.log4j.PatternLayout"
    //appender.'syslog.layout.ConversionPattern'='[%r] %c{2} %m%n'
    rootLogger="error,stdout"
    logger {
	//grails."app"="debug,stdout"
        //grails="warn,stdout"
        org {
            codehaus.groovy.grails.web.servlet="error,stdout"  //  controllers
            codehaus.groovy.grails.web.errors="error,stdout"  //  web layer errors            
	    codehaus.groovy.grails.web.pages="error,stdout" //  GSP
            codehaus.groovy.grails.web.sitemesh="error,stdout" //  layouts
            codehaus.groovy.grails."web.mapping.filter"="error,stdout" // URL mapping
            codehaus.groovy.grails."web.mapping"="error,stdout" // URL mapping
            codehaus.groovy.grails.commons="info,stdout" // core / classloading
            codehaus.groovy.grails.plugins="error,stdout" // plugins
            codehaus.groovy.grails.orm.hibernate="error,stdout" // hibernate integration
            springframework="off,stdout"
            hibernate="off,stdout"
        }
    }
    additivity.'default' = false
    additivity {
	grails=false
	org {
           codehaus.groovy.grails=false
           springframework=false
	   hibernate=false
	}
    }
}

environments {
  development {
    log4j {
      logger {
        //rootLogger="debug,stdout"
        grails."app"="debug,stdout"
        grails="debug,stdout"
      }
    }
  }
  production {
    log4j {
      logger {
        //rootLogger="debug,stdout"
        grails."app"="debug,stdout"
        grails="debug,stdout"
      }
    }
  }
}

