grails.servlet.version = "2.5" // Change depending on target container compliance (2.5 or 3.0)
grails.project.class.dir = "target/classes"
grails.project.test.class.dir = "target/test-classes"
grails.project.test.reports.dir = "target/test-reports"
grails.project.target.level = 1.6
grails.project.source.level = 1.6
grails.project.war.file = "target/${appName}-${appVersion}.${grails.util.Environment.current.name}.war"
//grails.project.war.file = "target/${appName}-${appVersion}.war"

grails.project.dependency.resolution = {
    legacyResolve false
    grails.project.dependency.resolver = "maven" // or ivy
    // inherit Grails' default dependencies
    inherits("global") {
        // uncomment to disable ehcache
        // excludes 'ehcache'
    }
    log "error" // log level of Ivy resolver, either 'error', 'warn', 'info', 'debug' or 'verbose'
    checksums true // Whether to verify checksums on resolve

    repositories {
        inherits true // Whether to inherit repository definitions from plugins
        grailsPlugins()
        grailsHome()
        grailsCentral()
        mavenCentral()

        // uncomment these to enable remote dependency resolution from public Maven repositories
        //mavenCentral()
//        mavenLocal()
//        mavenRepo "http://snapshots.repository.codehaus.org"
//        mavenRepo "http://repository.codehaus.org"
//        mavenRepo "http://download.java.net/maven/2/"
//        mavenRepo "http://repository.jboss.com/maven2/"

		// For Tomcat JDBC Pool Plugin Dependency
		mavenRepo "http://repository.springsource.com/maven/bundles/release/"
		mavenRepo "http://repository.springsource.com/maven/bundles/external/"
    }
    dependencies {
        // specify dependencies here under either 'build', 'compile', 'runtime', 'test' or 'provided' scopes eg.

        runtime 'mysql:mysql-connector-java:5.1.32'

		// For Tomcat JDBC Pool Plugin Dependency
		//compile ":jdbc-pool:1.0.9.3"

		// These should resolve the NekoHTML XML SAX parser dependency issues!
		runtime ('xerces:xercesImpl:2.8.1'){
			excludes "xml-apis"
		}
		runtime ('net.sourceforge.nekohtml:nekohtml:1.9.9'){
			excludes "xercesImpl"
		}
		// Found at: http://tapomay.blogspot.com/2010/04/fighting-javalangnoclassdeffounderror.html
    }

    plugins {
        //runtime ":hibernate:$grailsVersion"
        compile ":hibernate:3.6.10.2"
        compile ":jquery:1.11.1"
        compile ":jquery-ui:1.10.4"
        compile ":resources:1.2.14"
        compile ":famfamfam:1.0.1"
        compile ":jdbc-pool:7.0.47"
        compile ":mail:1.0.7"
        compile ":spring-security-core:1.2.7.4"
        compile ":spring-security-ui:0.2"
        compile ":scaffolding:2.0.0"

        //build ":tomcat:$grailsVersion"
        build ":tomcat:7.0.55.3"
    }
}
