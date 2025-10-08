import org.apache.catalina.valves.AccessLogValve;
//import org.apache.catalina.valves.RequestDumperValve;

eventConfigureTomcat = { tomcat ->
  /*
   * Create an access log under $HOME/.grails/<version>/projects/<project>/tomcat/logs
   * http://tomcat.apache.org/tomcat-6.0-doc/config/valve.html#Access%20Log%20Valve
   * http://grails.1312388.n4.nabble.com/grails-and-tomcat-s-access-log-td2232305.html
   */
    System.out.println('eventConfigureTomcat grailsEnv = ' + grailsEnv);
    if (grailsEnv == 'development') {
      try {
        def catalinaHome = System.getProperty("catalina.home")

        org.apache.catalina.valves.AccessLogValve alv = new org.apache.catalina.valves.AccessLogValve()
        alv.directory="${catalinaHome}/logs"
        alv.pattern='combined'
        alv.buffered=false
        alv.resolveHosts=false
        tomcat.engine.pipeline.addValve(alv)

        //org.apache.catalina.valves.RequestDumperValve rdv = new org.apache.catalina.valves.RequestDumperValve()
        //tomcat.engine.pipeline.addValve(rdv)

        def ctx=tomcat.host.findChild(serverContextPath)
        ctx.privileged = true 
      } catch (e) {
        System.err.println 'eventConfigureTomcat: trying to add logfile valves failed with exception:'
        System.err.println(e)
      }
    }
}
