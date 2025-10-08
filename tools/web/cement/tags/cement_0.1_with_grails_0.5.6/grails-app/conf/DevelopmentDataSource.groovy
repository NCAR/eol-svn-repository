class DevelopmentDataSource {
   boolean pooling = true
   String dbCreate = "update" // one of 'create', 'create-drop','update'
//   String url = "jdbc:hsqldb:mem:devDB"
//   String driverClassName = "org.hsqldb.jdbcDriver"
//   String driverClassName = "com.mysql.jdbc.Driver"
    String driverClassName = "com.p6spy.engine.spy.P6SpyDriver" // use this driver to enable p6spy logging
   String url = "jdbc:mysql://localhost/cement_dev"
   String username = "cement"
   String password = "grout"
}
