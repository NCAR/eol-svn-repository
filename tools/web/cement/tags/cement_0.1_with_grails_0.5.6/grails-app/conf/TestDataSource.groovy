class TestDataSource {
   boolean pooling = true
   String dbCreate = "update" // one of 'create', 'create-drop','update'
   String url = "jdbc:mysql://localhost/cement_test"
   String driverClassName = "com.mysql.jdbc.Driver"
//    String driverClassName = "com.p6spy.engine.spy.P6SpyDriver" // use this driver to enable p6spy logging
   String username = "cement"
   String password = "grout"
}
