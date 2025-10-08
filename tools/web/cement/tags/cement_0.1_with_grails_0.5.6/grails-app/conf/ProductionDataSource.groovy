class ProductionDataSource {
   boolean pooling = true
   String dbCreate = "update" // one of 'create', 'create-drop','update'
   String url = "jdbc:mysql://localhost/cement_prod"
   String driverClassName = "com.mysql.jdbc.Driver"
   String username = "cement"
   String password = "grout"
}
