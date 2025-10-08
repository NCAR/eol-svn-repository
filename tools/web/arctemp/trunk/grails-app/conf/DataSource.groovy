dataSource {
	pooled = false
	driverClassName = 'com.mysql.jdbc.Driver'
//	dbCreate = 'update'
	dbCreate = "create-drop"
	username = "landof"
	password = "ice_and_snow"
}

hibernate {
	cache.use_second_level_cache=true
	cache.use_query_cache=true
	cache.provider_class='org.hibernate.cache.EhCacheProvider'
}

environments {
	development {
  		dataSource {
      		dialect = org.hibernate.dialect.MySQLInnoDBDialect
      		url = 'jdbc:mysql://localhost/arctemp_dev'
    	}
  	}
  	test {
    	dataSource {
      		url = 'jdbc:mysql://localhost/arctemp_test'
    	}
  	}
  	production {
    	dataSource {
      		dialect = org.hibernate.dialect.MySQLInnoDBDialect
      		url = 'jdbc:mysql://merlot.eol.ucar.edu/arctemp_prod'
    	}
	}
}
