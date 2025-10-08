dataSource {
	pooled = false
	driverClassName = 'com.mysql.jdbc.Driver'
	dialect = org.hibernate.dialect.MySQLInnoDBDialect
	dbCreate = 'update' //	one of create_drop, create, or update 
	username = 'landof'
	password = 'ice_and_snow'
}

//hibernate {
//  cache.use_second_level_cache=false
//  cache.use_query_cache=false
//  cache.provider_class=null
//}

hibernate {
  cache.use_second_level_cache=true
  cache.use_query_cache=true
  cache.provider_class='org.hibernate.cache.EhCacheProvider'
}

environments {
	development {
  		dataSource {
      		url = 'jdbc:mysql://localhost/bersea_dev'
    	}
  	}
  	test {
    	dataSource {
      		url = 'jdbc:mysql://localhost/bersea_test'
    	}
  	}
  	production {
    	dataSource {
            url = 'jdbc:mysql://riesling.eol.ucar.edu/bersea_prod'
//          url = 'jdbc:mysql://merlot.eol.ucar.edu/bersea_prod'
    	}
	}
}
