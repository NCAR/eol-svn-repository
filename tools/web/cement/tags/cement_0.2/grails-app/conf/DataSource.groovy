dataSource {
	pooled = true
	dbCreate = "update" // one of 'create', 'create-drop','update'
	driverClassName = "com.mysql.jdbc.Driver"
	username = "cement"
	password = "grout"
}
hibernate {
    cache.use_second_level_cache=true
    cache.use_query_cache=true
    cache.provider_class='org.hibernate.cache.EhCacheProvider'
}
// environment specific settings
environments {
	development {
		dataSource {
			driverClassName = "com.p6spy.engine.spy.P6SpyDriver" // p6spy logging
			url = "jdbc:mysql://localhost/cement_dev"
		}
	}
	test {
		dataSource {
			url = "jdbc:mysql://localhost/cement_test"
		}
	}
	production {
		dataSource {
			url = "jdbc:mysql://localhost/cement_prod"
		}
	}
}
