dataSource {
  pooled = true
  dbCreate = 'update' // one of 'create', 'create-drop','update'
  driverClassName = 'com.mysql.jdbc.Driver'
  }

hibernate {
  cache.use_second_level_cache=true
  cache.use_query_cache=true
  cache.provider_class='org.hibernate.cache.EhCacheProvider'
  }

environments {

  production {
    dataSource {
      username = 'cadiseditor'
      password = 'cement2'
      dialect = org.hibernate.dialect.MySQLInnoDBDialect
      url = 'jdbc:mysql://riesling.eol.ucar.edu/cadis'
      }
    }

  development {
    dataSource {
      username = 'cement'
      password = 'grout'
      url = 'jdbc:mysql://localhost/cement_dev'
      //url = 'jdbc:mysql://merlot.eol.ucar.edu/cement_prod' // "old" beta production database
      }
    }

  test {
    dataSource {
      username = 'cement'
      password = 'grout'
      url = 'jdbc:mysql://localhost/cement_test'
      }
    }

  }
