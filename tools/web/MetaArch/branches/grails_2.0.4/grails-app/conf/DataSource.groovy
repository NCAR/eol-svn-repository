dataSource {
    pooled = true
    driverClassName = 'com.mysql.jdbc.Driver'
    
    dialect = org.hibernate.dialect.MySQLInnoDBDialect
    
    dbCreate = 'update' //  one of create_drop, create, or update
    
    username = 'landof'
    password = 'ice_and_snow'
}
dataSource_lookup {
    pooled = true
    driverClassName = 'com.mysql.jdbc.Driver'
    
    dialect = org.hibernate.dialect.MySQLInnoDBDialect
    
    dbCreate = 'none';
    //dbCreate = 'validate'  //went from bigint ids to int on farskol, doesn't validate
    
    username = 'landof'
    password = 'ice_and_snow'
}
hibernate {
    cache.use_second_level_cache = false //true
    cache.use_query_cache = false //true
    cache.region.factory_class = 'net.sf.ehcache.hibernate.EhCacheRegionFactory'
}
// environment specific settings
environments {
    development {
        dataSource {
          //url = 'jdbc:mysql://sferic-dev.eol.ucar.edu/metaarch_dev'
          url = 'jdbc:mysql://localhost/metaarch_dev'
            
            // now in connectionProperties
            //'?useUnicode=true&characterEncoding=UTF-8'+
            //'&serverTimezone=UTC&sessionVariables=time_zone="%2B00:00"'+
            //'&useTimezone=false&noTimezoneConversionForTimeType=false&useLegacyDatetimeCode=false'+
            //'&autoReconnect=true&autoReconnectForPools=true&failOverReadOnly=false'

            // time zones:
            // required/important:
            //  serverTimezone=UTC
            //  sessionVariables=time_zone="%2B00:00"  // +00:00, names like UTC or GMT not available unless DB is loaded with timezone info
            //  useLegacyDatetimeCode=false
            // optional/sounds good but doesn't seem to matter:
            //  noTimezoneConversionForTimeType=true
            // defaults (which sound like what we want):
            //  useTimezone=false
            //  useGmtMillisForDatetimes=false
            //  useJDBCCompliantTimezoneShift=false
            //  useSSPSCompatibleTimezoneShift=false

            /**/
            properties {
                validationQuery="select 1 as dbcp_connection_test"

                testOnBorrow=true
                testWhileIdle=true
                testOnReturn=false

                minIdle=1
                maxActive=8
                initialSize=2

                removeAbandoned=true
                removeAbandonedTimeout=14400
                logAbandoned=true
                numTestsPerEvictionRun=3    // commons-dbcp only, not used in tomcat-jdbc-pool
                minEvictableIdleTimeMillis=60000
                timeBetweenEvictionRunsMillis=60000

                // tomcat-jdbc-pool (plugin)
                validationInterval=60000

                // connectionProperties must start & end with ;
                // http://www.mail-archive.com/users@tomcat.apache.org/msg91607.html
                // http://bugs.mysql.com/bug.php?id=61201
                connectionProperties=';'+
                    'useUnicode=true;characterEncoding=UTF-8;'+
                    'serverTimezone=UTC;' +
                    //'sessionVariables=time_zone="+00:00";'+
                    'useTimezone=false;noTimezoneConversionForTimeType=true;useLegacyDatetimeCode=false;'+
                    'autoReconnect=false;autoReconnectForPools=true;failOverReadOnly=false;'

                        }
            /**/
                    }
        dataSource_lookup {
          url = 'jdbc:mysql://localhost/zith9'
          //url = 'jdbc:mysql://sferic-dev.eol.ucar.edu/zith9'
          //url = 'jdbc:mysql://sferic.eol.ucar.edu/zith9'
            
            /**/
            properties {
                validationQuery="select 1 as dbcp_connection_test"

                testOnBorrow=true
                testWhileIdle=true
                testOnReturn=false

                minIdle=1
                maxActive=8
                initialSize=2

                removeAbandoned=true
                removeAbandonedTimeout=14400
                logAbandoned=true
                numTestsPerEvictionRun=3    // commons-dbcp only, not used in tomcat-jdbc-pool
                minEvictableIdleTimeMillis=60000
                timeBetweenEvictionRunsMillis=60000

                // tomcat-jdbc-pool (plugin)
                validationInterval=60000

                // connectionProperties must start & end with ;
                // http://www.mail-archive.com/users@tomcat.apache.org/msg91607.html
                // http://bugs.mysql.com/bug.php?id=61201
                connectionProperties=';'+
                    'useUnicode=true;characterEncoding=UTF-8;'+
                    'serverTimezone=UTC;' +
                    //'sessionVariables=time_zone="+00:00";'+
                    'useTimezone=false;noTimezoneConversionForTimeType=true;useLegacyDatetimeCode=false;'+
                    'autoReconnect=false;autoReconnectForPools=true;failOverReadOnly=false;'

            }
            /**/
                    }
                }  // end dev
    test {
        dataSource {
            //url = "jdbc:h2:mem:testDb;MVCC=TRUE"
            //url = 'jdbc:mysql://sferic-dev.eol.ucar.edu/metaarch_test'
              url = 'jdbc:mysql://localhost/metaarch_dev'
        }
        dataSource_lookup {
            url = 'jdbc:mysql://localhost/zith9'
            //url = 'jdbc:mysql://sferic-dev.eol.ucar.edu/zith9'
            //url = 'jdbc:mysql://sferic.eol.ucar.edu/zith9'
        }
    }  // end test
    production {
        dataSource {
            //url = 'jdbc:mysql://localhost/metaarch_prod'
            //url = 'jdbc:mysql://sferic-dev.eol.ucar.edu/metaarch_test'
              url = 'jdbc:mysql://riesling.eol.ucar.edu/metaarch_prod'

            /**/
            properties {
                validationQuery="select 1 as dbcp_connection_test"

                testOnBorrow=true
                testWhileIdle=true
                testOnReturn=false

                //minIdle=1
                //maxActive=16
                //initialSize=2

                removeAbandoned=true
                removeAbandonedTimeout=14400
                logAbandoned=true
                numTestsPerEvictionRun=3    // commons-dbcp only, not used in tomcat-jdbc-pool
                minEvictableIdleTimeMillis=60000
                timeBetweenEvictionRunsMillis=60000

                // tomcat-jdbc-pool (plugin)
                validationInterval=60000

                // connectionProperties must start & end with ;
                // http://www.mail-archive.com/users@tomcat.apache.org/msg91607.html
                // http://bugs.mysql.com/bug.php?id=61201
                connectionProperties=';'+
                    'useUnicode=true;characterEncoding=UTF-8;'+
                    'serverTimezone=UTC;' +
                    //'sessionVariables=time_zone="+00:00";'+
                    'useTimezone=false;noTimezoneConversionForTimeType=true;useLegacyDatetimeCode=false;'+
                    'autoReconnect=false;autoReconnectForPools=true;failOverReadOnly=false;'

            }
            /**/
        }
        dataSource_lookup {
            //url = 'jdbc:mysql://localhost/zith9'
              url = 'jdbc:mysql://farskol.eol.ucar.edu/zith9'
            
            /**/
            properties {
                validationQuery="select 1 as dbcp_connection_test"

                testOnBorrow=true
                testWhileIdle=true
                testOnReturn=false

                //minIdle=1
                //maxActive=16
                //initialSize=2

                removeAbandoned=true
                removeAbandonedTimeout=14400
                logAbandoned=true
                numTestsPerEvictionRun=3    // commons-dbcp only, not used in tomcat-jdbc-pool
                minEvictableIdleTimeMillis=60000
                timeBetweenEvictionRunsMillis=60000

                // tomcat-jdbc-pool (plugin)
                validationInterval=60000

                // connectionProperties must start & end with ;
                // http://www.mail-archive.com/users@tomcat.apache.org/msg91607.html
                // http://bugs.mysql.com/bug.php?id=61201
                connectionProperties=';'+
                    'useUnicode=true;characterEncoding=UTF-8;'+
                    'serverTimezone=UTC;' +
                    //'sessionVariables=time_zone="+00:00";'+
                    'useTimezone=false;noTimezoneConversionForTimeType=true;useLegacyDatetimeCode=false;'+
                    'autoReconnect=false;autoReconnectForPools=true;failOverReadOnly=false;'

            }
            /**/
        }
    } // end prod
}
