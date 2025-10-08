//-----------------------------------------------------------------
// Utility script to help load the NPRB transferred datasets
// Reads in all *.xml files and 
//  writes out SQL commands to help create dataset in CODIAC
//
//  5 Jun 2011, ds
//    rev 10 Jun 11, ds
//    rev 21 Jun 11, ds
//    rev  2 Jul 11, ds
//    rev  3 Jul 11, 11:50am - ds
//    rev 12 Sep 11, ds 
//-----------------------------------------------------------------

def Project_Short_Name = "BSIERP"
def Project_Long_Name = "Bering Sea Integrated Ecosystem Research Program"
def id = "999"
def lastID = "999"
def p_i = "None"
def ds_num = 1

def beginDate = "2011-06-06"
def endDate = "2011-06-06"
        
def minLat = -90.00
def maxLat = 90.00
def minLon = -180.00
def maxLon = 180.00

def quote_space = "', '"
def begin_comma_space = "', "
def comma_space = ", "
def end_comma_space = ", '"
def start_values = "  ('"

new File(".").eachFile{infile ->
    if(infile.name.endsWith(".xml")){
		IDArray = infile.toString().split("_")
		id = IDArray[0];
		p_i = IDArray[1]

        println ""
        println "reading in " + infile 
        
        def x = new XmlSlurper().parse(infile)

        def v = [ 	ds_ID : IDArray[0],
                    name : x.idinfo.citation.citeinfo.title, 
                    begin_date : x.idinfo.timeperd.timeinfo.rngdates.begdate, 
                    end_date : x.idinfo.timeperd.timeinfo.rngdates.enddate, 
                    min_lat : x.idinfo.spdom.bounding.southbc, 
                    max_lat : x.idinfo.spdom.bounding.northbc, 
                    min_lon : x.idinfo.spdom.bounding.westbc, 
                    max_lon : x.idinfo.spdom.bounding.eastbc, 
                    description : x.idinfo.descript.abstract, 
					ds_creator : x.idinfo.citation.citeinfo.origin[0],
                    internal_contact : "149",      	// Don Stott is internal contact
                    onlineorderable : "1", 
                    row_revise_time : x.metainfo.metd ]
        
        println "$v.ds_ID, $v.name, $v.begin_date, $v.end_date."
        println "$v.min_lat, $v.max_lat, $v.min_lon, $v.max_lon"   
        
        def String this_ID = v.ds_ID - "./"
        def String dsID = this_ID.toUpperCase()
		(dsID == lastID) ?  ds_num++ : (ds_num = 1)
        def datasetID = "245." + dsID + "-00" + ds_num
		def dirID = dsID + "-00" + ds_num
        
        def String s = v.row_revise_time
        println "row revise time = $s"
		date_size = s.size()
        println "date size = $date_size"
        v.row_revise_time = s[0..3]
		if (date_size > 4) {
			v.row_revise_time = v.row_revise_time +"-"+s[4..5]
		}
		if (date_size > 6) {
			v.row_revise_time = v.row_revise_time +"-"+s[6..7]
		}
        println "row revise time is $v.row_revise_time"
          
        def num_dates = v.begin_date?.size()

        if (num_dates) {
            s = v.begin_date
			date_size = s.size()
			println "begin date is size $date_size"
            println "begin date = $s"
            beginDate = s[0..3]
			if (date_size > 4) {
				beginDate = beginDate +"-"+s[4..5]
			}
			if (date_size > 6) {
				beginDate = beginDate +"-"+s[6..7]
			}
            s = v.end_date
			date_size = s.size()
            endDate = s[0..3]
			if (date_size > 4) {
				endDate = endDate +"-"+s[4..5]
			}
			if (date_size > 6) {
				endDate = endDate +"-"+s[6..7]
			}
        	println "begin date is $beginDate"
        	println "end date is $endDate"
        } else {
            println "*** No start/stop dates for dataset $dsID ***"
        }
        
        def num_lats = v.min_lat?.size()
        def num_lons = v.min_lon?.size()

        if (num_lats) {
            if (v.min_lat[0] != "") { minLat = v.min_lat[0]?.toBigDecimal() }
            if (v.max_lat[0] != "") { maxLat = v.max_lat[0]?.toBigDecimal() }
            if (v.min_lon[0] != "") { minLon = v.min_lon[0]?.toBigDecimal() }
            if (v.max_lon[0] != "") { maxLon = v.max_lon[0]?.toBigDecimal() }
            if (num_lats > 1) {
                for (i in 0..(num_lats-1)) {
                    def thislat =  v.min_lat[i]?.toBigDecimal()
                    if(thislat < minLat) {minLat = thislat}
                    thislat = v.min_lat[i]?.toBigDecimal()
                    if(thislat > maxLat) {maxLat = thislat}
                    def thislon = v.min_lon[i]?.toBigDecimal()
                    if(thislon < minLon) {minLon = thislon}
                    thislon = v.max_lon[i]?.toBigDecimal()
                    if(thislon > maxLon) {maxLon = thislon}
                }
            }
        } else {
            println "*** No lat/lon for dataset $dsID ***"
        }

        print("  the ID is: " + datasetID + "\n")

		if (dsID != lastID) {
			println "making new directory for ${dsID}"
        	new File("../sql_inserts/${dirID}").mkdir()
		} else {
			println "dataset number $ds_num for dsID"
			println "making new directory for ${dsID}:${ds_num}"
        	new File("../sql_inserts/${dirID}").mkdir()
		}
 
        def outfile = new File("../sql_inserts/${dirID}/" + infile.name - ".xml" + ".sql") 
        println "writing out to " +outfile
        outfile.write("#----------------------------------------------\n")
        outfile.append("# SQL to load this dataset: "+datasetID+"\n") 
        outfile.append("#----------------------------------------------\n\n")

        // project metadata - only needed once!
        // outfile.append("#----------------------------------------------\n")
        // outfile.append("# project metadata\n")
        // outfile.append("#----------------------------------------------\n")
        // outfile.append("INSERT INTO project (id, full_name) VALUES\n")
        // outfile.append("  ('"+Project_Short_Name+"', '"+Project_Long_Name+"');\n\n")

        // dataset creator
        outfile.append("#----------------------------------------------\n")
        outfile.append("# dataset PI\n")
        outfile.append("# ")
        outfile.append(v.ds_creator)
        outfile.append("\n#----------------------------------------------\n\n")

        // contact metadata
        // outfile.append("#----------------------------------------------\n")
        // outfile.append("# contact metadata\n")
        // outfile.append("#----------------------------------------------\n")
        // outfile.append("INSERT INTO contact (person_name, email, phone, fax) VALUES\n")
        // outfile.append("  ('"+x.Personnel.First_Name[0]+" "+x.Personnel.Last_Name[0]+"', '"+x.Personnel.Email[0]+"', '"+x.Personnel.Phone[0]+"', '"+x.Personnel.Fax[0]+"');\n\n")
        // outfile.append("INSERT INTO contact (person_name, email, phone, fax) VALUES\n")
        // outfile.append("  ('"+x.Personnel.First_Name[1]+" "+x.Personnel.Last_Name[1]+"', '"+x.Personnel.Email[1]+"', '"+x.Personnel.Phone[1]+"', '"+x.Personnel.Fax[1]+"');\n\n")
        // outfile.append("INSERT INTO contact (person_name, email, phone, fax) VALUES\n")
        // outfile.append("  ('"+x.Personnel.First_Name[2]+" "+x.Personnel.Last_Name[2]+"', '"+x.Personnel.Email[2]+"', '"+x.Personnel.Phone[2]+"', '"+x.Personnel.Fax[2]+"');\n\n")

        // dataset metadata goes to own file
        File ds_outfile = new File("../sql_inserts/${dirID}/" + infile.name - ".xml" + "_dataset.sql") 
        ds_outfile.write("#----------------------------------------------------------------------------\n")
        ds_outfile.append("# " + datasetID+": " + Project_Short_Name + "\n")
        ds_outfile.append("#----------------------------------------------------------------------------\n\n")
        ds_outfile.append("#----------------------------------------------\n")
        ds_outfile.append("# dataset metadata\n")
        ds_outfile.append("#----------------------------------------------\n")
        ds_outfile.append("INSERT INTO dataset (dataset_id, name, begin_date, end_date, minlat, maxlat, minlon, maxlon, description, internal_contact_id, auth_reqd, onlineorderable, row_revise_time) VALUES\n")

        // write out the values, now
        ds_outfile.append(start_values)
        ds_outfile.append(datasetID)
        ds_outfile.append(quote_space)
        ds_outfile.append(v.name)
        ds_outfile.append(quote_space)
        ds_outfile.append(beginDate)
        ds_outfile.append(" 00:00:00")
        ds_outfile.append(quote_space)
        ds_outfile.append(endDate)
        ds_outfile.append(" 23:59:59")
        ds_outfile.append(begin_comma_space)
        ds_outfile.append(minLat)
        ds_outfile.append(comma_space)
        ds_outfile.append(maxLat)
        ds_outfile.append(comma_space)
        ds_outfile.append(minLon)
        ds_outfile.append(comma_space)
        ds_outfile.append(maxLon)
        ds_outfile.append(end_comma_space)
        ds_outfile.append(v.description)
        ds_outfile.append(begin_comma_space)
        ds_outfile.append(v.internal_contact)
        ds_outfile.append(comma_space)
		ds_outfile.append("1")
        ds_outfile.append(comma_space)
        ds_outfile.append(v.onlineorderable)
        ds_outfile.append(end_comma_space)
        ds_outfile.append(v.row_revise_time)
        if(x.metainfo.metd != '') {
              ds_outfile.append(" 11:00:00');\n\n")
        } else {
              ds_outfile.append("');\n\n")
        }

        // we'll add the readme info for the dataset, too
        ds_outfile.append("INSERT INTO file (dataset_id, directory, filename, begin_date, end_date, format_id, size_kb, purpose, data_archive_date) VALUES\n")
        ds_outfile.append(start_values)
        ds_outfile.append(datasetID)
        ds_outfile.append(quote_space)
        ds_outfile.append("/net/archive/data/bsierp/")
        ds_outfile.append(dsID)
        ds_outfile.append("/docs")
        ds_outfile.append(quote_space)
        ds_outfile.append(infile.name - ".xml" + ".html")
        ds_outfile.append(quote_space)
        ds_outfile.append(beginDate)
        ds_outfile.append(" 00:00:00")
        ds_outfile.append(quote_space)
        ds_outfile.append(endDate)
        ds_outfile.append(" 23:59:59")
        ds_outfile.append(begin_comma_space)       
        ds_outfile.append("83")                // HTML file type
        ds_outfile.append(comma_space)
        ds_outfile.append("25")
        ds_outfile.append(end_comma_space)
        ds_outfile.append("doc")
        ds_outfile.append(quote_space)
        ds_outfile.append(v.row_revise_time)
        if(x.metainfo.metd != '') {
              ds_outfile.append(" 11:00:00');\n\n")
        } else {
              ds_outfile.append("');\n\n")
        }

		// now put in the password protection
		ds_outfile.append("INSERT INTO dataset_user (dataset_id, username) VALUES\n")
        ds_outfile.append(start_values)
        ds_outfile.append(datasetID)
        ds_outfile.append(quote_space)
		ds_outfile.append("bsierp")	
        ds_outfile.append("');\n\n")

        // dataset_project metadata goes into own file
        File dp_outfile = new File("../sql_inserts/${dirID}/" + infile.name - ".xml" + "_ds-proj.sql") 
        dp_outfile.write("#----------------------------------------------------------------------------\n")
        dp_outfile.append("# " + datasetID+": " + Project_Short_Name + "\n")                  
        dp_outfile.append("#----------------------------------------------------------------------------\n\n") 
        dp_outfile.append("#----------------------------------------------\n") 
        dp_outfile.append("# dataset_project metadata\n")            
        dp_outfile.append("#----------------------------------------------\n") 
        dp_outfile.append("INSERT INTO dataset_project (dataset_id, project_id) VALUES\n")                    
        dp_outfile.append(start_values) 
        dp_outfile.append(datasetID) 
        dp_outfile.append(quote_space) 
        dp_outfile.append(Project_Short_Name) 
        dp_outfile.append("');\n\n")

        // master list metadata goes into own file
        // master list metadata - dataset
        File ml_outfile = new File("../sql_inserts/${dirID}/" + infile.name - ".xml" + "_masterlist.sql") 
        ml_outfile.write("#----------------------------------------------------------------------------\n")
        ml_outfile.append("# " + datasetID+": " + Project_Short_Name + "\n")
        ml_outfile.append("#----------------------------------------------------------------------------\n\n")
        ml_outfile.append("#----------------------------------------------\n")
        ml_outfile.append("# master list metadata for dataset\n")
        ml_outfile.append("#----------------------------------------------\n")
        ml_outfile.append("INSERT INTO dataset (dataset_id, name, url, doc_url, author_pi) VALUES\n")
        ml_outfile.append(start_values)
        ml_outfile.append(datasetID)
        ml_outfile.append(quote_space)
        ml_outfile.append(v.name)
        ml_outfile.append(quote_space)
        ml_outfile.append("http://data.eol.ucar.edu/codiac/dss/id=")
        ml_outfile.append(datasetID)
        ml_outfile.append(quote_space)
        ml_outfile.append("http://data.eol.ucar.edu/datafile/nph-get/")
        ml_outfile.append(datasetID+"/"+infile.name - ".xml" + ".html")
        ml_outfile.append(quote_space)
        ml_outfile.append(v.ds_creator)
        ml_outfile.append("');\n\n")

        // master list metadata - dataset_project
        ml_outfile.append("#----------------------------------------------\n")
        ml_outfile.append("# master list metadata for dataset_project\n")
        ml_outfile.append("#----------------------------------------------\n")
        ml_outfile.append("INSERT INTO dataset_project (dataset_id, project_id, date_posted, hide_flag) VALUES\n")
        ml_outfile.append(start_values)
        ml_outfile.append(datasetID)
        ml_outfile.append(quote_space)
        ml_outfile.append(Project_Short_Name) 
        ml_outfile.append(quote_space)
        ml_outfile.append(v.row_revise_time)
        ml_outfile.append("', 1")
        ml_outfile.append(");\n\n")

        // xlink metadata
        // outfile.append("#----------------------------------------------\n")
        // outfile.append("# xlink metadata\n")
        // outfile.append("#----------------------------------------------\n")
        // outfile.append("INSERT INTO xlink (href, title) VALUES\n")
        // outfile.append("  ('"+x.Related_URL[0].URL+"', '"+x.Related_URL[0].Description+"');\n\n")
        // outfile.append("INSERT INTO xlink (href, title) VALUES\n")
        // outfile.append("  ('"+x.Related_URL[1].URL+"', '"+x.Related_URL[1].Description+"');\n\n")

        // keywords for dataset
        // outfile.append("#----------------------------------------------\n")
        // outfile.append("# keywords metadata\n")
        // outfile.append("#----------------------------------------------\n")
        // outfile.append("/* \n\n")
        // outfile.append("Source Name: " + x.Source_Name[0].Long_Name + ", " + x.Source_Name[0].Short_Name + "; " + x.Source_name[1].Long_Name + ", " + x.Source_Name[1].Short_Name + ".")
        // def keyWordList = x.Keyword
        // outfile.append("\n\nKeyword list:\n")
        // keyWordList.each{outfile.append(it); outfile.append("\n")}  
        // outfile.append("*/ \n")
		
		lastID = dsID 
    }

}