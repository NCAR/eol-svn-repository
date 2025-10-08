/* ********************** authorshipConnect.groovy ************************* */
/*
	DATE:
	Created on October 7, 2013.
	Modified on May 9, 2014.
	Modified on Dec 15, 2014. - changed db back to zith9 on farskol


	AUTHOR:
	Amanda Orin

       Modified April 2015 by J. Scannell 
       1) If only one contact to choose, then contact number is entered for the user.
          If there is a possible error in the ML, then no contact number is entered for the user.
       2) Check for numeric answer when choosing a contact from the list.
       3) Add more descriptive comments.
       4) Flag as possible error in ML if author has more than 3 blanks in the name. 
          This can catch problems if first author is entered as firstname lastname in ML.
       5) If the logs directory doesn't already exist, create it.
       6) Remove dataset from questionDatasets if known.
	
	
	ABOUT THIS SCRIPT:
	This script is designed to be executed via command line to assess the 
	current state of the datasets within a given Master List to verify that 
	the authors provided are connected as authors to the dataset in Zinc as 
	well as in the correct sorted order. The script parses the Master List 
	author_pi field of datasets to separate authors given a set of conventions, 
	which can be found at:
		https://internal.eol.ucar.edu/node/395
		
	Additional details on how to do this process manually can be found at:
		https://internal.eol.ucar.edu/node/396
	
	This script is interactive. Specifically, it will prompt you to input the 
	contact ID for the specific parsed author from the given list of suggested 
	contacts found which may match the author string. However, if you do not 
	see the contact ID in the list provided but do know the contact ID, you can 
	enter that in and the script will attempt to find that contact for you to 
	confirm.
	
	Based upon your feedback as well as what has already been learned 
	(contained in nicknames.txt to re-use between script executions), the script 
	references a key-value hash to add new string versions of contacts (i.e. the 
	key is the contact ID, and the value is an array of strings containing 
	previously identified "nicknames" that are associated with said contact). 
	For a given dataset being checked, an array of contact IDs is constructed 
	where the given index order of elements is the sort order used by the Master 
	List string. This array is then referenced against the group of dataset 
	contacts with iso_citation_role of "author" to see if it has already been 
	added to Zinc as an author for said dataset and if its sort order is 
	correct.
	
	Authors not found in the Contacts are added to unknowns.txt; contacts not 
	found as an author for a dataset in Zinc are automatically added, and sort 
	order is modified to the appropriate value. However, this script does not 
	delete author-type contacts from datasets in Zinc - this must still be done 
	manually.
	
	Questionable datasets are datasets which may need their author_pi string 
	modified slightly to improve parsing and contact searching, and are stored 
	in questionable_datasets.txt.
	
	
	NOTES:
	When you execute this script, be sure to add and check in any generated 
	log files to subversion. You can add them by using the following command:
		svn add --depth=infinity logs/
	
	
	INPUTS:
	- One or more Master List project names (case-sensitive)
	- OR - "all" to process everything

	
	OUTPUTS:
	- Log files get sent to a logs/ subdirectory detailing the interactive 
	  output of the execution.
	
	
	EXAMPLE CALLS:
	- To call one specific Master List project:
	groovy authorshipConnect.groovy BEST
	
	- To call more than one Master List project:
	groovy authorshipConnect.groovy SHEBA SBI DC3
	
	- To process all the Master List projects:
	groovy authorshipConnect.groovy all


	ANALYTICS:
	find . -name 'log_201311*\.txt' -type f -exec grep -l "insert " {} \; -print | xargs grep -A 1 "insert " | wc -l
	find . -name 'log_201311*\.txt' -type f -exec grep -l "update " {} \; -print | xargs grep -A 1 "update " | wc -l

	Divide both results by 2 to estimate the number of inserts/updates for specific months.
*/
/* ************************************************************************* */

/* ******************************* IMPORTS ********************************* */
package master_list.author_check

@Grapes([
	@Grab(group='mysql', module='mysql-connector-java', version='5.1.22'),
	@GrabConfig(systemClassLoader=true)
])

import groovy.sql.*
import java.util.regex.*
/* ***************************** END IMPORTS ******************************* */

/* **************************** LOG FILE SETUP ***************************** */
def timeNow = new Date().format("yyyyMMddHHmmss")
def folder = new File("logs")
if (!folder.exists()) {
   folder.mkdirs()
}
File logFile = new File("logs/log_"+timeNow.toString()+".txt")

if ( !logFile.exists() ) {
	logFile.createNewFile()
	Runtime.getRuntime().exec("chmod go-w " + logFile.toString())
	Runtime.getRuntime().exec("chgrp ctm-dmg " + logFile.toString())
	
	printLog( logFile, 0, "> groovy ${this.class.getSimpleName()}.groovy " + (this.args).join(" ") + "\n" )
	printLog( logFile, 0, "Created log file:\t" + logFile.toString() )
}
/* ************************** END LOG FILE SETUP *************************** */

/* **************************** SQL FILE SETUP ***************************** */
File sqlFile = new File("sql_output.txt")

if ( !sqlFile.exists() ) {
	sqlFile.createNewFile()
	Runtime.getRuntime().exec("chmod go-w " + sqlFile.toString())
	Runtime.getRuntime().exec("chgrp ctm-dmg " + sqlFile.toString())
}
/* ************************** END SQL FILE SETUP *************************** */

/* ********************************* MAIN ********************************** */
printLog( logFile, 0, "  About to connect to Zinc and Master List databases..." )
// Connect to the Zinc and Master List databases
// Production databases
def z_sql = Sql.newInstance('jdbc:mysql://farskol.eol.ucar.edu/zith9', 'zithupdate', 'change-999', 'com.mysql.jdbc.Driver')
def ml_sql = Sql.newInstance('jdbc:mysql://farskol.eol.ucar.edu/dmg_merged_ml', 'mlview', 'st00p1d', 'com.mysql.jdbc.Driver')
// Final testing databases
//def z_sql = Sql.newInstance('jdbc:mysql://sferic-dev.eol.ucar.edu/zith9_dev', 'zithupdate', 'change-999', 'com.mysql.jdbc.Driver')
//def ml_sql = Sql.newInstance('jdbc:mysql://sferic-dev.eol.ucar.edu/dmg_merged_ml', 'dts-full', 'l@micbc', 'com.mysql.jdbc.Driver')
// Test databases
//def z_sql = Sql.newInstance('jdbc:mysql://ctm-dev.eol.ucar.edu/zith9_dev', 'zithupdate', 'change-999', 'com.mysql.jdbc.Driver')
//def ml_sql = Sql.newInstance('jdbc:mysql://ctm-dev.eol.ucar.edu/dmg_merged_ml', 'dts-full', 'l@micbc', 'com.mysql.jdbc.Driver')
z_sql.connection.autoCommit = false
ml_sql.connection.autoCommit = false
printLog( logFile, 0, "  Connected!\n" )


// Instantiate the project, archiveId, and author_pi variables to be called in the SQL loop.
def project = ""
def lastProject = ""
def archiveId = ""
def author_pi = ""
def authors = []
def orgs = []
def unknownAuthors = []
def recentUnknowns = []
ArrayList zSqlStmts = []
Map nicknames = new HashMap() // Pulled from a text file upon runtime to improve performance speed
def questionDatasets = []
File nicknamesFile = new File("nicknames.txt")
File unknownsFile = new File("unknowns.txt")
File qDsFile = new File("questionable_datasets.txt")

def unkStr = ""
if ( !unknownsFile.exists() ) {
	unknownsFile.createNewFile()
	Runtime.getRuntime().exec("chmod go-w " + unknownsFile.toString())
	Runtime.getRuntime().exec("chgrp ctm-dmg " + unknownsFile.toString())
} else {
	unknownsFile.eachLine { line -> unknownAuthors << line }
}

// Import the questionable_datasets.txt file to the qDsList List
def qDsStr = ""
if ( !qDsFile.exists() ) {
	qDsFile.createNewFile()
	Runtime.getRuntime().exec("chmod go-w " + qDsFile.toString())
	Runtime.getRuntime().exec("chgrp ctm-dmg " + qDsFile.toString())
} else {
	qDsFile.eachLine { line -> questionDatasets << line }
}

// Import the nicknames.txt file to the nicknames HashMap
def nickStr = ""
if ( !nicknamesFile.exists() ) {
	nicknamesFile.createNewFile()
	Runtime.getRuntime().exec("chmod go-w " + nicknamesFile.toString())
	Runtime.getRuntime().exec("chgrp ctm-dmg " + nicknamesFile.toString())
} else {
	nicknamesFile.eachLine { line -> nickStr += line + "\n" }
}

if (nickStr != "") {
	nicknames = Eval.me( nickStr ) // Convert the nicknames file to the HashMap
	
	/*
	nicknames.sort().each() { k, v ->
		printLog( logFile, 0, "${k}\t${v}\t${v.size()}" )
	}
	
	return
	*/
}



// Assume each input argument is a Master List project
def projects = []
if (this.args[0] == "all") {
	projects = fetchAllProjects(ml_sql)
/*
	projects.each { p ->
		println p
	}
	return
*/
} else {
	for (arg in this.args) {
		projects.add("'" + arg + "'")
	}
}

if (projects.size() <= 0) {
	printLog( logFile, 0, "\n  USAGE:    groovy authorshipConnect.groovy [Master List Project Name]..." )
	return
}
/*
else {
	println "\n" + projects.join(" ") + "\n"
	return
}
*/

printLog( logFile, 0, "  About to enter into the processing..." )
// If we made it this far, then at least one Master List project was provided.
def mlQuery = "select dp.project_id, d.dataset_id, d.author_pi from dataset d left join dataset_project dp on "
	mlQuery += "d.dataset_id=dp.dataset_id where dp.project_id in (" + projects.join(",") + ")"
	mlQuery += " and d.dataset_id not like 'ML.%'"						// Comment out this line if porting Master-List prefixes
	mlQuery += " order by dp.project_id, d.dataset_id"

ml_sql.eachRow(mlQuery) { row ->
	project = row[0]
	archiveId = row[1]
	author_pi = row[2]
	
	
	// Check if this row's project is the same as the previous row's project
	if (lastProject != project) {
		// Check if there ever was a previous row
		if (lastProject != "") {
			// Send the list of SQL commands to the transaction method to execute
			executeSqlTransaction(z_sql, zSqlStmts, logFile, sqlFile)
			
			// Clear out the list of SQL commands for this project
			zSqlStmts.clear()
		
			printLog( logFile, 0, "\n--------------------------------------------------------------------------------" )
			printLog( logFile, 0, "--------------------------------------------------------------------------------\n${project}" )
			printLog( logFile, 0, "--------------------------------------------------------------------------------" )
			printLog( logFile, 0, "--------------------------------------------------------------------------------\n" )
		}
		
		// Set the previous row's project to be this project (for the next iteration)
		lastProject = project
	}
	
	
	// Prepare the author list for checking against existing authors in Zinc data sets
	(authors, orgs) = prepareAuthorList(author_pi)
	
	/*
	printLog( logFile, 0, "  ${project}:  ${archiveId}\n\t\t${author_pi}\n" )
	authors.each { a ->
		printLog( logFile, 0, "\t${a}" )
	}
	printLog( logFile, 0, "-----------------\n" )
	*/
	
	def contactOptions = ""
	def datasetAuthors = [authors.size()]
	def answer = ""
	def otherOpt = 0
	def i = 0
	// Check author list against contact table in Zinc
	authors.each { a ->
		// Check the recently answered unknowns list for a previously marked "unknown" contact
		def unknownResults = recentUnknowns.findAll { it == a }
		def isOrgFlag = isOrg(orgs, a)
		
		if (unknownResults.size() > 0 ) {
			datasetAuthors[i] = [0, i] // For the sort-key search
		} else {
	
			// Check the nicknames HashMap for an previously picked contact
			def nicknameResults = findAuthorByNickname(nicknames, a) // Returns a collection of objects with parameters id and name?
			
			if ( nicknameResults.size() == 1 ) {
				// Now we have confirmed the author, so let's add it to the datasetAuthors array
				datasetAuthors[i] = [nicknameResults.get(0).toInteger(), i]
				
			} else if ( nicknameResults.size() > 1 ) {
				// Now we have confirmed more than one author, so we need to still choose
				
				// NOTE: One of the nicknames cannot be NCAR-EOL id 954.
				if (nicknameResults.indexOf(954) != -1) {
					nicknameResults.remove( nicknameResults.indexOf(954) )
				}
				if ( nicknameResults.size() == 1 ) {
					datasetAuthors[i] = [nicknameResults.get(0).toInteger(), i]
				} else if ( nicknameResults.size() > 1 ) {
		
					a = a.replaceAll(/(?!\\)"/, "'")
			
					def output = "--------------------------------------------------------------------------------\n"
					output += "  ${project}:  ${archiveId}\n\t\t${author_pi}\n"
					output += "Which of these known contacts matches this author:\t" + a + "\n\n"
					def ansOpts = []
                                        def invalid = 1
					
					ansOpts.add(0)
					
					def zsql = "select id, short_name, person_name, organization_name from contact where id in ("+ nicknameResults.join(',') +")"
					if (isOrgFlag) { zsql += " and primary_name = 'org'" }
					zsql += " order by id"
					
					z_sql.rows( zsql ).each { c ->
						output += "\t\t" + c[0] + "\t" + c[1] + "\t" + c[2] + "\t" + c[3] + "\n"
						ansOpts.add(c[0])
					}
					
					output += "\nEnter zero to select none of the above or enter your own contact id.\n"
					output += "--------------------------------------------------------------------------------\n> "
					
					//printLog( logFile, 0, output )
					answer = ""
					otherOpt = 0
					while ( ansOpts.findAll{it.toString() == answer}.size() == 0 && otherOpt == 0 ) {
						answer = System.console().readLine(output)
                                                if (answer.isNumber()) {
                                                   invalid = 0
                                                }
						printLog( logFile, 1, output + "${answer}\n" )
						
						if ( ansOpts.findAll{it.toString() == answer }.size() == 0 ) {
							otherOpt = 0
                                                        if (! invalid) {
							   otherOpt = fetchContact(logFile, z_sql, answer) // Check if input was a valid id
							}
							if ( otherOpt == 0 ) {
								printLog( logFile, 0, "${answer} is not a valid input.\n" )
                                                                invalid = 1
							}
						}
					}
					
					if (answer != "0") {
						addKnown(datasetAuthors, unknownAuthors, questionDatasets, answer, a, i, archiveId)
					} else {
						addUnknown(unknownAuthors, recentUnknowns, questionDatasets, a, archiveId)
						datasetAuthors[i] = [0, i] // For the sort-key search
					}
				}
				
			} else {
				// Otherwise, query zith9 to find a contact to choose from
				//println "['${a}']"
				def ansOpts = [] 
                                def prompt = ""
                                def invalid = 1
                                def blanks = []
                                def putout = ""
				(contactOptions, ansOpts) = chooseContact(logFile, a, z_sql, isOrgFlag )
				
				if (contactOptions != "") {
					//printLog( logFile, 0, contactOptions )
					
					answer = ""
					otherOpt = 0
                                        if (ansOpts.size() == 2) {
                                           prompt = ansOpts[1].toString()
                                        } 
					while ( ansOpts.findAll{it.toString() == answer}.size() == 0 && otherOpt == 0 ) {
					
						def output = "--------------------------------------------------------------------------------\n"
						output += "  ${project}:  ${archiveId}\n\t\t${author_pi}\n"
						output += "Which of these contacts matches this author:\t" + a + "\n\n"
			
// If only one contact to choose from, enter the contact id on the line printed on the screen.
// If nothing entered, then use the one contact id as the default
// If there are 3 or more blanks in a name, then notify user there could be an error in the ML for the author entry.
// Don't show the default if there is a possible error in the ML
	
                                                blanks = a.findAll(" ")
                                                if (a[a.size()-1] == " ") {
                                                   blanks.remove(blanks.size()-1)
                                                }
                                                if (blanks.size() >= 3) {
                                                   output += "There may be an error in the Master List for this dataset.\n Please check that the first author is lastname, firstname.\n\n"
                                                   putout = output + contactOptions	
                                                } else {
                                                   putout = output + contactOptions + prompt
                                                }
						answer = System.console().readLine(putout)
                                                if (answer.isAllWhitespace()) {
                                                   answer = prompt
                                                }
                                                if (answer.isNumber()) {
                                                   invalid = 0
                                                }
						printLog( logFile, 1, output + contactOptions + "${answer}\n" )
						
						if ( ansOpts.findAll{it.toString() == answer }.size() == 0 ) {
							otherOpt = 0
                                                        if (! invalid) {
							   otherOpt = fetchContact(logFile, z_sql, answer) // Check if input was a valid id
                                                        }
							
							if ( otherOpt == 0 ) {
								printLog( logFile, 0, "${answer} is not a valid input.\n" )
                                                                invalid = 1
							}
						}
						
					}
					
					if (answer != "0") {
						addNickname(nicknames, answer, a)
						addKnown(datasetAuthors, unknownAuthors, questionDatasets, answer, a, i, archiveId)
					} else {
						addUnknown(unknownAuthors, recentUnknowns, questionDatasets, a, archiveId)
						datasetAuthors[i] = [0, i] // For the sort-key search
					}
				} else {
					// Didn't find a contact, but the user may know of a specific contact ID that does exist.
					def uStr = "--------------------------------------------------------------------------------\n"
					uStr += "  ${project}:  ${archiveId}\n\t\t${author_pi}\n"
					uStr += "Unable to find a contact ID for author:\t\t" + a + "\n"
                                        blanks = a.findAll(" ")
                                        if (a[a.size()-1] == " ") {
                                           blanks.remove(blanks.size()-1)
                                        }
                                        if ( blanks.size() >=3 ) {
                                           uStr += "There may be an error in the Master List for this dataset.\n Please check that the first author is lastname, firstname.\n\n"
                                        }
                                        invalid = 1
                                        def isValid = 0
					uStr += "Please enter in a contact ID for this author, or enter zero if unknown.\n>  "
                                        while (invalid) {
					   answer = System.console().readLine(uStr)
					   printLog( logFile, 1, uStr + "${answer}\n" )
                                           if (answer.isNumber()) {
                                              invalid = 0
                                           } else {
                                              printLog( logFile, 0, "${answer} is not a valid input.\n" )
                                              invalid = 1
                                           }
                                           if (! invalid) {
					      isValid = fetchContact(logFile, z_sql, answer) // Check if input was a valid id
                                           }
                                        }
					if ( isValid != 0 ) {
						addNickname(nicknames, answer, a)
						addKnown(datasetAuthors, unknownAuthors, questionDatasets, answer, a, i, archiveId)
					} else {				
						addUnknown(unknownAuthors, recentUnknowns, questionDatasets, a, archiveId)
						datasetAuthors[i] = [0, i] // For the sort-key search
					}
				}
			}
		}
		
		i += 1 // Increment to the next author
	}
	
	// Build the query to refer to in the sql call
	def query = "select contact_id, sort_key, dataset_id from dataset_contact, dataset, "
	query += "contact where dataset_id=dataset.id and contact_id=contact.id and "
	query += "iso_citation_role = 'author' and dataset.archive_ident = "
	
	// Retreive all the authors for a specified data set
	def aRows = z_sql.rows(query + "\"" + archiveId + "\" order by sort_key")
	
	i = 0 // Reset the counter to zero for the datasetAuthors loop
	while ( i < datasetAuthors.size() ) {
		// The sort_key order of authors is (first = 1, second = 2, third = 3, etc.)
		
		if ( datasetAuthors[i] != null && datasetAuthors[i][0] != 0 ) {
		
			// Check author list against existing authors in Zinc data set
			if (  aRows.find{ it[0].toInteger() == datasetAuthors[i][0].toInteger() }  ) {
				def dcInstance = aRows.find{ it[0].toInteger() == datasetAuthors[i][0].toInteger() }
				def contactId = dcInstance[0].toInteger()
				def sortKey = dcInstance[1].toInteger()
				def datasetId = dcInstance[2].toInteger()
				
				// Okay, we found the author. But is it in the correct order?
				if (  contactId == datasetAuthors[i][0].toInteger()  &&  sortKey != datasetAuthors[i][1]+1  ) {
					// Update the sort_key for this dcInstance
					printLog( logFile, 0, "Found contact ${datasetAuthors[i][0]} for data set ${archiveId} with sort_key ${sortKey} (instead of ${datasetAuthors[i][1]+1}). Will update sort_key." )
					def updateStatement = "update dataset_contact "
					updateStatement += "set sort_key = ${datasetAuthors[i][1]+1} "
					updateStatement += "where iso_citation_role = 'author' and dataset_id = ${datasetId} and contact_id = ${contactId}"
					
					//printLog( logFile, 0, updateStatement )
					//printLog( logFile, 1, "Updating dataset_contact instance (dataset_id=${datasetId}, contact_id=${contactId}) sort_key from ${sortKey} to ${datasetAuthors[i][1]+1}" )
					
					// Add the insert statement to the list of statements to be executed
					zSqlStmts << updateStatement
					
				}
				
			} else {
				// We didn't find the author, so we need to add it to dataset_contact
				printLog( logFile, 0, "Unable to find contact ${datasetAuthors[i][0]} for data set ${archiveId}. Will insert contact." )
				
				def insertStatement = "insert into dataset_contact (dataset_id, contact_id, iso_citation_role, sort_key, row_create_time) select "
				insertStatement += "d.id, ${datasetAuthors[i][0]}, 'author', ${datasetAuthors[i][1]+1}, CURRENT_TIMESTAMP "
				insertStatement += "from dataset d where d.archive_ident = '${archiveId}'"
				
				//printLog( logFile, 0, insertStatement )
				//printLog( logFile, 1, "Inserting into dataset_contact: (contact_id=${datasetAuthors[i][0]}, sort_key=${datasetAuthors[i][1]+1}) for data set ${archiveId}" )
				
				// Add the insert statement to the list of statements to be executed
				zSqlStmts << insertStatement
				
			}
			
			
			// Remove authors from data set in Zinc that are not actually authors
			/*
			z_sql.connection.autoCommit = false
			z_sql.withTransaction{ stmt ->
				z_sql.execute()
			}
			*/
			
		}
		
		i += 1 // Increment to the next author
	}
	
}
// Send the final list of SQL commands to the transaction method to execute
executeSqlTransaction(z_sql, zSqlStmts, logFile, sqlFile)

// Clear out the list of SQL commands for the last project
zSqlStmts.clear()


printLog( logFile, 0, "  Processing complete.\n  Writing the author nicknames to file..." )


// Export the nicknames map back to the nicknames.txt file
def nKeys = nicknames.sort( { k1, k2 -> k1 <=> k2 } as Comparator )*.key
nicknamesFile.withWriter { out ->
	out.writeLine("[")
//	printLog( logFile, 0, "[" )
	
	def counter = 0
    nKeys.each() { key ->
    	if (key != "0") {
			def valList = nicknames.get(key.toInteger())
			
			if ( (valList.join("@@") =~ /(?!\\)'/).asBoolean() ) {
				def v = valList.join("@@")
				v = v.replaceAll(/(?!\\)'/, /\\'/)
				valList = v.split("@@")
			}
			
    		def valStr = "['" 
    		valStr += valList.join("','")
    		valStr += "']"
    		
			def lineVal = "${key}:${valStr}"
			if (counter < nicknames.size()-1) {
				lineVal += ","
			}
			
			out.writeLine(lineVal)
//			printLog( logFile, 0, lineVal + "\t\t${counter} of ${nicknames.size()-1}" )
        }
        
        counter += 1
    }
    
    out.writeLine("]")
//    printLog( logFile, 0, "]" )
}
printLog( logFile, 0, "  Writing complete.\n  Writing the questionable datasets to file..." )


// Export the questionable datasets list back to the questionable_datasets.txt file
questionDatasets.unique()
qDsFile.withWriter { out ->
	questionDatasets.each() { qd ->
		out.writeLine(qd)
	}
}
printLog( logFile, 0, "  Writing complete.\n  Writing the unknown authors to file..." )


// Export the nicknames map back to the nicknames.txt file
unknownAuthors.unique()
unknownsFile.withWriter { out ->
	unknownAuthors.each() { ua ->
		out.writeLine(ua)
	}
}
printLog( logFile, 0, "  Writing complete.\n  Execution complete." )
println "\nLOG FILE:\t\t${logFile.absoluteFile.toString()}\n"



/*
// For ML-DB side cleanup
mlQuery = "select dp.project_id, d.dataset_id, d.author_pi from dataset d left join dataset_project dp on d.dataset_id=dp.dataset_id where dp.dataset_id in "
mlQuery += "(\"" + questionDatasets.join("\",\"") + "\")"
mlQuery += " order by dp.project_id, d.dataset_id;"

println "\n\n\n${mlQuery}\n"
*/

/* ******************************* END MAIN ******************************** */

/* ******************************* METHODS ********************************* */
def addKnown(datasetAuthors, unknownAuthors, questionDatasets, cId, a, i, archiveId) {
	datasetAuthors[i] = [cId, i]
							
	// Check for this nickname in the unknownAuthors list
	if (unknownAuthors.indexOf(a) != -1) {
		// Try to remove this nickname from the unknownAuthors list
		unknownAuthors.remove(unknownAuthors.indexOf(a))
		unknownAuthors.sort()
	}

	// Check for this dataset in the questionDatasets list
	if (questionDatasets.indexOf(archiveId) != -1) {
		// Try to remove this dataset from the questionDatasets list
		questionDatasets.remove(questionDatasets.indexOf(archiveId))
		questionDatasets.sort()
	}
}

def addNickname(nicknames, answer, a) {
	def nList = []
	if ( nicknames.get(answer.toInteger()) != null ) {
		nicknames.get(answer.toInteger()).each() { nList.add(it) }
	}
	nList.add(a) // Adds the nickname to the chosen author collection (nList)
	nicknames.put(answer.toInteger(), nList) // Adds the nList collection to the nicknames HashMap
}

def addUnknown(unknownAuthors, recentUnknowns, questionDatasets, a, archiveId) {
	// Add the unknown author to the list of contacts to be added
	unknownAuthors.add(a)
	unknownAuthors.sort()
	recentUnknowns.add(a)
	recentUnknowns.sort()
	questionDatasets.add(archiveId)
	questionDatasets.sort()
}

def chooseContact(logFile, author, z_sql, isOrgFlag) {
	def outString = ""
	
	author = author.replaceAll(/(?!\\)"/, "'")
	author = author.replaceAll(/\./, "%") // Replaces all periods with a wild card.
	
	def cQuery = "select id, short_name, person_name, organization_name from contact where short_name like \"%"+author+"%\" or person_name like \"%"+author+"%\" or organization_name like \"%"+author+"%\""
	if (isOrgFlag) { cQuery += " and primary_name = 'org'" }
	cQuery += " order by id"
	
	def cRows = z_sql.rows(cQuery)
	def cIds = []
	
	// If no contacts were found, modify the author to look up only the last name
	if (cRows.size() == 0) {
		def nameList = author.split(/[\s]+/).toList()
		
		cQuery = "select id, short_name, person_name, organization_name from contact where short_name like \"%"+nameList[nameList.size()-1]+"%\" or person_name like \"%"+nameList[nameList.size()-1]+"%\" or organization_name like \"%"+nameList[nameList.size()-1]+"%\""
		if (isOrgFlag) { cQuery += " and primary_name = 'org'" }
		cQuery += " order by id"
		
		cRows = z_sql.rows(cQuery)
	}
	
	if ( cRows.size() == 0 || (author =~ /^\s*$/).asBoolean() ) {
		// If no contacts match now, then return an empty string
		author = author.replaceAll(/%/, ".") // Replaces all wild cards with a period.
		printLog( logFile, 1, "\n--------------------------------------------------------------------------------\n"	 )	
		printLog( logFile, 1, "No contacts match this author:\t" + author + "\n" )
		printLog( logFile, 1, "--------------------------------------------------------------------------------\n" )
		return ["", cIds]
	}
	
	cIds.add(0)
        
	while (cRows.size() > 0) {
		def cRow = cRows.get(0)
		outString += "\t\t" + cRow[0] + "\t" + cRow[1] + "\t" + cRow[2] + "\t" + cRow[3] + "\n"
		
		cIds.add(cRow[0])
		
		cRows.remove(0)
	}
	
	outString += "\nEnter zero to select none of the above (Will not select default if zero is entered).\n"
	outString += "--------------------------------------------------------------------------------\n> "
	
	return [outString, cIds]
}

def executeSqlTransaction(z_sql, sqlList, logFile, sqlFile) {
	def answer
	def executePrompt = ""
	
	// If the sqlList is empty, then just return
	if (sqlList.size() == 0) {
		return
	}

	executePrompt += "\n"
	sqlList.each{
		executePrompt += "${it}\n"
	}
	executePrompt += "\n\nExecute the SQL statements above? (y/n)\n"
	executePrompt += "--------------------------------------------------------------------------------\n> "
	
	// Prompt the user if we should execute these SQL commands
	answer = System.console().readLine(executePrompt)
	
	// While the answer is neither y nor n...
	while ( (answer =~ /^[yn]$/).asBoolean() == false ) {
		// Prompt the user again
		answer = System.console().readLine(executePrompt)
	}
		
	// If the user says okay, then call the try-catch-finally
	if ( answer == "y" ) {
		//println "\nYou typed:\t${answer}"
		
		/*
		sqlFile.withWriterAppend { out ->
			sqlList.each { s ->
				out.writeLine(s+";")
			}
			out.writeLine("\n")
		}
		*/
		
		/**/
		try {
			z_sql.withTransaction {
				sqlList.each {
					printLog( logFile, 1, it+";\n" )
					z_sql.execute(it)
				}
			}
			z_sql.commit()
		} catch (Exception se) { // Catches SQLException
			printLog( logFile, 0, se+"\n\n" )
			z_sql.rollback()
		} finally { }
		/**/
	} else {
		//println "\nYou typed:\t${answer}"
		printLog( logFile, 0, "\nSkipping over SQL statements.\n" )
	}

}

def fetchAllProjects(ml_sql) {
	def projects = []
	def pQuery = "select project_id from project"
	
	ml_sql.eachRow(pQuery) { row ->
		projects.add("'" + row[0] + "'")
	}
	
	return projects
}

def fetchContact(logFile, z_sql, contactId) {
	if (contactId == 0) { return 0 } // Immediately exit this if the feedback is zero.
	
	def cQuery = "select id, short_name, person_name, organization_name, email from contact where id = " + contactId
	def cRows = z_sql.rows(cQuery)
	def outString = ""
	def response
	
	if (cRows.size() > 0) {
		def cRow = cRows.get(0)
		outString += "\t\t" + cRow[0] + "\t" + cRow[1] + "\t" + cRow[2] + "\t" + cRow[3] + "\t(" + cRow[4] + ")\n"
		outString += "\nIs this the author? (y/n)"
		outString += "  > "
	
		response = System.console().readLine(outString)
		printLog( logFile, 1, outString + response )
		
		if ( (response =~ /^[yY]$/).asBoolean() ) {
			return cRow[0]
		}
	}
	
	return 0
}

def findAuthorByNickname(nicknameHashMap, nickname) {
	def authorList = []
	
	authorList = nicknameHashMap.findAll{ k ->
		(k.value).findAll{ it == nickname }
	}.collect{it.key}
	
	//if (authorList.size() == 0) { println "Didn't find ${nickname}." }
	
	return authorList
}

def isOrg(orgs, author) {
	def orgList = []
	def flag = false
	
	orgList = orgs.findAll{ it == author }.collect{it}
	
	if (orgList.size() > 0) { flag = true }
	
	return flag
}

def prepareAuthorList(auth_pi) {

	auth_pi = auth_pi.replaceAll(/[\s]+et[\.]* al[\.]*/, "") // Replace all "et al." with nothing
	auth_pi = auth_pi.replaceAll(/(?<=,)\s+and\s+/, " ") // Replace all ", and " with ", "
	auth_pi = auth_pi.replaceAll(/\s+and\s+/, ", ") // Replace all " and " with ", "
	
	def orgs = auth_pi // Save all (...) as orgs list
	auth_pi = auth_pi.replaceAll(/\s*\(.*\)\s*/, "") // Replace all (...) with nothing
	
	if ( (orgs =~ /\s*\(.*\)\s*/).asBoolean() ) {
		orgs = orgs.replaceAll(/^.*\(/, "") // Replace all pre-( with nothing
		orgs = orgs.replaceAll(/\)\s*$/, "") // Replace all post-) with nothing
	} else {
		orgs = ""
	}
	//def orgList = orgs.split(/[(,)|\/]\s*/).toList()
	def orgList = orgs.split(/[(,)]\s*/).toList()
	
	if ( (auth_pi =~ /[\w.][^,]\s+and\s+/).asBoolean() ) {
		// Replace all " and " with ", "
		auth_pi = auth_pi.replaceAll(/(?<!,)\s+and\s+/, ", ")
	}
	
	// Split out this line for searching individual terms
	def authList = auth_pi.split(/[,][\s]*/).toList()
	//printLog( logFile, 0, "[" + authList.join('][') + "]" )
	//println "[" + authList.join('][') + "]"

	// Special handling for a Lastname, Firstname and Firstname Lastname
	if ( authList.size() == 2 && (auth_pi =~ /[\s]+and[\s]+/).asBoolean() ) {
		auth_pi = auth_pi.replaceAll(/[\s]+and[\s]+/, ", ")
		authList = auth_pi.split(/,[\s]*/).toList()
	}
	
	//Special handling for the organizations/PIs joined by a "/"
	if ( (auth_pi =~ /\//).asBoolean() && ( !(auth_pi =~ /NCAR[\/\-]/).asBoolean() && !(auth_pi =~ /[\/\-]RAF/).asBoolean() && !(auth_pi =~ /EOL[\/\-]/).asBoolean() ) ) {
		if ( (auth_pi =~ /\//).asBoolean() ) {
			// Special handling for a PIname/Organization
			//auth_pi = auth_pi.replaceAll(/\//, " and ")
			//authList[0] = auth_pi
			
			def tmpList = auth_pi.split(/\//)
			authList.clear()
			tmpList.each { t ->
				if ( (t =~ /[,][\s]*/).asBoolean() ) {
					def tList = t.split(/[,][\s]*/)
					authList.add(tList[1] + " " + tList[0])
				} else { authList.add(t) }
			}
			//println "[" + authList.join('][') + "]"
		}
	}

	//Special handling for the first author listed (originally LastName, FirstName)
	else if ( authList.size() > 1 ) { 
		def firstAuthor = authList[1] + " " + authList[0]
		authList.remove(0)
		authList.remove(0)
		authList.add(0,firstAuthor)
	}
	
	//printLog( logFile, 0, "[" + authList.join('][') + "]" )
	
	if (orgList.size() > 0 && !(orgList.size() == 1 && orgList[0] == "")) {
		authList = authList.plus(orgList)
		if (authList[0] == "") { authList.remove(0) }
	}
	//println "[" + authList.join('][') + "]"
	//println "[" + orgList.join('][') + "]"
	return [authList, orgList]
}

def printLog(logFile, logOnly, str) {
	// logOnly == 0 if you wish to print to the screen.
	// logOnly == 1 if you wish to only log the string.

	if (logOnly != 1) {
		println str
	}

	logFile.withWriterAppend { out ->
		out.writeLine(str)
	}

}
/* ***************************** END METHODS ******************************* */
