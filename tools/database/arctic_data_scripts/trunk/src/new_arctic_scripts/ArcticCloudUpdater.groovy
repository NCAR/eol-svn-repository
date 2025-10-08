/*
ArcticCloudUpdater.groovy
Checks and updates the arctic cloud staging area specified in the StagingUtils class
for the projects enumerated in arctic_projects

Dependencies:
  groovy-2.3.9 or greater (and its dependencies)
  mysql connector java 5.1.34 or greater
    you will need to add mysql-connector-java-[version#]-bin.jar to your classpath, either
    through an environment variable or with the -cp flag with the groovy command.
    This can be downloaded at: http://dev.mysql.com/downloads/connector/j/
    At the time of this writing, there is also a copy of mysql-connector-java-5.1.34-bin.jar
    in /net/work/lib

Syntax: groovy ArcticCloudUpdater.groovy [options] [project name(s)]
-a   applies flags to all supported Arctic projects in the staging area:
     /scr/ctm/cloud/s3_staging
     Default is to update localhost with prompt for mass store if other
     flags are not supplied.
-c   check: prints a summary of which datasets need to be updated without
     adding or removing files to the cloud staging area
-f   if updating, do not prompt to update mass store datasets
-h   display usage information
-u   updates datasets with localhost files. Will prompt for datasets that
     have files on the mass store
-v   display extra messages

Example use:
  ArcticCloudUpdater -ua
    updates all supported projects in the staging area
  ArcticCloudUpdater -cv PROJECT1 PROJECT2
    checks the contents of the PROJECT1 and PROJECT2 directories in the cloud
    staging area. Prints which dataset it is currently checking and a summary
    for both projects of missing and extra files when done.

Capabilities:
  Can check and update files for local, HPSS, and versioned datasets
  Can recreate the directory structure of the staging area from scratch if 
    StagingUtils.staging_root is changed
  Will prompt users before copying HPSS datasets, allowing users to update or skip updating for
    current dataset only or for that and any other HPSS datasets that may crop up.

Does not:
  Copy the ISO xml metadata into the dataset directories
  Regenerate the arctic manifest if files have been copied/removed
  Upload anything to the Amazon S3 bucket
                                                     
20 May 15: created by Morgan Garske                                 
*/

import groovy.sql.*
import groovy.io.FileType
import java.nio.file.*

/*
StagingUtils is a static class that holds various variables and methods that need to be able to be accessed publicly at any time.
*/
final class StagingUtils {

	// Map of supported projects to their corresponding id in the Zith9 database. To add a supported project, you will need to add the project
	// and its ID to this mapping. You will also need to update the arctic_project_archive_prefix map, which maps the name of the supported project
	// to a list of numerical prefixes for the archive identifiers belonging to the project datasets. Failing to update BOTH when adding a new project
	// can result in unexpected behavior
	private static final def arctic_projects = ['AMTS':240,'ARC-MIP':108,'ARCSS':100,'ATLAS':103,'BARROW':198,'BASE':308,'BEST':343,'BSIERP':342,'BOREAS':312,'ITEX':102,'PacMARS':364,'SBI':71,'SHEBA':73,'DBO':481]
	public static final def arctic_project_archive_prefix = ['AMTS':['215'],'ARC-MIP':['61'],'ARCSS':['106'],'ATLAS':['46'],'BARROW':[],'BASE':['230'],'BEST':['102'],'BSIERP':['245'],'BOREAS':['234', '413'],'ITEX':['56'],'PacMARS':['255'],'SBI':['62'],'SHEBA':['13'],'DBO':['481'] ]
	
	// The location of the cloud staging area. Change this variable if you want to create a staging area elsewhere.
	private static String staging_root = "/scr/ctm/cloud/s3_staging"
	
	static String username = 'zithview', password = 'look-999', database = 'zith9', host = 'farskol.eol.ucar.edu'
	public static boolean update_all_hpss = false // if true, program will not prompt user to update hpss datasets
	public static boolean skip_hpss = false // if true, ignore all hpps datasets when updating
	public static boolean verbose = false // if true, display extra info in various parts of the update process

	private StagingUtils(){} //private constructor - no need to instantiate this class

	public static Sql getDatabaseHandle()
	{
		Sql db = Sql.newInstance("jdbc:mysql://$host/$database", username, password, 'com.mysql.jdbc.Driver')
		return db
	}

	public static String getStagingRoot() {
		return staging_root
	}

	// creates nicely formatted list of the supported projects for use in usage information and error messages
	public static String supported_project_list()
	{
		Collection projects = arctic_projects.keySet().sort()
		return "${projects[0..projects.size()-2].join(', ')}, and ${projects[projects.size()-1]}"
	}

	public static boolean isSupportedProject(String project_name) {
		return (boolean) arctic_projects[project_name]
	}

	public static int getProjectID(String project_name) {
		return arctic_projects[project_name]
	}

	public static Collection getProjectList() {
		return arctic_projects.keySet().sort()
	}

	public static Collection getProjectPrefixes(String project_name) {
		return arctic_project_archive_prefix[project_name]
	}

}

/* 
CloudProject represents a codiac project on the cloud. This class handles the database calls to zith9
and creates and holds its own datasets
*/
class CloudProject{
	String project_dir
	int project_id
	String project_name
	Sql db
	LinkedHashMap datasets

	public CloudProject(String project_name)
	{
		if (!StagingUtils.isSupportedProject(project_name)) throw new IllegalArgumentException("$project_name not a supported project. Only ${StagingUtils.supported_project_list()} currently supported.")
		this.project_name = project_name
		project_id = StagingUtils.getProjectID(project_name)

		// BEST and BSIERP have a slightly different directory structure since they both belong to the BeringSea project
		if (project_name in ['BEST','BSIERP']) project_dir = "BeringSea/$project_name" 
		// The data for most projects will just be in a directory named after the project
		else project_dir = project_name

		db = StagingUtils.getDatabaseHandle()
		datasets = [:]
	}

	public def getProjectDir() {
		return project_dir
	}
	
	// prints out the list of archived filepath -> cloud filepath
	public void printDataset(String archive_ident) {
		datasets[archive_ident].printFiles()
	}

	// adds the dataset to the dataset map, and sets the files for the dataset from the database
	public void addDataset(String archive_ident) {

		// Create an essentially empty dataset and store a reference
		def dataset = new CloudDataset(archive_ident, this)
		datasets[archive_ident] = dataset

		// Setting the files for the dataset will actually add the files
		// to the dataset. The strategy for doing this depends on if the
		// dataset is versioned or not
		if (isVersioned(dataset)) {
			setVersionedDatasetFiles(dataset)
		}
		else {
			setDatasetFiles(dataset)
		}
		dataset.setCloudPaths()
		
	}

	public void updateDataset(String archive_ident) {
		datasets[archive_ident].updateDataset()
	}

	// Adds files to the specified dataset from the database
	private void setDatasetFiles(CloudDataset dataset)
	{
		def archive_ident = dataset.getArchiveIdent()
		Sql db = StagingUtils.getDatabaseHandle()
		db.eachRow("select CONCAT(directory, '/', filename) as path, purpose, host from file join dataset on dataset.id = file.dataset_id where file.visible=1  and archive_ident = $archive_ident;") { row ->
			dataset.addCloudFile(row.path, row.purpose, row.host)
		}
		db.close()
	}

	// Adds versioned files to a dataset. If the dataset does not contained versioned
	// files per the codiac database, no files will be added.
	private void setVersionedDatasetFiles(CloudDataset dataset)
	{
		def archive_ident = dataset.getArchiveIdent()
		Sql db = StagingUtils.getDatabaseHandle()
		db.eachRow("select version_number, CONCAT(directory, '/', filename) as path, purpose, host from dataset join dataset_version on dataset_id = dataset.id join dataset_version_file on dataset_version_id = dataset_version.id join file on file.id = file_id where archive_ident = $archive_ident;") { row ->
			// adding a cloud file uses a different constructor for the cloud file object, as it needs to 
			// keep track of a version number for puposes of constructing the staging area path.
			dataset.addCloudFile(row.path, row.purpose, row.host, row.version_number)
		}
		db.close()
	}

	// checks to see if dataset has at least one versioned file. A dataset may have versions,
	// but if it doesn't have any versioned files, it shouldn't be treated as a versioned
	// dataset for the purposes of this program
	private boolean isVersioned(CloudDataset dataset)
	{
		def archive_ident = dataset.getArchiveIdent()
		boolean versioned = false
		Sql db = StagingUtils.getDatabaseHandle()
		db.eachRow("select file_id from dataset join dataset_version on dataset_id = dataset.id join dataset_version_file on dataset_version_id = dataset_version.id where archive_ident = $archive_ident limit 1;") { row ->
			versioned = true
		}
		db.close()
		return versioned
	}

	// performs an update on all datasets associated with the project
	public void updateProjectDatasets()
	{
		Sql db = StagingUtils.getDatabaseHandle()
		db.eachRow("select archive_ident from dataset join dataset_project on dataset.id = dataset_id where dataset.visible = 1 and project_id = $project_id;") {row ->
			String archive_ident = row.archive_ident
			if (archive_ident.tokenize('.')[0] in StagingUtils.getProjectPrefixes(project_name)) {
				this.addDataset(archive_ident)
				this.updateDataset(archive_ident)
			}
		}
		db.close()
	}

	// checks the datasets (does not copy or remove files) associated with the project
	// prints a summary for the datasets regarding missing and extraneous files
	public void checkProjectDatasets()
	{
		int num_missing_files = 0
		int num_extra_files = 0
		Collection localhost_datasets_to_update = []
		Collection hpss_datasets_to_update = []
		Sql db = StagingUtils.getDatabaseHandle()
		db.eachRow("select archive_ident from dataset join dataset_project on dataset.id = dataset_id where dataset.visible = 1 and project_id = $project_id;") {row ->
			String archive_ident = row.archive_ident
			if (archive_ident.tokenize('.')[0] in StagingUtils.getProjectPrefixes(project_name)) {
				this.addDataset(archive_ident)
				if (StagingUtils.verbose) print "Checking $project_name dataset: $archive_ident         \r"
				def errors = datasets[archive_ident].checkDataset()
				if (errors.missing != [] || errors.extra != []) {
					if (datasets[archive_ident].hasHPSSFiles()) hpss_datasets_to_update.add(archive_ident)
					else localhost_datasets_to_update.add(archive_ident)
					num_missing_files += errors.missing.size()
					num_extra_files += errors.extra.size()
				}
			}
		}

		if (StagingUtils.verbose) println ""
		String check_message = ""
		if (num_missing_files + num_extra_files > 0) {
			check_message += "\nProject: $project_name\n"
			if (num_missing_files > 0) check_message += "Missing files: $num_missing_files\n"
			if (num_extra_files > 0) check_message += "Extra files: $num_extra_files\n"
			check_message += "Datasets needing update: ${localhost_datasets_to_update.size() + hpss_datasets_to_update.size()}\n"
			if (localhost_datasets_to_update != []) check_message += "Datasets to update with local files only:\n${localhost_datasets_to_update.join(' ')}\n"
			if (hpss_datasets_to_update != []) check_message += "Datasets to update with files on HPSS:\n${hpss_datasets_to_update.join(' ')}\n"
		}
		else check_message += "All files for project $project_name up to date"

		println check_message
		db.close()
	}

}

// CloudDataset represents a dataset on the cloud. It keeps a reference to its project and contains
// and manages a list of all files that belong to it. It also handles deleting extra files from and copying
// missing files to the cloud staging area
class CloudDataset{
	Collection cloud_files
	CloudProject project
	String archive_ident
	Collection cloud_paths
	String root
	boolean is_versioned

	public CloudDataset(String archive_ident, CloudProject project) {
		this.cloud_paths = []
		this.cloud_files = []
		this.archive_ident = archive_ident
		this.project = project
		this.root = ""
		this.is_versioned = false
	}
	
	public String getArchiveIdent() {
		return archive_ident
	}

	// adds a standard cloud file to the dataset
	public void addCloudFile(String archive_path, String purpose, String host) {
		CloudFile file = new CloudFile(archive_path, purpose, host)
		cloud_files.add(file)
	}

	// adds a versioned cloud file to the datset
	public void addCloudFile(String archive_path, String purpose, String host, String version) {
		is_versioned = true
		CloudFile file = new CloudFile(archive_path, purpose, host, version)
		cloud_files.add(file)
	}

	// sets the path in the staging area where the file will be copied to (if necessary)
	// This is neccessary to give the cloud file instance extra information not available
	// at instantiation. Namely, the shared part of the path for the cloud files (root) can 
	// only be determined after the list of cloud files has been populated for the dataset.
	public void setCloudPaths() {
		this.setDatasetRoot()
		cloud_files.each { file ->
			file.setCloudPath(root, archive_ident, project.getProjectDir())
			cloud_paths.add(file.getCloudPath())
		}
	}

	// gets the list of files that are already present in the dataset directory in the staging area
	public def getExistingFiles()
	{
		def file_list = []
		String datasetPath = "${StagingUtils.getStagingRoot()}/${project.getProjectDir()}/$archive_ident"
		File datasetDir = new File(datasetPath)
		File docDir = new File(datasetPath + '/doc')
		File dataDir = new File(datasetPath + '/data')
		if (!datasetDir.exists()) datasetDir.mkdir()
		else if (!docDir.exists() && !is_versioned) docDir.mkdir()
		else if (!dataDir.exists() && !is_versioned) dataDir.mkdir()
		
		
		if (datasetDir.exists()){
			datasetDir.eachFileRecurse(FileType.FILES) { file ->
				if (!(file.getName() ==~ /${archive_ident}.+ISO\.xml/)) { //we want to skip the ISO XML metadata - it belongs in the staging area even though it isn't recorded in the database
					file_list << file.getAbsolutePath()
				}
			}
		}
		return file_list
	}

	// Gets the list of existing files and checks this list against the list of files that belong
	// to the dataset. Returns a map which contains the list of extra and missing files for the dataset
	public def checkDataset()
	{
		if (cloud_paths == []) this.setCloudPaths()
		def existingPaths = getExistingFiles()
		def missing = []
		def errors = [:]

		cloud_paths.each { cloud_path -> 
			if (!existingPaths.remove(cloud_path)) {
				missing.add(cloud_path)
			}
		}
		errors['missing'] = missing
		errors['extra'] = existingPaths
		return errors
	}

	// Determines the part of the path that is common to all files belonging to the dataset. This is done
	// so that the directory structure can be preserved when the files are copied over. Otherwise, all files
	// would be copied over into a flat directory, requiring either manual organization or making it hard for
	// to browse through datasets with a large number of files that would otherwise be divided into more
	// manageable directories. It also solves the issue of filename collisions.
	public void setDatasetRoot()
	{
		if (root == "") { // only set if it hasn't been already
			if (this.countDataFiles() == 1) { root = getSingleDataFile().getArchivePath().substring(0, getSingleDataFile().getArchivePath().lastIndexOf("/")+1) }
			else if (this.countDataFiles() != 0) {
				def paths = tokenizeCloudDataFilePaths()
				def tokenized_root = paths[0]
				// basic approach is to start with a path for one of the files and then chop it down until
				// all files will belong to a directory under that path
				paths.each { path ->
					if (tokenized_root.disjoint(path)){
						println('could not find common root for dataset $archive_ident')
						System.exit(-1)
					}
					def temp_root = []
					for (int i = 0; i < tokenized_root.size(); i++) {
						if (tokenized_root[i] == path[i]) temp_root << path[i]
						else break
					}
					tokenized_root = temp_root
				}
				root = '/' + tokenized_root.join('/') // set the root
			}
		}
	}
	
	private int countDataFiles() {
		int numDataFiles = 0
		cloud_files.each { file ->
			if (file.purpose == "data") numDataFiles ++
		}
		return numDataFiles
	}

	private CloudFile getSingleDataFile() {
		return cloud_files.find { it.purpose == "data" }
	}

	// create and return a list of lists of strings representing the cloud file paths
	private def tokenizeCloudDataFilePaths()
	{
		def tokenized_paths = []
		cloud_files.each { file ->
			if (file.purpose == "data") tokenized_paths.add(file.tokenizeArchivePath())
		}
		return tokenized_paths
	} 	

	// Copies missing files to the staging area. Works for both localhost and hpss
	private void copyMissingFiles()
	{

		def missing_files = checkDataset()['missing']
		if (!missing_files.isEmpty()){
			cloud_files.each { file ->
				def cloud_path = file.getCloudPath()
				def archive_path = file.getArchivePath()
				if (cloud_path in missing_files)
				{
					println("Copying $archive_path to $cloud_path")
					def destination = new File(cloud_path)
					if ( !destination.exists() ) {
						destination.getParentFile().mkdirs();
					}
					if (file.host == 'localhost') {
						Files.copy(new File(archive_path).toPath(), destination.toPath())
					}
					else if (file.host == 'hpss') {
						Process proc = Runtime.getRuntime().exec("hsi get ${destination.toPath()} : ${new File(archive_path).toPath()}")
						InputStream stderr = proc.getErrorStream()
						InputStreamReader isr = new InputStreamReader(stderr)
						BufferedReader br = new BufferedReader(isr)
						String line = null
						while ((line = br.readLine()) != null) println line
						int exitVal = proc.waitFor()
						if (exitVal != 0) {
							println "An error occured with HPSS. Check that the HPSS is online and try again later."
							println "Exit Value: $exitVal"
							System.exit(exitVal)
						}
					}
				}
			}
		}
	}

	// gets the list of files that do not belong in the staging area for this dataset and removes them
	private void deleteExtraFiles()
	{
		def extra_files = checkDataset()['extra']
		extra_files.each { file ->
			println("Removing extra file: $file");
			Files.deleteIfExists(Paths.get(file));
		}
	}
	
	public boolean hasHPSSFiles()
	{
		boolean hasHPSS = false
		cloud_files.any { file -> 
			if (file.host == 'hpss') {
				hasHPSS = true
			}
		}
		return hasHPSS
	}

	// Handles updating the dataset, including removing extra files and copying missing ones.
	// Will display a prompt if the dataset has hpss files, as these files can take a considerable
	// amount of time to transfer.
	public void updateDataset(){
		def updated = false
		def errors = checkDataset()
		def update_hpss = StagingUtils.update_all_hpss
		if (errors.extra != [] || errors.missing !=[]) {
			if ((!update_hpss && hasHPSSFiles()) && !StagingUtils.skip_hpss) {
				// if the hpss dataset only has extra files, we can just delete those without bugging the user
				if (errors.extra != [] && errors.missing == []) update_hpss = true
				else {
					String answer = System.console().readLine "Dataset $archive_ident has files on the HPSS and needs to be updated (${errors.missing.size()} missing file(s) and ${errors.extra.size()} extra file(s)).\nThis operation can take a very long time depending on the number and size of files that need to be transferred. Do you wish to update now? \nType (y/n) for this dataset or (ya/na) to apply this decision to this and subsequent datasets. \n(y|n|ya|na): "
					switch (answer) {
						case "y":
							update_hpss = true
							break
						case "n":
							update_hpss = false
							break
						case "ya":
							update_hpss = true
							StagingUtils.update_all_hpss = true
							break
						case "na":
							update_hpss = false
							StagingUtils.skip_hpss = true
							break
						default:
							println "Did not recognize input \"$answer\". Default to no update."
							break
					}
				}
			}	
			if (!hasHPSSFiles() || update_hpss) {
				if (errors.extra != []) { 
					deleteExtraFiles() 
					updated = true
				}
				if (errors.missing != []) { 
					copyMissingFiles() 
					updated = true
				}
				removeEmptyDirs(new File("${StagingUtils.getStagingRoot()}/${project.getProjectDir()}/$archive_ident"))
				if (!updated && StagingUtils.verbose) println("Dataset $archive_ident was already up to date")
				else println ("Dataset $archive_ident has been updated")
			}
		}
		else {
			if (StagingUtils.verbose) println("Dataset $archive_ident was already up to date")
		}
	}


	//Recursivelty removes empty directories below the given directory
	private void removeEmptyDirs(File dir) {
		File[] dirContents = dir.listFiles()
		
		dirContents.each { content ->
			if (content.isDirectory()) {
				removeEmptyDirs(content)
			}
		}

		dirContents = dir.listFiles()
		if (dirContents.length == 0) {
			String dataPath = "${StagingUtils.getStagingRoot()}/${project.getProjectDir()}/$archive_ident/data"
			String docPath = "${StagingUtils.getStagingRoot()}/${project.getProjectDir()}/$archive_ident/doc"
			if (!(dir.getAbsolutePath() in [dataPath, docPath])){
				Files.deleteIfExists(dir.toPath())
				println "deleting empty directory ${dir.getName()}"
			}
		}
	}


	// prints all the files in the dataset to stdout. 
	public void printFiles() {
		cloud_files.each { file ->
			println "${file.getArchivePath()} ->  ${file.getCloudPath()}"
		}
	}


}


// CloudFile represents a single file in the cloud staging area. It holds both its 
// archive path (the path listed in the database) in addition to its designated path
// in the cloud staging area. Cloud files can also be versioned files - this is the
// only place where the version is stored.
class CloudFile{

	String archive_path
	String cloud_path
	String purpose
	String host
	String version

	// standard constructor
	public CloudFile(String archive_path, String purpose, String host) {
		this.archive_path = archive_path
		if (purpose == "eula") { this.purpose = "doc" }
		else { this.purpose = purpose }
		this.host = host
		this.cloud_path = ""
		this.version = ""
	}

	// constructor for a versioned file (uses normal constructor and stores version)
	public CloudFile(String archive_path, String purpose, String host, String version) {
		this(archive_path, purpose, host)
		this.version = version
	}

	// get the path that is stored in the database
	public String getArchivePath() {
		return archive_path
	}

	// split the path up into a list of subdirectories (with the filename being the last item in the list)
	// used for finding the common root path.
	public def tokenizeArchivePath() {
		return archive_path.tokenize('/')
	}

	// given information from the dataset, sets the path where the file belongs in the cloud staging area
	public void setCloudPath(String root, String archive_ident, String project_dir) {
		if (purpose == "doc") {
			cloud_path = archive_path.substring(archive_path.lastIndexOf("/")+1)
		}
		else cloud_path = archive_path.replaceFirst("$root/?", "")
		if (version == "") {
			cloud_path = "${StagingUtils.getStagingRoot()}/$project_dir/$archive_ident/$purpose/$cloud_path"
		} else {
			cloud_path = "${StagingUtils.getStagingRoot()}/$project_dir/$archive_ident/v_$version/$purpose/$cloud_path"
		}
	}

	public String getCloudPath() {
		return cloud_path
	}

	public String getPurpose() {
		return purpose
	}

	// 'hpss' or 'localhost'
	public String getHost() {
		return host
	}
}



def cli = new CliBuilder(usage:'ArcticCloudUpdater [options] <project name>', header:"\nDescription: ArcticCloudUpdater is a utitily for updating\
 and managing the ACADIS Cloud staging area located at ${StagingUtils.getStagingRoot()}. It can update and check for updates for versioned and non-versioned datasets,\
 and for datasets with files on HPSS and localhost. It cannot update the ISO metadata located in each dataset directory or sync\
 the staging area with the Amazon S3 bucket. Please refer to the documentation located at https://internal.eol.ucar.edu/content/dmg-documentation-0\
 for instructions on how to perform these actions.\n\nSupported projects: " + StagingUtils.supported_project_list())
cli.u('updates datasets with localhost files. Will prompt for datasets that have files on the mass store')
cli.c('check: prints a summary of which datasets need to be updated without adding or removing files to the cloud staging area')
cli.f('if updating, do not prompt to update mass store datasets')
cli.a("applies flags to all supported Arctic projects in the staging area: ${StagingUtils.getStagingRoot()}\nDefault is to update localhost with prompt for mass store if other flags are not supplied.")
cli.h('display usage information')
cli.v('display extra messages')
def opt = cli.parse(args)


if (opt.v) StagingUtils.verbose = true
if (opt.f) StagingUtils.update_all_hpss = true
if(opt.h || (opt.arguments().isEmpty() && !opt.a)) {
	cli.usage()
}
else if (opt.a) {
	StagingUtils.getProjectList().each { project_name ->
		CloudProject project = new CloudProject(project_name)
		if (opt.c) project.checkProjectDatasets()
		else if (opt.u) project.updateProjectDatasets()
	}
}
else {
	if (opt.arguments().size > 0) {
		opt.arguments().each { project_name -> 
			if (project_name in StagingUtils.getProjectList()) {
				CloudProject project = new CloudProject(project_name)
				if (opt.c) project.checkProjectDatasets()
				else if (opt.u) project.updateProjectDatasets()
				else cli.usage()
			}
			else println "$project_name not a supported project. Use 'ArcticCloudUpdater -h' to see list of supported projects."
		}
	}
	else cli.usage()
}
