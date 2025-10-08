package dmg.ml.port;

import java.sql.*;
import java.util.*;
import java.util.regex.*;

/**
 * The ArcticGeneralMerge class is a self-contained program for porting the
 * <code>dmg_general_ml</code> and <code>dmg_arctic</code> Master List databases
 * into the <code>dmg_merged_ml</code>.  It reads in all information in each
 * database, reorganizes, merges, and inserts the data into the new database.  It
 * also generates some new classifications, renames classifications, and prevents
 * old classifications from being put into the new database.
 *
 * @author Joel Clawson
 **/
public class ArcticGeneralMerge {
  
  // Database Constants to use to establish connections
  private static final String username = "dts-full";
  private static final String password = "l@micbc";
  
  // URLS for the different databases to use.
  private static final String ARCTIC_DB = "jdbc:mysql://data.eol.ucar.edu/dmg_arctic";
  private static final String GENERAL_DB = "jdbc:mysql://data.eol.ucar.edu/dmg_general_ml";
  private static final String TEST_DB = "jdbc:mysql://data.dev.eol.ucar.edu/dmg_merged_ml";
  private static final String FINAL_DB = "jdbc:mysql://data.eol.ucar.edu/dmg_merged_ml";
  
  // Constants to define Classification Types.
  private static final String CATEGORY_TYPE = "Category";
  private static final String EVENT_TYPE = "Event";
  private static final String SITE_TYPE = "Site";
  
  private Connection dbArctic,dbGeneral,dbWrite;
  private boolean testPort;
  
  /**
   * Create a new instance of the ArcticGeneralMerge class.
   * @throws ClassNotFoundException when there is a problem loading the 
   * driver for the database.
   **/
  public ArcticGeneralMerge() throws ClassNotFoundException {
    Class.forName("com.mysql.jdbc.Driver");
  }
  
  /**
   * Clean up the connections to the database and rollback any commands written
   * to the database since they have not been manually committed.
   * @throws SQLException when there is a problem closing down the database connections.
   **/
  public void cleanUp() throws SQLException {
    if (dbArctic != null) { dbArctic.close(); }
    if (dbGeneral != null) { dbGeneral.close(); }
    if (dbWrite != null) {
      dbWrite.rollback();
      dbWrite.close();
    }
  }
  
  /**
   * Create the connection to read data from the arctic database.
   * @throws SQLException when there is a problem establishing the connection.
   **/
  public void createArcticConnection() throws SQLException {
    dbArctic = DriverManager.getConnection(ARCTIC_DB,username,password);
    dbArctic.setAutoCommit(false);
  }

  /**
   * Create the connection to read data from the general database.
   * @throws SQLException when there is a problem establishing the connection.
   **/
  public void createGeneralConnection() throws SQLException {
    dbGeneral = DriverManager.getConnection(GENERAL_DB,username,password);
    dbGeneral.setAutoCommit(false);
  }
  
  /**
   * Create the connection to write data to the merged database.
   * @throws SQLException when there is a problem establishing the connection.
   **/
  public void createWriteConnection() throws SQLException {
    dbWrite = DriverManager.getConnection(testPort ? TEST_DB : FINAL_DB,username,password);
    dbWrite.setAutoCommit(false);
  }

  /**
   * Merge the arctic and general Master List databases into the merged database.
   * @throws SQLException when there is an problem during the merging of databases.
   **/
  public void mergeMasterList() throws SQLException {
    createArcticConnection();
    createGeneralConnection();
    
    ProjectMap projects = new ProjectMap(dbArctic,dbGeneral,testPort);
    ClassMap classes = new ClassMap(dbArctic,dbGeneral);

    projects.associateClassifications(classes);
    projects.associatePhases();
    
    DatasetMap datasets = new DatasetMap(dbArctic,dbGeneral,projects);
    datasets.associateClassifications(projects,classes);
    datasets.associatePhases(projects);
    
    createWriteConnection();
    
    // Need to store the information in this order
    // 1. Classifications
    // 2. Projects/Phases (Needs Classifications)
    // 3. Datasets (Needs Classifications, Projects, and Phases)
    classes.store(dbWrite);
    projects.store(dbWrite,classes);
    datasets.store(dbWrite);
  }
  
  /**
   * Save all of the commands writted to the merged database.
   * @throws SQLException when there is a problem saving the transaction.
   **/
  public void save() throws SQLException {
    dbWrite.commit();
  }
  
  /**
   * Set the port action to be either a test port or a final port.
   * @param testPort <code>true</code> if the porting should be done to the
   * test database, <code>false</code> if the porting should be done to the final
   * database.
   **/
  public void setTest(boolean testPort) { this.testPort = testPort; }
  
  /**
   * Run the porting of the databases to the merged database.
   * @param args A list of arguments to pass to the command.  This should only include
   * a first parameter of "test" or "final" to define the port type.
   **/
  public static void main(String[] args) throws ClassNotFoundException,SQLException {
    ArcticGeneralMerge merger = new ArcticGeneralMerge();
    
    // Make sure the type parameter is test or final
    if (args.length > 0 && args[0].equalsIgnoreCase("test")) {
      merger.setTest(true);
    } else if (args.length > 0 && args[0].equalsIgnoreCase("final")) {
      merger.setTest(false);
    } else {
      System.err.println("Usage: java dmg.ml.port.ArcticGeneralMerge final|test");
      System.exit(0);
    }
    
    // Redefine the database.
    try {
      merger.mergeMasterList();
      merger.save();
    } catch (SQLException e) {
      e.printStackTrace();
    }
    
    // Shutdown all open connections.
    merger.cleanUp();
  }

  /**
   * The ClassMap class is a specialized map for loading, holding, and storing to the
   * database Classifications for the Master List.
   **/
  private class ClassMap extends TreeMap<String,Classification> {
    
    /**
     * Create a new instance of a ClassMap.
     * @param arctic The connection to be used for accessing arctic classifications.
     * @param general The connection to be used for accessing general classifications.
     * @throws SQLException when there is a problem reading the classifications
     * from one of the databases.
     **/
    public ClassMap(Connection arctic, Connection general) throws SQLException {
      createNewClassifications();
      readArcticDataTypes(arctic);
      readArcticDisciplines(arctic);
      readArcticPlatforms(arctic);
      readArcticSites(arctic);
      readGeneralCategories(general);
      readGeneralEvents(general);
      readGeneralSites(general);
      buildHeirarchy();
    }

    /**
     * Build the heirarchy for classifications that are not currently part of the
     * classification tree structure.  This primarily is to handle newly created
     * classifications and arctic classifications, but also contains a few of the
     * general classificationas as well.
     **/
    public void buildHeirarchy() {
      get("Biology").addChild(get("Abundance"));
      get("Biology").addChild(get("Biomass"));
      get("Biology").addChild(get("Microbiology"));
      get("Biology").addChild(get("Plankton"));
      get("Biology").addChild(get("Production"));
      get("Biology").addChild(get("Vegetation"));
      get("Flux").addChild(get("Flux Tower"));
      get("Flux").addChild(get("Trace Gases"));
      get("Hydrology").addChild(get("Precipitation"));
      get("Land Based").addChild(get("Mesonet"));
      get("Land Based").addChild(get("Precipitation"));
      get("Land Characterization").addChild(get("DEM"));
      get("Land Characterization").addChild(get("Bathymetry"));
      get("Land Characterization").addChild(get("Vegetation"));
      get("Model").addChild(get("Data Assimilation"));
      get("Model").addChild(get("Verification"));
      get("Oceanography").addChild(get("Buoy"));
      get("Oceanography").addChild(get("CTD"));
      get("Oceanography").addChild(get("Mooring"));
      get("Oceanography").addChild(get("Paleoceanography"));
      get("Oceanography").addChild(get("Water Sampling"));
      get("Photography").addChild(get("Aerial Photography"));
      get("Precipitation").addChild(get("Snow"));
      get("Satellite").addChild(get("SAR"));
      get("Ship Based").addChild(get("CCGS Des Groseilliers"));
      get("Ship Based").addChild(get("R/V Alpha Helix"));
      get("Ship Based").addChild(get("R/V Nathaniel B. Palmer"));
      get("Ship Based").addChild(get("Submarine"));
      get("Ship Based").addChild(get("USCGC Healy"));
      get("Ship Based").addChild(get("USCGC Polar Star"));
      get("Ship Based").addChild(get("USS Burton Island"));
      get("Soils").addChild(get("Active Layer/Permafrost"));
      get("Upper Air").addChild(get("Constant Level Balloon"));
      get("Upper Air").addChild(get("Dropsonde"));
      get("Upper Air").addChild(get("GPS"));
      get("Upper Air").addChild(get("Lidar"));
      get("Upper Air").addChild(get("Ozonesonde"));
      get("Upper Air").addChild(get("Profiler"));
      get("Upper Air").addChild(get("Radiosonde"));
      get("Upper Air").addChild(get("SODAR"));
      get("Upper Air").addChild(get("Tethersonde"));
      get("Upper Air").addChild(get("Thermosonde"));
    }
    
    /**
     * Clean the specified classification name.  This handles renaming classifications
     * and preventing unnecessary classifications from being created.
     * @param name The name of the classification to be cleaned.
     * @return The cleaned classification name.
     **/
    public String cleanClassificationName(String name) {
      if (name.equals("Atmospheric Thermodynamics (Surface)")) { name = "Land Based"; }
      else if (name.equals("Climate/Weather")) { name = "Land Based"; }
      else if (name.equals("Cloud Imagery")) { name = "Satellite"; }
      else if (name.equals("Cross-Platform")) { name = "Ancillary"; }
      else if (name.equals("Fluxes")) { name = "Flux"; }
      else if (name.equals("Forcing")) { name = "Model"; }
      else if (name.equals("Land")) { name = "Land Characterization"; }
      else if (name.equals("Land Use")) { name = "Land Characterization"; }
      else if (name.equals("Mineral")) { name = "Benthos"; }
      else if (name.equals("Oceanographic")) { name = "Oceanography"; }
      else if (name.equals("Radiance")) { name = "Radiation"; }
      else if (name.equals("Radiometer")) { name = "Radiation"; }
      else if (name.equals("Radioisotope Tracer")) { name = "Radioisotope"; }
      else if (name.equals("Rawinsonde")) { name = "Radiosonde"; }
      else if (name.equals("SAR Satellite")) { name = "SAR"; }
      else if (name.equals("Scintillometer")) { name = "Optics"; }
      else if (name.equals("Service-Bottle")) { name = "Water Sampling"; }
      else if (name.equals("Service-CTD")) { name = "CTD"; }
      else if (name.equals("Single Column Model")) { name = "Model"; }
      else if (name.equals("Snow and Ice Properties")) { name = "Sea Ice"; }
      else if (name.equals("Snow Gauge")) { name = "Snow"; }
      else if (name.equals("Soundings")) { name = "Radiosonde"; }
      else if (name.equals("Surface")) { name = "Land Based"; }
      else if (name.equals("Surface Based")) { name = "Land Based"; }

      // Prevent the following classfications from ever being created.
      //else if (name.equals("GTS")) { name = null; }
      else if (name.equals("Atmospheric Structure")) { name = null; }
      else if (name.equals("Multiple")) { name = null; }
      else if (name.equals("Portable Instrument")) { name = null; }
      else if (name.equals("Direct Sampling")) { name = null; }
      
      return name;
    }

    /**
     * Create a set of new classifications that are not in either database and
     * add them to the ClassMap.
     **/
    public void createNewClassifications() {
      // Create a Biology Classification
      Classification biology = new Classification(CATEGORY_TYPE);
      biology.setName("Biology");
      merge(biology);
      
      // Create a Chemistry Classification
      Classification chemistry = new Classification(CATEGORY_TYPE);
      chemistry.setName("Chemistry");
      merge(chemistry);
      
      // Create Sounding Classifications
      Classification rawinsonde = new Classification(CATEGORY_TYPE);
      rawinsonde.setName("Radiosonde");
      merge(rawinsonde);
      Classification dropsonde = new Classification(CATEGORY_TYPE);
      dropsonde.setName("Dropsonde");
      merge(dropsonde);
      Classification thermosonde = new Classification(CATEGORY_TYPE);
      thermosonde.setName("Thermosonde");
      merge(thermosonde);
      Classification tethersonde = new Classification(CATEGORY_TYPE);
      tethersonde.setName("Tethersonde");
      merge(tethersonde);
      Classification ozonesonde = new Classification(CATEGORY_TYPE);
      ozonesonde.setName("Ozonesonde");
      merge(ozonesonde);
      
      // Create Upper Air Classifications that will be used instead of Profiler/SODAR
      Classification profiler = new Classification(CATEGORY_TYPE);
      profiler.setName("Profiler");
      merge(profiler);
      Classification sodar = new Classification(CATEGORY_TYPE);
      sodar.setName("SODAR");
      merge(sodar);
      
      // Create Ship Classifications
      Classification helix = new Classification(CATEGORY_TYPE);
      helix.setName("R/V Alpha Helix");
      merge(helix);
      Classification palmer = new Classification(CATEGORY_TYPE);
      palmer.setName("R/V Nathaniel B. Palmer");
      merge(palmer);
      Classification healy = new Classification(CATEGORY_TYPE);
      healy.setName("USCGC Healy");
      merge(healy);

      Classification flxTwr = new Classification(CATEGORY_TYPE);
      flxTwr.setName("Flux Tower");
      merge(flxTwr);
    }
    
    /**
     * Merge the specified classification into the map.  It will ensure values are the
     * same if the classification is already in the map or will add it to the map if
     * it is not already in it.
     * @param classification The classification to be merged into the map.
     **/
    public void merge(Classification classification) {
      if (containsKey(classification.getName())) {
	get(classification.getName()).merge(classification);
      } else {
	put(classification.getName(),classification);
      }
    }

    /**
     * Read in the classifications from the arctic DataType table into the map.
     * @param conn The connection to use to read the DataTypes.
     * @throws SQLException when there is a problem reading the arctic data types.
     **/
    public void readArcticDataTypes(Connection conn) throws SQLException {
      String sql = "SELECT DISTINCT(name) FROM DataType WHERE proj_id IS NOT NULL";
      PreparedStatement stmt = conn.prepareStatement(sql);
      ResultSet results = stmt.executeQuery();
      while (results.next()) {
	String name = cleanClassificationName(results.getString(1));
	if (name != null) {
	  Classification classification = new Classification(CATEGORY_TYPE);
	  classification.setName(name);
	  merge(classification);
	}
      }
      results.close();
      stmt.close();
    }
    
    /**
     * Read in the classifications from the arctic Discipline table into the map.
     * @param conn The connection to use to read the Displines.
     * @throws SQLException when there is a problem reading the arctic disciplines.
     **/
    public void readArcticDisciplines(Connection conn) throws SQLException {
      String sql = "SELECT DISTINCT(name) FROM Discipline WHERE proj_id IS NOT NULL";
      PreparedStatement stmt = conn.prepareStatement(sql);
      ResultSet results = stmt.executeQuery();
      while (results.next()) {
	String name = cleanClassificationName(results.getString(1));
	if (name != null) {
	  Classification classification = new Classification(CATEGORY_TYPE);
	  classification.setName(name);
	  merge(classification);
	}
      }
      results.close();
      stmt.close();
    }
    
    /**
     * Read in the classifications from the arctic Platforms table into the map.
     * @param conn The connection to use to read the Platforms.
     * @throws SQLException when there is a problem reading the arctic platforms.
     **/
    public void readArcticPlatforms(Connection conn) throws SQLException {
      String sql = "SELECT DISTINCT(name) FROM Platform WHERE proj_id IS NOT NULL";
      PreparedStatement stmt = conn.prepareStatement(sql);
      ResultSet results = stmt.executeQuery();
      while (results.next()) {
	String name = cleanClassificationName(results.getString(1));
	if (name != null) {
	  Classification classification = null;
	  
	  // The following are really cruises, so make them events.
	  if (name.equals("AOS-94") || name.equals("AWS-02") || name.startsWith("HLY-") ||
	      name.startsWith("HX-") || name.equals("NBP03-04a")) {
	    classification = new Classification(EVENT_TYPE);
	  }
	  // The following are really sites, so make them sites.
	  else if (name.equals("SHEBA Ice Camp") || name.equals("SHEBA Tower")) {
	    classification = new Classification(SITE_TYPE);
	  } 
	  // Everything eles is a category.
	  else {		    
	    classification = new Classification(CATEGORY_TYPE);
	  }
	  classification.setName(name);
	  merge(classification);
	}
      }
      results.close();
      stmt.close();
    }

    /**
     * Read in the classifications form the arctic Sites table into the map.
     * @param conn The connection to use to read the Sites.
     * @throws SQLException when there is a problem reading the arctic sites.
     **/
    public void readArcticSites(Connection conn) throws SQLException {
      String sql = "SELECT DISTINCT(name) FROM Site WHERE proj_id IS NOT NULL";
      PreparedStatement stmt = conn.prepareStatement(sql);
      ResultSet results = stmt.executeQuery();
      while (results.next()) {
	String name = cleanClassificationName(results.getString(1));
	if (name != null) {
	  Classification classification = new Classification(SITE_TYPE);
	  classification.setName(name);
	  merge(classification);
	}
      }
      results.close();
      stmt.close();
    }
    
    /**
     * Read in the general category classifications into the map.
     * @param conn The connection to use to read the categories.
     * @throws SQLException when there is a problem reading the general categories.
     **/
    public void readGeneralCategories(Connection conn) throws SQLException {
      String sql = "SELECT name FROM category WHERE parent_id IS NULL";
      PreparedStatement stmt = conn.prepareStatement(sql);
      ResultSet results = stmt.executeQuery();
      while (results.next()) {
	String name = cleanClassificationName(results.getString(1));

	// Only handle the categories.
	if (name != null && !name.equals("Events") && !name.equals("Sites")) {
	  Classification classification = null;

	  // The following are actually sites
	  if (name.equals("Diego Garcia") || name.equals("India") || name.equals("Maldives") ||
	      name.equals("Mauritius")) {
	    classification = new Classification(SITE_TYPE);
	  } 
	  // The rest are really categories.
	  else {
	    classification = new Classification(CATEGORY_TYPE);
	  }
	  classification.setName(name);
	  merge(classification);
	  
	  readGeneralSubCategories(conn,get(classification.getName()));
	}
      }
      results.close();
      stmt.close();
    }
    
    /**
     * Read in the general event classifications into the map.
     * @param conn The connection to use to read in the events.
     * @throws SQLException when there is a problem reading in the events.
     **/
    public void readGeneralEvents(Connection conn) throws SQLException {
      String sql = "SELECT category.name FROM category JOIN category AS parent ON category.parent_id=parent.category_id WHERE parent.name=?";
      PreparedStatement stmt = conn.prepareStatement(sql);
      stmt.setString(1,"Events");
      ResultSet results = stmt.executeQuery();
      while (results.next()) {
	String name = cleanClassificationName(results.getString(1));
	if (name != null) {
	  Classification classification = new Classification(EVENT_TYPE);
	  classification.setName(name);
	  merge(classification);
	  
	  readGeneralSubCategories(conn,get(classification.getName()));
	}
      }
      results.close();
      stmt.close();
    }
    
    /**
     * Read in the general site classifications into the map.
     * @param conn The connection to use to read the sites.
     * @throws SQLException when there is a problem reading in the sites.
     **/
    public void readGeneralSites(Connection conn) throws SQLException {
      String sql = "SELECT category.name FROM category JOIN category AS parent ON category.parent_id=parent.category_id WHERE parent.name=?";
      PreparedStatement stmt = conn.prepareStatement(sql);
      stmt.setString(1,"Sites");
      ResultSet results = stmt.executeQuery();
      while (results.next()) {
	String name = cleanClassificationName(results.getString(1));
	if (name != null) {
	  Classification classification = new Classification(SITE_TYPE);
	  classification.setName(name);
	  merge(classification);
	  
	  readGeneralSubCategories(conn,get(classification.getName()));
	}
      }
      results.close();
      stmt.close();
    }
    
    /**
     * Determine the subcategories from the general database for the specified classification.
     * @param conn The connection to use to read the child categories.
     * @param parent The classification that is having its children loaded.
     * @throws SQLException when there is a problem reading in the parent's children.
     **/
    public void readGeneralSubCategories(Connection conn, Classification parent) throws SQLException {
      String sql = "SELECT category.name,parent.name FROM category JOIN category AS parent ON category.parent_id=parent.category_id WHERE parent.name=?";
      PreparedStatement stmt = conn.prepareStatement(sql);
      stmt.setString(1,parent.getName());
      ResultSet results = stmt.executeQuery();
      while (results.next()) {
	String name = cleanClassificationName(results.getString(1));
	if (name != null) {
	    Classification child = new Classification(parent.getType());
	    child.setName(results.getString(1));
	    merge(child);
	    
	    readGeneralSubCategories(conn,get(child.getName()));
	    parent.addChild(get(child.getName()));
	}
      }
      results.close();
      stmt.close();
    }

    /**
     * Store all of the Classifications held in this map into the merged database.
     * @param conn The connection to use to store the classifications.
     * @throws SQLException when there is a problem storing the classifications.
     **/
    public void store(Connection conn) throws SQLException {
      Map<String,Integer> typeMap = storeTypes(conn);
	    
      String sql = "INSERT INTO classification(name,type_id) VALUES(?,?)";
      PreparedStatement stmt = conn.prepareStatement(sql,Statement.RETURN_GENERATED_KEYS);

      for (Classification classification: values()) {
	// Ignore these classifications as they should be handled through other
	// classifications.
	if (!classification.getName().equals("Profiler/SODAR")) {
	  stmt.setString(1,classification.getName());
	  stmt.setInt(2,typeMap.get(classification.getType()));
	  stmt.executeUpdate();
	
	  // Access the auto-incremented id and put it with the classification
	  ResultSet keys = stmt.getGeneratedKeys();
	  keys.next();
	  classification.setClassificationId(keys.getInt(1));
	  keys.close();
	}
      }
      stmt.close();
      
      // Now insert the parent/child relationships
      sql = "INSERT INTO classification_parent(class_id,parent_class_id) VALUES(?,?)";
      stmt = conn.prepareStatement(sql);
      for (Classification parent: values()) {
	for (Classification child: parent.getChildren()) {
	  stmt.setInt(1,child.getClassificationId());
	  stmt.setInt(2,parent.getClassificationId());
	  stmt.executeUpdate();
	}
      }
      stmt.close();
    }

    /**
     * Store the classification types used by the Master List into the database.
     * @param conn The connection used to store the classification types.
     * @return A map of type names to unique database ids.
     * @throws SQLException when there is a problem storing the classification types.
     **/
    public Map<String,Integer> storeTypes(Connection conn) throws SQLException {
      Map<String,Integer> typeMap = new TreeMap<String,Integer>();
      typeMap.put(CATEGORY_TYPE,0);
      typeMap.put(EVENT_TYPE,0);
      typeMap.put(SITE_TYPE,0);
      
      String sql = "INSERT INTO classification_type(name) VALUES(?)";
      PreparedStatement stmt = conn.prepareStatement(sql,Statement.RETURN_GENERATED_KEYS);
      
      for (String key: typeMap.keySet()) {
	stmt.setString(1,key);
	stmt.executeUpdate();
	
	// Access the auto-incremented type id and store it with the type.
	ResultSet keys = stmt.getGeneratedKeys();
	keys.next();
	typeMap.put(key,keys.getInt(1));
	keys.close();
      }
      
      return typeMap;
    }
  }

  /**
   * The Classification class is a container for representing a Classification
   * in the merged database.
   **/
  private class Classification implements Comparable<Classification> {

    private Integer id;
    private String name,type;
    private Set<Classification> children;
    
    /**
     * Create a new Classification of the specified type.
     * @param type The type of the classification.
     **/
    public Classification(String type) {
      this.type = type;
      children = new TreeSet<Classification>();
    }
    
    /**
     * Add a child classification to this classification.
     * @param child A classification that is a direct child of this classification.
     **/
    public void addChild(Classification child) {
      children.add(child);
    }
    
    /**
     * Compare this classification with the specified one.
     * @param item The classification to be compared to this one.
     * @return A negative, zero, or positive integer if the name of this classification
     * is less than, equal to, or greater than the item.
     **/
    public int compareTo(Classification item) {
      return getName().toLowerCase().compareTo(item.getName().toLowerCase());
    }
    
    /**
     * Get the set of children for this classification.
     * @return This classification's children.
     **/
    public Set<Classification> getChildren() { return children; }

    /**
     * Get the unique id number for the classification.
     * @return The classification's id number.
     **/
    public Integer getClassificationId() { return id; }
    
    /**
     * Get the name of the classification.
     * @return The classification's name.
     **/
    public String getName() { return name; }

    /**
     * Get the type of the classification.
     * @return The classification's type.
     **/
    public String getType() { return type; }

    /**
     * Merge the specified classification into this one.
     * @param classification The classification to merge into this one.
     * @throws PortException if the values of the specified classification do not match
     * the same values in this classification.
     **/
    public void merge(Classification classification) {
      // Sanity Check!!  Should never occur!
      if (!getName().equals(classification.getName())) {
	throw new PortException("Classification name merge error: Should never occur!");
      }

      // Compare the types.
      if (!getType().equals(classification.getType())) {
	throw new PortException(String.format("%s has mismatched types: %s - %s",
					      getName(),getType(),classification.getType()));
      }
    }

    /**
     * Set the unique identifier for the classification.
     * @param id The classification's id number.
     **/
    public void setClassificationId(Integer id) { this.id = id; }

    /**
     * Set the name of the classification.
     * @param name The classification's name.
     **/
    public void setName(String name) { this.name = name; }

    /**
     * Get the String representation of the classification.
     * @return The classification's name.
     **/
    public String toString() { return getName(); }
  }

  /**
   * The DatasetMap class is a specialized container for loading, holding, and storing
   * Datasets into the Master List database.
   **/  
  private class DatasetMap extends TreeMap<String,Dataset> {
    
    private Connection arctic,general;
    private int nextIdNumber;
    
    /**
     * Create a new instance of a DatasetMap.
     * @param arctic The connection to use to connect to the arctic database.
     * @param general The connection to use to connect to the general database.
     * @param projects The map of the projects to associate to the database.
     * @throws SQLException when there is a problem reading in the dataset information.
     **/
    public DatasetMap(Connection arctic, Connection general, ProjectMap projects) throws SQLException {
      this.arctic = arctic;
      this.general = general;
      
      // General MUST be done before arctic so the ML ids don't collide!
      readGeneralDatasets(projects);
      readArcticDatasets(projects);
    }

    /**
     * Associate the arctic data type classifications with their data sets.
     * @param projects The map of projects for the classifications.
     * @param classes The map of classifications that can be assigned to the data set.
     * @throws SQLException when there is a problem reading in the data types.
     **/
    public void associateArcticDataTypes(ProjectMap projects, ClassMap classes) throws SQLException {
      String sql = "SELECT storm_id,title,ListProjects.name,DataType.name FROM List JOIN ListProjects ON List.proj_id=ListProjects.id JOIN DataType ON List.data_type_id=DataType.id WHERE DataType.proj_id IS NOT NULL";
      PreparedStatement stmt = arctic.prepareStatement(sql);
      ResultSet results = stmt.executeQuery();
      while (results.next()) {
	Dataset dataset = new Dataset(this);
	dataset.setDatasetId(results.getString(1));
	dataset.setArcticName(results.getString(2));
	dataset.setProjectInformation(projects.get(results.getString(3)),null,false,false);
	
	String name = classes.cleanClassificationName(results.getString(4));
	if (name != null) {
	  get(dataset.getDatasetId()).addClassification(classes.get(name));
	}
      }
      results.close();
      stmt.close();
    }
    
    /**
     * Associate the arctic disciplines classifications with their data sets.
     * @param projects The map of projects for the classifications.
     * @param classes The map of classifications that can be assigned to the data set.
     * @throws SQLException when there is a problem reading in the disciplines.
     **/
    public void associateArcticDisciplines(ProjectMap projects, ClassMap classes) throws SQLException {
      String sql = "SELECT storm_id,title,ListProjects.name,Discipline.name FROM List JOIN ListProjects ON List.proj_id=ListProjects.id JOIN Discipline ON List.discipline_id=Discipline.id WHERE Discipline.proj_id IS NOT NULL";
      PreparedStatement stmt = arctic.prepareStatement(sql);
      ResultSet results = stmt.executeQuery();
      while (results.next()) {
	Dataset dataset = new Dataset(this);
	dataset.setDatasetId(results.getString(1));
	dataset.setArcticName(results.getString(2));
	dataset.setProjectInformation(projects.get(results.getString(3)),null,false,false);
	
	String name = classes.cleanClassificationName(results.getString(4));
	if (name != null) {
	  get(dataset.getDatasetId()).addClassification(classes.get(name));
	}
      }
      results.close();
      stmt.close();
    }
    
    /**
     * Associate the arctic platform classifications with their data sets.
     * @param projects The map of projects for the classifications.
     * @param classes The map of classifications that can be assigned to the data set.
     * @throws SQLException when there is a problem reading in the platforms.
     **/
    public void associateArcticPlatforms(ProjectMap projects, ClassMap classes) throws SQLException {
      String sql = "SELECT storm_id,title,ListProjects.name,Platform.name FROM List JOIN ListProjects ON List.proj_id=ListProjects.id JOIN Platform ON List.platform_id=Platform.id WHERE Platform.proj_id IS NOT NULL";
      PreparedStatement stmt = arctic.prepareStatement(sql);
      ResultSet results = stmt.executeQuery();
      while (results.next()) {
	Dataset dataset = new Dataset(this);
	dataset.setDatasetId(results.getString(1));
	dataset.setArcticName(results.getString(2));
	dataset.setProjectInformation(projects.get(results.getString(3)),null,false,false);
	
	String name = classes.cleanClassificationName(results.getString(4));
	if (name != null) {
	  get(dataset.getDatasetId()).addClassification(classes.get(name));

	  // Handle healy events by adding the healy ship to both the project and data set.
	  if (name.startsWith("HLY-")) {
	    projects.get(results.getString(3)).addClassification(classes.get("USCGC Healy"));
	    projects.get(results.getString(3)).addClassification(classes.get("Ship Based"));
	    get(dataset.getDatasetId()).addClassification(classes.get("USCGC Healy"));
	  } 
	  // Handle alpha helix events by adding the ship to both the project and data set.
	  else if (name.startsWith("HX-")) {
	    projects.get(results.getString(3)).addClassification(classes.get("R/V Alpha Helix"));
	    projects.get(results.getString(3)).addClassification(classes.get("Ship Based"));
	    get(dataset.getDatasetId()).addClassification(classes.get("R/V Alpha Helix"));
	  }
	  // Handle nathaniel palmer events by adding the ship to both the project and data set
	  else if (name.startsWith("NBP03-04a")) {
	    projects.get(results.getString(3)).addClassification(classes.get("R/V Nathaniel B. Palmer"));
	    projects.get(results.getString(3)).addClassification(classes.get("Ship Based"));
	    get(dataset.getDatasetId()).addClassification(classes.get("R/V Nathaniel B. Palmer"));
	  }
	}
      }
      results.close();
      stmt.close();
    }

    /**
     * Associate the arctic site classifications with their data sets.
     * @param projects The map of projects for the classifications.
     * @param classes The map of classifications that can be assigned to the data set.
     * @throws SQLException when there is a problem reading in the sites.
     **/
    public void associateArcticSites(ProjectMap projects, ClassMap classes) throws SQLException {
      String sql = "SELECT storm_id,title,ListProjects.name,Site.name FROM List JOIN ListProjects ON List.proj_id=ListProjects.id JOIN Site ON List.site_id=Site.id WHERE Site.proj_id IS NOT NULL";
      PreparedStatement stmt = arctic.prepareStatement(sql);
      ResultSet results = stmt.executeQuery();
      while (results.next()) {
	Dataset dataset = new Dataset(this);
	dataset.setDatasetId(results.getString(1));
	dataset.setArcticName(results.getString(2));
	dataset.setProjectInformation(projects.get(results.getString(3)),null,false,false);
	
	String name = classes.cleanClassificationName(results.getString(4));
	if (name != null) {
	  get(dataset.getDatasetId()).addClassification(classes.get(name));
	}
      }
      results.close();
      stmt.close();
    }
    
    /**
     * Associate all of the classifications for the data sets.
     * @param projects The map of projects.
     * @param classifications The map of classifications.
     * @throws SQLException if there is a problem associating the classifications 
     * to the data sets.
     **/
    public void associateClassifications(ProjectMap projects, ClassMap classifications) throws SQLException {
      associateArcticDataTypes(projects,classifications);
      associateArcticDisciplines(projects,classifications);
      associateArcticPlatforms(projects,classifications);
      associateArcticSites(projects,classifications);
      associateGeneralCategories(classifications);
    }
    
    /**
     * Associate the general category classifications with their data sets.
     * @param projects The map of projects for the classifications.
     * @param classes The map of classifications that can be assigned to the data set.
     * @throws SQLException when there is a problem reading in the categories.
     **/
    public void associateGeneralCategories(ClassMap classifications) throws SQLException {
      String sql = "SELECT dataset_id,category.name FROM dataset_category JOIN category ON dataset_category.category_id=category.category_id WHERE category.name != ?";
      PreparedStatement stmt = general.prepareStatement(sql);
      stmt.setString(1,"Events");
      ResultSet results = stmt.executeQuery();
      while (results.next()) {
	Dataset dataset = get(results.getString(1));
	String name = classifications.cleanClassificationName(results.getString(2));
	if (name != null) {
	  // Special case profiler/sodar to split data sets into the correct category
	  if (name.equals("Profiler/SODAR")) {
	    boolean setClassification = false;
	    // Handle cases where data set is profiler data
	    if (dataset.getName().toLowerCase().contains("profile")) {
	      dataset.addClassification(classifications.get("Profiler"));
	      setClassification = true;
	    }
	    // Handle cases where data set is SODAR data
	    if (dataset.getName().toLowerCase().contains("sodar")) {
	      dataset.addClassification(classifications.get("SODAR"));
	      setClassification = true;
	    }
	    // If a classification wasn't set, set it to both.
	    if (!setClassification) {
	      dataset.addClassification(classifications.get("Profiler"));
	      dataset.addClassification(classifications.get("SODAR"));
	    }
	  } else {
	    dataset.addClassification(classifications.get(name));
	  }
	}
      }
      results.close();
      stmt.close();
    }

    /**
     * Associate the arctic phases with their data sets.
     * @param projects The map of projects for the phases.
     * @throws SQLException when there is a problem reading in the phases.
     **/
    public void associatePhases(ProjectMap projects) throws SQLException {
      String sql = "SELECT storm_id,title,ListProjects.name,phase FROM List JOIN ListProjects ON List.proj_id=ListProjects.id WHERE phase IS NOT NULL AND phase != ?";
      PreparedStatement stmt = arctic.prepareStatement(sql);
      stmt.setString(1,"");
      ResultSet results = stmt.executeQuery();
      while (results.next()) {
	Dataset dataset = new Dataset(this);
	dataset.setDatasetId(results.getString(1));
	dataset.setArcticName(results.getString(2));
	dataset.setProjectInformation(projects.get(results.getString(3)),null,false,false);
	
	get(dataset.getDatasetId()).addPhase(projects.get(results.getString(3)).getPhase(results.getString(4)));
      }
    }
    
    /**
     * Generate the next Master List dataset id for the specified data set if it cannot
     * be found in the map.
     * @param dataset The dataset to find the dataset id for.
     **/
    public String generateDatasetId(Dataset dataset) {
      for (String key: keySet()) {
	if (key.startsWith("ML.")) {
	  Dataset entry = get(key);
	  if (entry.getName().equals(dataset.getName()) && entry.hasProjects(dataset.getProjects())) {
	    return key;
	  }
	}
      }
      return String.format("ML.%d",nextIdNumber++);
    }

    /**
     * Merge the specified dataset into the map by adding it if it is new, or merging it
     * with the dataset with the same id in the map.
     * @param dataset The dataset to be merged into the map.
     **/
    public void merge(Dataset dataset) {
      if (containsKey(dataset.getDatasetId())) {
	get(dataset.getDatasetId()).merge(dataset);
      } else {
	put(dataset.getDatasetId(),dataset);
      }
    }

    /**
     * Add the specified dataset into the map.  This ensures that the dataset has a valid
     * dataset id and that the map's id counter is properly incremented.
     * @param key The key that uniquely identifies the dataset.
     * @param dataset The dataset to be added to the map.
     **/
    @Override public Dataset put(String key, Dataset dataset) {
      Dataset entered = super.put(key,dataset);
      if (dataset.getDatasetId().startsWith("ML.")) {
	int value = Integer.parseInt(dataset.getDatasetId().substring(3)) + 1;
	nextIdNumber = value > nextIdNumber ? value : nextIdNumber;
      }
      return entered;
    }

    /**
     * Read in the datasets that are in the arctic database.
     * @param projects The map of projects to associate to the datasets.
     * @throws SQLException if there is a problem reading in the datasets.
     **/
    public void readArcticDatasets(ProjectMap projects) throws SQLException {
      // Define the pattern that contains updated data set information in the comments.
      Pattern pattern = Pattern.compile("updated.+(\\d+)\\/(\\d+)\\/(\\d+)");
      
      String sql = "SELECT storm_id,title,date,author,doc_url,url,ListProjects.name,comments FROM List JOIN ListProjects ON List.proj_id=ListProjects.id";
      PreparedStatement stmt = arctic.prepareStatement(sql);
      ResultSet results = stmt.executeQuery();
      while (results.next()) {
	Dataset dataset = new Dataset(this);
	dataset.setDatasetId(results.getString(1));
	dataset.setArcticName(results.getString(2));
	dataset.setProjectInformation(projects.get(results.getString(7)),results.getDate(3),false,false);
	dataset.setAuthorPi(results.getString(4));
	dataset.setDocUrl(results.getString(5));
	dataset.setUrl(results.getString(6));
	
	// Search for updated dates in the comments.
	Matcher matcher = pattern.matcher(results.getString(8).toLowerCase());
	if (matcher.matches()) {
	  dataset.setDateUpdated(java.sql.Date.valueOf(String.format("%04d-%02d-%02d",Integer.parseInt(matcher.group(3)),Integer.parseInt(matcher.group(1)),Integer.parseInt(matcher.group(2)))));
	}
	
	// Search for preliminary information in the dataset name or comments.
	if (results.getString(8).toLowerCase().contains("prelim") || results.getString(2).toLowerCase().contains("prelim")) {
	  dataset.setPreliminary(true);
	}
	
	merge(dataset);
	put(dataset.getDatasetId(),dataset);
      }
      results.close();
      stmt.close();
    }
    
    /**
     * Read in the datasets that are in the general database.
     * @param projects The map of projects to associate with the datasets.
     * @throws SQLException when there is a problem reading the datasets.
     **/
    public void readGeneralDatasets(ProjectMap projects) throws SQLException {
      PreparedStatement projStmt = general.prepareStatement("SELECT project_id,date_posted,in_progress_flag,hide_flag FROM dataset_project WHERE dataset_id=?");
      
      // Define the pattern to pull out the author/pi information from the dataset name.
      Pattern pattern = Pattern.compile("^\\s*(.+)\\s+\\[(.+)\\]\\s*$");
      
      String sql = "SELECT dataset_id,name,url,doc_url,date_expected,date_updated FROM dataset";
      PreparedStatement stmt = general.prepareStatement(sql);
      ResultSet results = stmt.executeQuery();
      while (results.next()) {
	Dataset dataset = new Dataset(this);
	dataset.setDatasetId(results.getString(1));
	
	// Try to pull out the author/pi from the dataset name
	Matcher matcher = pattern.matcher(results.getString(2));
	if (matcher.matches()) {
	  dataset.setName(matcher.group(1));
	  dataset.setAuthorPi(matcher.group(2));
	} else {
	  dataset.setName(results.getString(2));
	}
	
	dataset.setUrl(results.getString(3));
	dataset.setDocUrl(results.getString(4));
	dataset.setDateExpected(results.getDate(5));
	dataset.setDateUpdated(results.getDate(6));
	
	// Read in the project specific information for the dataset.
	projStmt.setString(1,dataset.getDatasetId());
	ResultSet projResults = projStmt.executeQuery();
	while (projResults.next()) {
	  Project project = projects.get(projResults.getString(1));
	  dataset.setProjectInformation(project,projResults.getDate(2),projResults.getBoolean(3),projResults.getBoolean(4));
	}
	projResults.close();
	
	// Search for preliminary information in the dataset name.
	if (results.getString(2).toLowerCase().contains("prelim")) {
	  dataset.setPreliminary(true);
	}

	merge(dataset);
	put(dataset.getDatasetId(),dataset);
      }
      results.close();
      stmt.close();
      projStmt.close();
    }

    /**
     * Store all of the datasets in the map into the merged database.
     * @param conn The connection to use to store the datasets.
     * @throws SQLException when there is a problem storing the datasets.
     **/
    public void store(Connection conn) throws SQLException {
      String sql = "INSERT INTO dataset(dataset_id,name,url,doc_url,date_expected,date_updated,author_pi) VALUES(?,?,?,?,?,?,?)";
      PreparedStatement stmt = conn.prepareStatement(sql);
      
      for (Dataset dataset: values()) {
	stmt.setString(1,dataset.getDatasetId());
	stmt.setString(2,dataset.getName());
	stmt.setString(3,dataset.getUrl());
	stmt.setString(4,dataset.getDocUrl());
	if (dataset.getDateExpected() == null) {
	  stmt.setNull(5,Types.DATE);
	} else {
	  stmt.setDate(5,dataset.getDateExpected());
	}
	if (dataset.getDateUpdated() == null) {
	  stmt.setNull(6,Types.DATE);
	} else {
	  stmt.setDate(6,dataset.getDateUpdated());
	}
	stmt.setString(7,dataset.getAuthorPi());
	stmt.executeUpdate();
	
	// Print out a warning for preliminary datasets, but don't set the flag in the
	// database.
	if (dataset.isPreliminary()) {
	  System.out.println(String.format("Dataset (%s: %s) is set as preliminary.",
					   dataset.getDatasetId(),dataset.getName()));
	}
	
	// Make all of the associations for the dataset.
	storeDatasetProjects(conn,dataset);
	storeDatasetPhases(conn,dataset);
	storeDatasetClassifications(conn,dataset);
      }
      stmt.close();
    }

    /**
     * Associate all of the dataset's classifications in the database.
     * @param conn The connection to use to execute the commands.
     * @param dataset The dataset to have its classifications associated.
     **/
    public void storeDatasetClassifications(Connection conn, Dataset dataset) throws SQLException {
      String sql = "INSERT INTO dataset_classification(dataset_id,class_id) VALUES(?,?)";
      PreparedStatement stmt = conn.prepareStatement(sql);
      stmt.setString(1,dataset.getDatasetId());
      
      for (Classification classification: dataset.getClassifications()) {
	stmt.setInt(2,classification.getClassificationId());
	stmt.executeUpdate();
      }
      stmt.close();
    }

    /**
     * Associated the dataset's phases in the database.
     * @param conn The connection to use to execute the command.
     * @param dataset The dataset to have its phases associated.
     **/
    public void storeDatasetPhases(Connection conn, Dataset dataset) throws SQLException {
      String sql = "INSERT INTO dataset_phase(dataset_id,phase_id) VALUES(?,?)";
      PreparedStatement stmt = conn.prepareStatement(sql);
      stmt.setString(1,dataset.getDatasetId());
      
      for (Phase phase: dataset.getPhases()) {
	stmt.setInt(2,phase.getPhaseId());
	stmt.executeUpdate();
      }
      stmt.close();
    }

    /**
     * Associate teh dataset's projects in the database.
     * @param conn The connection to use to execute the commands.
     * @param dataset The dataset to have its projects associated.
     **/
    public void storeDatasetProjects(Connection conn, Dataset dataset) throws SQLException {
      String sql = "INSERT INTO dataset_project(dataset_id,project_id,date_posted,in_progress_flag,hide_flag) VALUES(?,?,?,?,?)";
      PreparedStatement stmt = conn.prepareStatement(sql);
      stmt.setString(1,dataset.getDatasetId());
      
      for (DatasetProject project: dataset.getProjects()) {
	stmt.setString(2,project.getProject().getProjectId());
	if (project.getDatePosted() == null) {
	  stmt.setNull(3,Types.DATE);
	} else {
	  stmt.setDate(3,project.getDatePosted());
	}
	stmt.setBoolean(4,project.isInProgress());
	stmt.setBoolean(5,project.isHidden());
	stmt.executeUpdate();
      }
      stmt.close();
    }
  }

  /**
   * The Dataset class is a container for a dataset in the Master List.
   **/
  private class Dataset implements Comparable<Dataset> {

    private boolean preliminary;
    private DatasetMap datasetMap;
    private String datasetId,name,authorPi,docUrl,url;
    private java.sql.Date dateExpected,dateUpdated;
    private Map<Project,DatasetProject> projects;
    private Set<Classification> classes;
    private Set<Phase> phases;
    
    /**
     * Create a new instance of a Dataset.
     * @param datasetMap The map that holds the dataset.
     **/
    public Dataset(DatasetMap datasetMap) {
      this.datasetMap = datasetMap;
      projects = new TreeMap<Project,DatasetProject>();
      classes = new TreeSet<Classification>();
      phases = new TreeSet<Phase>();
    }

    /**
     * Add the specified classification to the set to be associated with this dataset.
     * @param classification The classification to associate with this dataset.
     **/
    public void addClassification(Classification classification) {
      classes.add(classification);
    }
    
    /**
     * Add the specified phase to the set to be associated with this dataset.
     * @param phase The phase to associate with this dataset.
     **/
    public void addPhase(Phase phase) { 
      phases.add(phase); 
    }

    /**
     * Clean up an arctic dataset name.
     * @param name The name to be cleaned.
     * @return The cleaned dataset name.
     **/
    private String cleanArcticName(String name) {
      // Remove projects from the dataset name.
      name = name.replaceAll("^\\s*ITEX:?\\s*","");
      name = name.replaceAll("^\\s*SBI:\\s*","");
      name = name.replaceAll("^\\s*SHEBA:\\s*","");
      name = name.replaceAll("^\\s*ATLAS:\\s*","");

      // Remove years in the dataset name.
      name = name.replaceAll("^\\s*\\d{4}\\-\\d{4}[,]?\\s*","");
      name = name.replaceAll("^\\s*(\\d{4}[,]?\\s*)+\\s*","");

      // Change Abreviations to the full text.
      name = name.replaceAll(" Avg "," Average ");
      name = name.replaceAll(" Temps "," Temperatures ");
      
      // Remove Author/PI information from the dataset name.
      name = name.replaceAll("\\s+\\(Ahlquist\\)\\s*$","");
      name = name.replaceAll("\\s+\\(Alatalo\\)\\s*$","");
      name = name.replaceAll("\\s+\\(Beringer\\)\\s*$","");
      name = name.replaceAll("\\s+\\(Bret\\-Harte\\)\\s*$","");
      name = name.replaceAll("\\s+\\(Calef\\)\\s*$","");
      name = name.replaceAll("\\s+\\(Epstein\\)\\s*$","");
      name = name.replaceAll("\\s+\\(Hollister\\)\\s*$","");
      name = name.replaceAll("\\s+\\(Jia.*\\)\\s*$","");
      name = name.replaceAll("\\s+\\(Jonsdottir\\)\\s*$","");
      name = name.replaceAll("\\s+\\(Magnusson\\)\\s*$","");
      name = name.replaceAll("\\s+\\(Molau\\)\\s*$","");
      name = name.replaceAll("\\s+\\(Oberbauer\\)\\s*$","");
      name = name.replaceAll("\\s+\\(Robinson.*\\)\\s*$","");
      name = name.replaceAll("\\s+\\(Romanovsky\\)\\s*$","");
      name = name.replaceAll("\\s+\\(Suding\\)\\s*$","");
      name = name.replaceAll("\\s+\\(Totland\\)\\s*$","");
      name = name.replaceAll("\\s+\\(M?Walker\\)\\s*$","");
      name = name.replaceAll("\\s+\\(Webber\\)\\s*$","");
      name = name.replaceAll("\\s+\\(Welker.*\\)\\s*$","");
      
      return name.trim();
    }
    
    /**
     * Determine if the specified classification is already associated with the dataset.
     * @param classification The classification to be tested.
     * @return <code>true</code> if the classification is associated with the dataset,
     * <code>false</code> otherwise.
     **/
    public boolean contains(Classification classification) {
      return classes.contains(classification);
    }

    /**
     * Determine the sort order of the specified dataset compared to this one.
     * @param dataset The dataset to be compared to this one.
     * @return A negative, zero, or positive integer if the dataset id of this dataset
     * is less than, equal to, or greater than the dataset id of the specified dataset.
     **/
    public int compareTo(Dataset dataset) {
      return getDatasetId().compareTo(dataset.getDatasetId());
    }

    /**
     * Get the author/PI for the dataset.
     * @return The dataset's author/PI.
     **/
    public String getAuthorPi() { return authorPi == null ? "" : authorPi; }

    /**
     * Get the set of classifications that are associated with this dataset.
     * @return The set of associated classifications.
     **/
    public Set<Classification> getClassifications() { return classes; }

    /**
     * Get the unique id for the dataset.
     * @return The dataset's unique id.
     **/
    public String getDatasetId() {
      if (datasetId == null || datasetId.equals("") || datasetId.startsWith("99.")) {
	setDatasetId(datasetMap.generateDatasetId(this));
      }
      return datasetId;
    }

    /**
     * Get the expected date for the dataset.
     * @return The dataset's expected date;
     **/
    public java.sql.Date getDateExpected() { return dateExpected; }

    /**
     * Get the updated date for the dataset.
     * @return The dataset's updated date.
     **/
    public java.sql.Date getDateUpdated() { return dateUpdated; }

    /**
     * Get the URL for the dataset's document.
     * @return The dataset's URL for the document.
     **/
    public String getDocUrl() { return docUrl; }

    /**
     * Get the name of the dataset.
     * @return The dataset's name.
     **/
    public String getName() { return name; }

    /**
     * Get the set of phases that are associated with this dataset.
     * @return The phases associated with the dataset.
     **/
    public Set<Phase> getPhases() { return phases; }
    
    /**
     * Get the set of projects associated with this dataset.
     * @return The set of projects associated with the dataset.
     **/
    public Collection<DatasetProject> getProjects() { return projects.values(); }
    
    /**
     * Get the URL for the dataset.
     * @return The dataset's URL.
     **/
    public String getUrl() { return url == null ? "" : url; }
    
    /**
     * Determine if the dataset is associated with all of the specified projects.
     * @param projectSet The set of projects to be tested.
     * @return <code>true</code> if the dataset is associated with all of the projects,
     * <code>false</code> otherwise.
     **/
    public boolean hasProjects(Collection<DatasetProject> projectSet) {
      boolean hasProjects = true;
      for (DatasetProject dataProj: projectSet) {
	hasProjects = hasProjects && projects.containsKey(dataProj.getProject());
      }
      return hasProjects;
    }

    /**
     * Determine if this dataset contains preliminary data.
     * @return <code>true</code> if the dataset is preliminary, <code>false</code> if
     * is not.
     **/
    public boolean isPreliminary() { return preliminary; }

    /**
     * Merge the specified dataset with this one.
     * @param dataset The dataset to be merged into this dataset.
     * @throws PortException if dataset values do not match.
     **/
    public void merge(Dataset dataset) throws PortException {
      // Dataset ID must be equal to get here!
      
      // Compare the Names
      if (!getName().equals(dataset.getName())) {
	throw new PortException(String.format("Dataset names do not match (%s):\n\t%s\n\t%s",getDatasetId(),getName(),dataset.getName()));
      }
      
      // Compare the URLs
      if (getUrl().equals("")) {
	setUrl(dataset.getUrl());
      } else if (!dataset.getUrl().equals("") && !getUrl().equals(dataset.getUrl())) {
	throw new PortException(String.format("Dataset URLs do not match (%s):\n\t%s\n\t%s",getDatasetId(),getUrl(),dataset.getUrl()));
      }
      
      // Compare the Doc URLS
      if (getDocUrl().equals("")) {
	setDocUrl(dataset.getDocUrl());
      } else if (!dataset.getDocUrl().equals("") && !getDocUrl().equals(dataset.getDocUrl())) {
	throw new PortException(String.format("Dataset Doc URLs do not match (%s):\n\t%s\n\t%s",getDatasetId(),getDocUrl(),dataset.getDocUrl()));
      }
      
      // Compare the Author/PI
      if (getAuthorPi().equals("")) {
	
      } else if (!dataset.getAuthorPi().equals("") && !getAuthorPi().equals(dataset.getAuthorPi())) {
	throw new PortException(String.format("Dataset Author/PIs do not match (%s):\n\t%s\n\t%s",getDatasetId(),getAuthorPi(),dataset.getAuthorPi()));
      }
      
      // Compare the Expected Dates
      if (getDateExpected() == null) {
	setDateExpected(dataset.getDateExpected());
      } else if (dataset.getDateExpected() != null && getDateExpected().compareTo(dataset.getDateExpected()) != 0) {
	throw new PortException(String.format("Dataset Date Expected do not match (%s):\n\t%s\n\t%s",getDatasetId(),getDateExpected(),dataset.getDateExpected()));
      }
      
      // Compare the Updated Dates
      if (getDateUpdated() == null) {
	setDateUpdated(dataset.getDateUpdated());
      } else if (dataset.getDateUpdated() != null && getDateUpdated().compareTo(dataset.getDateUpdated()) != 0) {
	throw new PortException(String.format("Dataset Date Updated do not match (%s):\n\t%s\n\t%s",getDatasetId(),getDateUpdated(),dataset.getDateUpdated()));
      }
      
      // Compare the project information
      for (DatasetProject project: dataset.getProjects()) {
	if (projects.containsKey(project.getProject())) {
	  projects.get(project.getProject()).merge(project);
	} else {
	  projects.put(project.getProject(),project);
	}
      }
    }
    
    /**
     * Set the name of the dataset after cleaning it for arctic.
     * @param name The name of the dataset.
     **/
    public void setArcticName(String name) { setName(cleanArcticName(name)); }
    
    /**
     * Set the author/PI for the dataset.
     * @param authorPi The author/PI for the dataset.
     **/
    public void setAuthorPi(String authorPi) { this.authorPi = authorPi; }

    /**
     * Set the id for the dataset.
     * @param datasetId The dataset's id.
     **/
    public void setDatasetId(String datasetId) { this.datasetId = datasetId; }

    /**
     * Set the expected date for the dataset.
     * @param date The expected date.
     **/
    public void setDateExpected(java.sql.Date date) { dateExpected = date; }

    /**
     * Set the updated date for the dataset.
     * @param date The dataset's updated date.
     **/
    public void setDateUpdated(java.sql.Date date) { dateUpdated = date; }

    /**
     * Set the URL for the dataset's document.
     * @param docUrl The document URL
     **/
    public void setDocUrl(String docUrl) { this.docUrl = docUrl; }

    /**
     * Set the name of of the dataset.
     * @param name The dataset's name.
     **/
    public void setName(String name) { this.name = name; }

    /**
     * Set the flag to mark the dataset as preliminary.
     * @param preliminary <code>true</code> if the dataset is preliminary, <code>false<code>
     * otherwise.
     **/
    public void setPreliminary(boolean preliminary) { this.preliminary = preliminary; }

    /**
     * Set project specific data for the dataset.
     * @param project The project the data is being assigned to.
     * @param date The date the dataset was posted to the project.
     * @param inProgress A flag to mark the dataset in progress.
     * @param hidden A flag to hide the dataset for the project.
     **/
    public void setProjectInformation(Project project, java.sql.Date date, Boolean inProgress, Boolean hidden) {
      DatasetProject dsProj = new DatasetProject(this,project);
      dsProj.setDatePosted(date);
      dsProj.setInProgress(inProgress);
      dsProj.setHidden(hidden);
      
      if (projects.containsKey(project)) {
	projects.get(project).merge(dsProj);
      } else {
	projects.put(project,dsProj);
      }	    
    }

    /**
     * Set the URL for the dataset.
     * @param url The dataset's URL.
     **/
    public void setUrl(String url) { this.url = url; }
  }
  
  /**
   * The DatasetProject class is a container for holding project specific information
   * for a dataset.
   **/
  private class DatasetProject implements Comparable<DatasetProject> {
    
    private Dataset dataset;
    private Project project;
    private java.sql.Date datePosted;
    private Boolean hidden,inProgress;
    
    /**
     * Create a new instance of a DatasetProject.
     * @param dataset The dataset being associated to the project.
     * @param project The project being associated to the dataset.
     **/
    public DatasetProject(Dataset dataset, Project project) {
      this.dataset = dataset;
      this.project = project;
    }
    
    /**
     * Compare the dataset/project information to the specified entry.
     * @param dataset The entry to be compared to this one.
     * @return A negative, zero, or positive integer if this DatasetProject is
     * less than, equal to, or greater than the specified dataset.
     **/
    public int compareTo(DatasetProject dataset) {
      if (getProject().compareTo(dataset.getProject()) == 0) {
	return getDataset().compareTo(dataset.getDataset());
      } else {
	return getProject().compareTo(dataset.getProject());
      }
    }

    /**
     * Get the dataset associated to the project.
     * @return The associated dataset.
     **/
    public Dataset getDataset() { return dataset; }

    /**
     * Get the date the dataset was posted to the project.
     * @return The posted date.
     **/
    public java.sql.Date getDatePosted() { return datePosted; }

    /**
     * Get the project associated to the dataset.
     * @return The associated project.
     **/
    public Project getProject() { return project; }

    /**
     * Determine if the dataset is hidden in the project.
     * @return <code>true</code> if the dataset is hidden, <code>false</code> otherwise.
     **/
    public Boolean isHidden() { return hidden; }
    
    /**
     * Determine if the dataset is in progress for the project.
     * @return <code>true</code> if the dataset is in progress, <code>false</code> otherwise.
     **/
    public Boolean isInProgress() { return inProgress; }
    
    /**
     * Merge the specified DatasetProject into the current one.
     * @param dsProj The dataset/project information to be merged.
     **/
    public void merge(DatasetProject dsProj) {
      if (compareTo(dsProj) != 0) {
	throw new PortException("Trying to merge 2 not equal DatasetProjects!");
      }
      
      if (getDatePosted() == null) {
	setDatePosted(dsProj.getDatePosted());
      } else if (dsProj.getDatePosted() != null && getDatePosted().compareTo(dsProj.getDatePosted()) != 0) {
	throw new PortException(String.format("Dataset/Project Posted Dates are not equals (%s:%s):\n\t%s\n\t%s",getDataset().getDatasetId(),getProject().getProjectId(),getDatePosted(),dsProj.getDatePosted()));
      }
      
      if (isHidden() != dsProj.isHidden()) {
	throw new PortException(String.format("Dataset/Project Hidden Flags are not equal (%s:%s):\n\t%s\n\t%s",getDataset().getDatasetId(),getProject().getProjectId(),isHidden(),dsProj.isHidden()));
      }
      
      if (isInProgress() != dsProj.isInProgress()) {
	throw new PortException(String.format("Dataset/Project In Progress Flags are not equal (%s:%s):\n\t%s\n\t%s",getDataset().getDatasetId(),getProject().getProjectId(),isInProgress(),dsProj.isInProgress()));
      }
    }
    
    /**
     * Set the date the dataset was posted to the project.
     * @param datePosted The date the dataset was posted.
     **/
    public void setDatePosted(java.sql.Date datePosted) {
      this.datePosted = datePosted;
    }

    /**
     * Set the flag to hide the dataset in the project.
     * @param hidden <code>true</code> if the dataset is hidden in the project,
     * <code>false</code> otherwise.
     **/
    public void setHidden(Boolean hidden) { this.hidden = hidden; }

    /**
     * Set the flag to mark the dataset in progress for the project.
     * @param hidden <code>true</code> if the dataset is in progress for the project,
     * <code>false</code> otherwise.
     **/
    public void setInProgress(Boolean inProgress) { this.inProgress = inProgress; }
  }

  /**
   * The Phase class is a container for holding phase information.
   **/
  private class Phase implements Comparable<Phase> {
    
    private Integer id;
    private Project project;
    private String name;
    
    /**
     * Create a new instance of a Phase.
     * @param project The project the phase is a part of.
     * @param name The name of the phase.
     **/
    public Phase(Project project, String name) {
      this.project = project;
      this.name = cleanPhaseName(name);
    }

    /**
     * Clean up the phase name.
     * @param name The name of the phase.
     * @return The cleaned up phase name.
     **/
    public String cleanPhaseName(String name) {
      if (name.equals("I")) { return "Phase I"; }
      else if (name.equals("II")) { return "Phase II"; }
      else if (name.equals("III")) { return "Phase III"; }
      else { return name; }
    }

    /**
     * Compare this phase with the specified phase.
     * @param phase The phase to be compared.
     * @return A negative, zero, or positive number if this phase is less than, equal
     * to, or greater than the specified phase.
     **/
    public int compareTo(Phase phase) {
      int value = getProject().compareTo(phase.getProject());
      if (value == 0) {
	value = getName().compareTo(phase.getName());
      }
      return value;
    }

    /**
     * Get the name of the phase.
     * @return The phase's name.
     **/
    public String getName() { return name; }

    /**
     * Get the unique id for the phase.
     * @return The phase's id.
     **/
    public Integer getPhaseId() { return id; }

    /**
     * Get the project the phase is associated with.
     * @return The phase's project.
     **/
    public Project getProject() { return project; }

    /**
     * Set the unique id for the phase.
     * @param id The phase's id.
     **/
    public void setPhaseId(Integer id) { this.id = id; }
  }

  /**
   * The PortException class is a specialized exception that is thrown during porting
   * when items do not merge correctly.
   **/
  private class PortException extends RuntimeException {
    
    /**
     * Create a new instance of a PortException.
     * @param msg The error message that generated the exception.
     **/
    public PortException(String msg) { super(msg); }
  }

  /**
   * The ProjectMap class is a specialized map for loading, holding, and storing project
   * information for the databases.
   **/
  private class ProjectMap extends TreeMap<String,Project> {
    
    private Connection arctic,general;
    private boolean testPort;
    
    /**
     * Create a new instance of a ProjectMap.
     * @param arctic The connection to use to execute arctic database commands.
     * @param general The connection to use to execute general database commands.
     * @param testPort A flag that marks the porting as a test port.
     * @throws SQLException when there is a problem loading the project information.
     **/
    public ProjectMap(Connection arctic, Connection general, boolean testPort) throws SQLException {
      this.arctic = arctic;
      this.general = general;
      this.testPort = testPort;
      readArcticProjects();
      readGeneralProjects();
    }
    
    /**
     * Associate the arctic Data Types to their projects.
     * @param classes The map that contains the data types.
     * @throws SQLException when there is a problem loading the data types.
     **/
    public void associateArcticDataTypes(ClassMap classes) throws SQLException {
      String sql = "SELECT ListProjects.name,DataType.name FROM ListProjects JOIN DataType ON ListProjects.id=DataType.proj_id";
      PreparedStatement stmt = arctic.prepareStatement(sql);
      ResultSet results = stmt.executeQuery();
      while (results.next()) {
	String name = classes.cleanClassificationName(results.getString(2));
	if (name != null) {
	  get(results.getString(1)).addClassification(classes.get(name));
	}
      }
      results.close();
      stmt.close();
    }

    /**
     * Associate the arctic Disciplines to their projects.
     * @param classes The map that contains the disciplines.
     * @throws SQLException when there is a problem loading the disciplines.
     **/
    public void associateArcticDisciplines(ClassMap classes) throws SQLException {
      String sql = "SELECT ListProjects.name,Discipline.name FROM ListProjects JOIN Discipline ON ListProjects.id=Discipline.proj_id";
      PreparedStatement stmt = arctic.prepareStatement(sql);
      ResultSet results = stmt.executeQuery();
      while (results.next()) {
	String name = classes.cleanClassificationName(results.getString(2));
	if (name != null) {
	  get(results.getString(1)).addClassification(classes.get(name));
	}
      }
      results.close();
      stmt.close();
    }

    /**
     * Associate the arctic Platforms to their projects.
     * @param classes The map that contains the platforms.
     * @throws SQLException when there is a problem loading the platforms.
     **/
    public void associateArcticPlatforms(ClassMap classes) throws SQLException {
      String sql = "SELECT ListProjects.name,Platform.name FROM ListProjects JOIN Platform ON ListProjects.id=Platform.proj_id";
      PreparedStatement stmt = arctic.prepareStatement(sql);
      ResultSet results = stmt.executeQuery();
      while (results.next()) {
	String name = classes.cleanClassificationName(results.getString(2));
	if (name != null) {
	  get(results.getString(1)).addClassification(classes.get(name));
	}
      }
      results.close();
      stmt.close();
    }

    /**
     * Associate the arctic Sites to their projects.
     * @param classes The map that contains the sites.
     * @throws SQLException when there is a problem loading the sites.
     **/
    public void associateArcticSites(ClassMap classes) throws SQLException {
      String sql = "SELECT ListProjects.name,Site.name FROM ListProjects JOIN Site ON ListProjects.id=Site.proj_id";
      PreparedStatement stmt = arctic.prepareStatement(sql);
      ResultSet results = stmt.executeQuery();
      while (results.next()) {
	String name = classes.cleanClassificationName(results.getString(2));
	if (name != null) {
	  get(results.getString(1)).addClassification(classes.get(name));
	}
      }
      results.close();
      stmt.close();
    }
    
    /**
     * Associate the classifications with their projects.
     * @param classes The map that contains the classifications to be associated.
     * @throws SQLException when their is a problem loading the classifications.
     **/
    public void associateClassifications(ClassMap classes) throws SQLException {
      associateArcticDataTypes(classes);
      associateArcticDisciplines(classes);
      associateArcticPlatforms(classes);
      associateArcticSites(classes);
      associateGeneralCategories(classes);
    }
    
    /**
     * Associate the general categories to their projects.
     * @param classes The map that contains the categories.
     * @throws SQLException when there is a problem loading the categories.
     **/
    public void associateGeneralCategories(ClassMap classes) throws SQLException {
      String sql = "SELECT project_id,name,hide_flag FROM category JOIN project_category ON category.category_id=project_category.category_id WHERE category.name != ? AND category.name != ?";
      PreparedStatement stmt = general.prepareStatement(sql);
      stmt.setString(1,"Events");
      stmt.setString(2,"Sites");
      ResultSet results = stmt.executeQuery();
      while (results.next()) {
	String name = classes.cleanClassificationName(results.getString(2));
	if (name != null ) {
	  // Replace Profiler/SODAR with 2 seperate categories.
	  if (name.equals("Profiler/SODAR")) {
	    get(results.getString(1)).addClassification(classes.get("Profiler"),results.getBoolean(3));
	    get(results.getString(1)).addClassification(classes.get("SODAR"),results.getBoolean(3));
	  } else { 
	    get(results.getString(1)).addClassification(classes.get(name),results.getBoolean(3));
	  }
	}
      }
      results.close();
      stmt.close();
    }

    /**
     * Associate the arctic phases to their projects.
     * @throws SQLException when there is a problem loading the phases.
     **/
    public void associatePhases() throws SQLException {
      String sql = "SELECT DISTINCT(phase),ListProjects.name FROM ListProjects JOIN List ON ListProjects.id=List.proj_id AND phase IS NOT NULL AND phase != ?";
      PreparedStatement stmt = arctic.prepareStatement(sql);
      stmt.setString(1,"");
      ResultSet results = stmt.executeQuery();
      while (results.next()) {
	get(results.getString(2)).addPhase(results.getString(1));
      }
      results.close();
      stmt.close();
    }
    
    /**
     * Merge the specified project with this project.
     * @param project The project to be merged into the map.
     **/
    public void merge(Project project) {
      if (containsKey(project.getProjectId())) {
	get(project.getProjectId()).merge(project);
      } else {
	put(project.getProjectId(),project);
      }
    }

    /**
     * Load the arctic project information from the database.
     * @throws SQLException when there is a problem reading the arctic information.
     **/
    public void readArcticProjects() throws SQLException {
      String sql = "SELECT name FROM ListProjects";
      PreparedStatement stmt = arctic.prepareStatement(sql);
      ResultSet results = stmt.executeQuery();
      while (results.next()) {
	Project project = new Project();
	project.setProjectId(results.getString(1));
	merge(project);
      }
      results.close();
      stmt.close();
    }

    /**
     * Load the general project information from the database.
     * @throws SQLException when there is a problem reading the general information.
     **/
    public void readGeneralProjects() throws SQLException {
      String sql = "SELECT project_id,system_directory,url,home_page_url,logo_url FROM project";
      PreparedStatement stmt = general.prepareStatement(sql);
      ResultSet results = stmt.executeQuery();
      while (results.next()) {
	Project project = new Project();
	project.setProjectId(results.getString(1));
	project.setSystemDirectory(results.getString(2));
	project.setUrl(results.getString(3));
	project.setHomePageUrl(results.getString(4));
	project.setLogoUrl(results.getString(5));
	merge(project);
      }
      results.close();
      stmt.close();
    }
    
    /**
     * Store the project information in the map into the database.
     * @param conn The connection to use to execute the statements.
     * @param classes The map of classifications associated to projects.
     * @throws SQLException when there is a problem storing the information to the database.
     **/
    public void store(Connection conn, ClassMap classes) throws SQLException {
      for (Project project: values()) {
	storeProject(conn,project);
	storeProjectPhases(conn,project);
	storeProjectClassifications(conn,project,classes);
      }
    }
    
    /**
     * Store the specified project into the database.
     * @param conn The connection to use to execute the statements.
     * @param project The project to be inserted into the database.
     * @throws SQLException when there is a problem inserting the project.
     **/
    public void storeProject(Connection conn, Project project) throws SQLException {
      String sql = "INSERT INTO project(project_id,url,system_directory,home_page_url,logo_url,css_url) VALUES(?,?,?,?,?,?)";
      PreparedStatement stmt = conn.prepareStatement(sql);
      stmt.setString(1,project.getProjectId());
      if (testPort) {
	String url = project.getUrl();
	url = url.replaceAll("data\\.eol","data.dev.eol");
	stmt.setString(2,url);
      } else {
	stmt.setString(2,project.getUrl());
      }
      if (testPort) {
	String dir = project.getSystemDirectory();
	dir = dir.replaceAll("^\\/export\\/web","/net/www_dev");
	stmt.setString(3,dir);
      } else {
	stmt.setString(3,project.getSystemDirectory());
      }
      stmt.setString(4,project.getHomePageUrl());
      stmt.setString(5,project.getLogoUrl());
      stmt.setString(6,project.getCssUrl());
      stmt.executeUpdate();
      stmt.close();
    }
    
    /**
     * Associate the project with its classifications in the database.
     * @param conn The connection to execute the statements.
     * @param project The project to associate its classifications.
     * @param classes The map of all classifications.
     * @throws SQLException when there is a problem associating the classifications.
     **/
    public void storeProjectClassifications(Connection conn, Project project, ClassMap classes) throws SQLException {
      if (project.contains(classes.get("Profiler")) || project.contains(classes.get("SODAR")) ||
	  project.contains(classes.get("Radiosonde"))) {
	project.addClassification(classes.get("Upper Air"));
      }
      String sql = "INSERT INTO project_classification(project_id,class_id,hide_flag) VALUES(?,?,?)";
      PreparedStatement stmt = conn.prepareStatement(sql);
      stmt.setString(1,project.getProjectId());
      
      for (Classification classification: project.getClassifications()) {
	stmt.setInt(2,classification.getClassificationId());
	stmt.setBoolean(3,project.isHidden(classification));
	stmt.executeUpdate();
      }
      stmt.close();
    }
    
    /**
     * Associate the project with its phases in the database.
     * @param conn The connection to use to execute the commands.
     * @param project The project to associate its phases.
     * @throws SQLException when there is a problem inserting the phases.
     **/
    public void storeProjectPhases(Connection conn, Project project) throws SQLException {
      String sql = "INSERT INTO phase(project_id,name) VALUES(?,?)";
      PreparedStatement stmt = conn.prepareStatement(sql,Statement.RETURN_GENERATED_KEYS);
      stmt.setString(1,project.getProjectId());
      
      for (Phase phase: project.getPhases()) {
	stmt.setString(2,phase.getName());
	stmt.executeUpdate();
	
	// Read in the auto-generated phase id
	ResultSet key = stmt.getGeneratedKeys();
	key.next();
	phase.setPhaseId(key.getInt(1));
	key.close();
      }
      stmt.close();
    }
  }

  /**
   * The Project class is a container for project information.
   **/
  private class Project implements Comparable<Project> {

    private Map<Classification,Boolean> classes;
    private Map<String,Phase> phases;
    private String projectId,systemDirectory,url,homePageUrl,logoUrl,cssUrl;
    
    /**
     * Create a new project.
     **/
    public Project() {
      classes = new TreeMap<Classification,Boolean>();
      phases = new TreeMap<String,Phase>();
    }

    /**
     * Associated the specified classification with the project.
     * @param classification The classification to associate with the project.
     **/
    public void addClassification(Classification classification) {
      addClassification(classification,Boolean.FALSE);
    }

    /**
     * Associated the specified classification with the project.
     * @param classification The classification to associate with the project.
     * @param hidden <code>true</code> if the classification is hidden in the project,
     * <code>false</code> otherwise.
     **/
    public void addClassification(Classification classification, Boolean hidden) {
      classes.put(classification,hidden);
    }

    /**
     * Assoiate the specified phase with the project.
     * @param phaseName The name of the phase.
     **/
    public void addPhase(String phaseName) {
      Phase phase = new Phase(this,phaseName);
      phases.put(phaseName,phase);
    }

    /**
     * Compare the specified project with this one.
     * @param project The project to be compared with this project.
     * @return A negative, zero, or positive integer if this project is less than, equal to,
     * or greater than the specified project.
     **/
    public int compareTo(Project project) {
      return getProjectId().toLowerCase().compareTo(project.getProjectId().toLowerCase());
    }
    
    /**
     * Determine if the specified classification is associated with this project.
     * @param classification The classification to be tested.
     * @return <code>true</code> if the classification is associated with the project,
     * <code>false</code> otherwise.
     **/
      public boolean contains(Classification classification) {
	return classes.containsKey(classification);
      }
	
    /**
     * Get the set of associated classifications for the project.
     * @return The project's classifications.
     **/
    public Set<Classification> getClassifications() { return classes.keySet(); }

    /**
     * Get the URL for the project's CSS.
     * @return The projects' CSS URL.
     **/
    public String getCssUrl() { return cssUrl == null ? "" : cssUrl; }

    /**
     * Get the URL of the project's home page.
     * @return The project's home page URL.
     **/
    public String getHomePageUrl() { 
      return homePageUrl == null ? "http://www.eol.ucar.edu/projects/"+getProjectId().toLowerCase() : homePageUrl; 
    }

    /**
     * Get the URL of the project's logo.
     * @return The project's logo.
     **/
    public String getLogoUrl() { return logoUrl == null ? "" : logoUrl; }

    /**
     * Get the phase associated with the project.
     * @param name The name of the phase.
     **/
    public Phase getPhase(String name) { return phases.get(name); }

    /**
     * Get the set of associated phases for the project.
     * @return The project's phases.
     **/
    public Set<Phase> getPhases() { return new TreeSet<Phase>(phases.values()); }

    /**
     * Get the id for the project.
     * @return The project's id.
     **/
    public String getProjectId() { return projectId; }

    /**
     * Get the project's system directory.
     * @return The project's system directory.
     **/
    public String getSystemDirectory() { 
      return systemDirectory == null ? "/net/web/data/html/master_lists/static/"+getProjectId().toLowerCase() : systemDirectory; 
    }
    
    /**
     * Get the Master List URL for the project.
     * @return The project's URL.
     **/
    public String getUrl() { 
      return url == null ? "http://data.eol.ucar.edu/master_list/?project="+getProjectId() : url; 
    }

    /**
     * Determine if the specified classification is hidden in the project.
     * @param classification The classification to be tested.
     * @return <code>true</code> if the classification is hidden, <code>false</code>
     * otherwise.
     **/
    public Boolean isHidden(Classification classification) {
      return classes.get(classification);
    }

    /**
     * Merge the specified project with this project.
     * @param project The project to be merged.
     **/
    public void merge(Project project) {
      throw new UnsupportedOperationException("Project merge not yet implemented.");
    }

    /**
     * Set the URL for the project's CSS.
     * @param cssUrl The CSS URL for the project.
     **/
    public void setCssUrl(String cssUrl) { this.cssUrl = cssUrl; }
    
    /**
     * Set the URL for the home page for the project.
     * @param homePageUrl The project's home page URL.
     **/
    public void setHomePageUrl(String homePageUrl) { this.homePageUrl = homePageUrl; }

    /**
     * Set the URL for the logo for the project.
     * @param logoUrl The project's logo URL.
     **/
    public void setLogoUrl(String logoUrl) { this.logoUrl = logoUrl; }

    /**
     * Set the id for the project.
     * @param projectId The project's id.
     **/
    public void setProjectId(String projectId) { this.projectId = projectId; }

    /**
     * Set the system directory for the project.
     * @param dir The project's system directory.
     **/
    public void setSystemDirectory(String dir) { systemDirectory = dir; }

    /**
     * Set the URL for the project.
     * @param url The project's URL.
     **/
    public void setUrl(String url) { this.url = url; }
  }
}
