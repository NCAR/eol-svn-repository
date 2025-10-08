package dmg.ml.bean;

import dmg.jsf.TreeException;
import dmg.jsf.model.TreeNode;
import dmg.ml.DatabaseAccessBean;
import dmg.ml.MasterListException;
import dmg.ml.manager.state.TreeState;
import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.ResourceBundle;
import javax.faces.event.ActionEvent;

/**
 * <p>The ProjectBean class is the basic representation of a Project in the
 * Master List.  It contains:</p>
 * <ul>
 *    <li>Project Id- Unique identifier for the project.</li>
 *    <li>URL- The URL where the Master List can be accessed.
 *    <li>System Directory- The physical directory where the Master List files
 * are stored and written.</li>
 *    <li>Home Page URL- The URL where the project's home page is located.</li>
 *    <li>Logo URL- The URL where the projects's logo is located.</li>
 * </ul>
 *
 * @author Joel Clawson
 */
public class ProjectBean extends MasterListBean<ProjectBean> {
    
    private List<ClassificationBean> classifications;
    private String cssUrl,homePageUrl,logoUrl,projectId,systemDirectory,url,originalId;
    
    /**
     * Create a new instance of a ProjectBean.
     **/
    public ProjectBean() { this(null); }
    
    /**
     * Create a new instance of a ProjectBean.
     * @param state A container for the expansion state for the TreeNode.
     **/
    public ProjectBean(TreeState state) { super(state); }
    
    /**
     * Compare this ProjectBean to another ProjectBean.
     * @param project The project to be compared to this project.
     * @return A negative integer, zero, or postive integer if this project
     * is less than, equal to, or greater than the specified project.
     **/
    public int compareTo(ProjectBean project) {
	return getProjectId().compareTo(project.getProjectId());
    }

    /**
     * Delete the project from the database.  This includes removing all
     * associations with data sets (and removing all data set metadata if it
     * is only association with this project) and all classification associations.
     * @param conn The connection to use to delete the project.
     * @throws MasterListException when there is a problem deleting the project
     * form the database.
     **/
    public void delete(Connection conn) throws MasterListException {
	List<DatasetProjectBean> datasets = new ArrayList<DatasetProjectBean>();
	// Get the list of data sets associated with this project.
	try {
	    String sql = "SELECT dataset_id FROM dataset_project WHERE project_id=?";
	    PreparedStatement stmt = conn.prepareStatement(sql);
	    stmt.setString(1,getProjectId());
	    ResultSet results = stmt.executeQuery();
	    while (results.next()) {
		datasets.add(DatasetProjectBean.loadDataset(results.getString(1),this));
	    }
	} catch (SQLException e) {
	    throw new MasterListException("Unable to load datasets to be "+
					  "deleted for project: "+getProjectId(),e);
	}
	
	// Delete the data set for this project.
	for (Iterator<DatasetProjectBean> itr = datasets.iterator(); itr.hasNext(); ) {
	    itr.next().delete(conn);
	}
            
	// Delete the project (includes classification associations)
	try {
	    String sql = "DELETE FROM project WHERE project_id=?";
	    PreparedStatement stmt = conn.prepareStatement(sql);
	    stmt.setString(1,getProjectId());
	    stmt.execute();
	} catch (SQLException e) {
	    // Convert the SQLException to a MasterListException
	    throw new MasterListException("Unable to delete project: "+getProjectId(),e);
	}
    }

    /**
     * Determine if the specified object is the same as this project.
     * @param item The item to compare to this project.
     * @return <code>true</code> if the item is a ProjectBean and has the same
     * project id as this project, <code>false</code> otherwise.
     **/
    public boolean equals(Object item) {
        try {
            return getProjectId().equals(((ProjectBean)item).getProjectId());
        } catch (ClassCastException e) { return false; }
    }

    /**
     * Get the list of children for the Project.
     * @return An empty list.
     * @throws TreeException should never occur, but is required from the 
     * TreeNode interface definition.
     **/
    public List<TreeNode> getChildren() throws TreeException {
        return new ArrayList<TreeNode>();
    }
    
    /**
     * Get the name of the facet to be displayed when the node is closed/not
     * displaying its children.
     * @return The name of the closed facet for the node.
     **/
    protected String getClosedFacetName() { return "closedProject"; }

    /**
     * Get the URL of the stylesheet used by this project.
     * @return The project's css URL.
     **/
    public String getCssUrl() { return cssUrl; }
    
    /**
     * Get the URL of the home page for the project.
     * @return The project's home page URL.
     **/
    public String getHomePageUrl() { return homePageUrl; }

    /**
     * Get a unique identifier for this TreeNode.
     * @return The unique identifier for the node.
     **/
    public Integer getId() { 
        return new Integer(getProjectId() == null ? "null".hashCode() :
            getProjectId().hashCode());
    }
    
    /**
     * Get the name of the facet to be displayed when the node is a leaf node.
     * @return The name of the leaf facet to be displayed.
     **/
    protected String getLeafFacetName() { return "leafProject"; }
    
    /**
     * Get the URL of the logo for the project to be used by the Master List.
     * @return The URL for the project's logo.
     **/
    public String getLogoUrl() { return logoUrl; }

    /**
     * Get the name of the facet to be displayed when the node is open (when it
     * is displaying its children).
     * @return The name of the open facet for the node.
     **/
    protected String getOpenFacetName() { return "openProject"; }
    
    /**
     * Get the original project identifier for the project.
     * @return The original project identifier.
     **/
    public String getOriginalId() { return originalId; }
    
    /**
     * Get the unique identifier for the project.
     * @return The project id for the current project.
     **/
    public String getProjectId() { return projectId; }
    
    /**
     * Get the list of all of the projects in the database.
     * @return The list of projects in the database.
     * @throws MasterListException when there is a problem loading the projects
     * from the database.
     **/
    public static List<ProjectBean> getProjectList() throws MasterListException {
        List<ProjectBean> projectList = new ArrayList<ProjectBean>();
        
        // Get a connection to the database from the connection pool.
        Connection conn = DatabaseAccessBean.getConnection();
        
        try {
            // Prepare the statement and execute it.
            String sql = "SELECT project_id,url,system_directory,css_url,"+
		"home_page_url,logo_url FROM project ORDER BY project_id";
            PreparedStatement stmt = conn.prepareStatement(sql);
            ResultSet results = stmt.executeQuery();

            // Create a new project and add it to the list.
            while (results.next()) {
                ProjectBean project = new ProjectBean();
                project.setProjectId(results.getString(1));
                project.setOriginalId(results.getString(1));
                project.setUrl(results.getString(2));
                project.setSystemDirectory(results.getString(3));
		project.setCssUrl(results.getString(4));
                project.setHomePageUrl(results.getString(5));
                project.setLogoUrl(results.getString(6));
                projectList.add(project);
            }
            
            // Properly close the open streams.
            results.close();
            stmt.close();
        } catch (SQLException e) {
            // Convert the SQLException to a MasterListException.
            throw new MasterListException("Unable to load the list of" +
                    " projects.",e);
        }
        
        return projectList;
    }
   
    /**
     * Get the list of projects associated with the specified classification.
     * @param classification The classification to find the associated projects for.
     * @return The list of projects associated with the specified classification.
     * @throws MasterListException when there is a problem accessing the list of
     * projects for the classification.
     **/ 
    public static List<ProjectBean> getProjectList(ClassificationBean classification) throws MasterListException {
        List<ProjectBean> projectList = new ArrayList<ProjectBean>();

        // Get a connection to the database from the connection pool.
        Connection conn = DatabaseAccessBean.getConnection();

        try {
            // Prepare the statement and execute it.
            String sql = "SELECT project.project_id,url,system_directory,css_url,home_page_url,logo_url FROM project JOIN project_classification ON project.project_id=project_classification.project_id WHERE class_id=?";
            PreparedStatement stmt = conn.prepareStatement(sql);
            stmt.setInt(1,classification.getClassificationId());
            ResultSet results = stmt.executeQuery();
  
            // Create a new project and add it to the list for each result returned.
            while (results.next()) {
                ProjectBean project = new ProjectBean();
                project.setProjectId(results.getString(1));
                project.setOriginalId(results.getString(1));
                project.setUrl(results.getString(2));
                project.setSystemDirectory(results.getString(3));
                project.setCssUrl(results.getString(4));
                project.setHomePageUrl(results.getString(5));
                project.setLogoUrl(results.getString(6));
                projectList.add(project);
            }

            // Properly close down the open streams.
            results.close();
            stmt.close();
        } catch (SQLException e) {
            // Convert the SQLException to a MasterListException.
            throw new MasterListException("Unable to load the list of projects for classification: "+classification.toString(),e);
        }

        return projectList;
    }
    
    /**
     * Get the list of projects associated with the specified data set.
     * @param dataset The data set that is having its projects retreived.
     * @return The list of projects for the specified data set.
     * @throws MasterListException when there is a problem loading the projects
     * for the data set.
     **/
    public static List<ProjectBean> getProjectList(DatasetBean dataset) throws MasterListException {
        List<ProjectBean> projectList = new ArrayList<ProjectBean>();

        // Get a connection from the connection pool.
        Connection conn = DatabaseAccessBean.getConnection();

        try {
            String sql = "SELECT project.project_id,url,system_directory,css_url,home_page_url,logo_url FROM project JOIN dataset_project ON project.project_id=dataset_project.project_id WHERE dataset_id=?";
            PreparedStatement stmt = conn.prepareStatement(sql);
            stmt.setString(1,dataset.getDatasetId());
            ResultSet results = stmt.executeQuery();

            while (results.next()) {
                ProjectBean project = new ProjectBean();
                project.setProjectId(results.getString(1));
                project.setOriginalId(results.getString(1));
                project.setUrl(results.getString(2));
                project.setSystemDirectory(results.getString(3));
                project.setCssUrl(results.getString(4));
                project.setHomePageUrl(results.getString(5));
                project.setLogoUrl(results.getString(6));
                projectList.add(project);
            }

            results.close();
            stmt.close();
        } catch (SQLException e) {
            throw new MasterListException("Unable to load the list of projects for data set: "+dataset.toString(),e);
        }

        return projectList;
    }
    
    /**
     * Get the directory where the output files for the Master List are to 
     * stored.
     * @return The physical location of the Master List files.
     **/
    public String getSystemDirectory() { return systemDirectory; }
    
    /**
     * Get the URL where the project's Master List can be accessed.
     * @return The Master List's URL for the project.
     **/
    public String getUrl() { return url; }
    
    /**
     * Insert the project into the Master List database including associating
     * its classifications.
     * @param conn The connection to use to execute the SQL statements.
     * @throws MasterListException when there is a problem inserting the project
     * into the database.
     **/
    public void insert(Connection conn) throws MasterListException {
        // Make sure the values for the project are valid
        testProjectValidity();
        
        try {
            // Prepare the statement and insert the project into the database.
            String sql = "INSERT INTO project(project_id,url,system_directory,home_page_url,logo_url,css_url) VALUES(?,?,?,?,?,?)";
            PreparedStatement stmt = conn.prepareStatement(sql);
            stmt.setString(1,getProjectId());
            stmt.setString(2,getUrl());
            stmt.setString(3,getSystemDirectory());
            stmt.setString(4,getHomePageUrl());
            stmt.setString(5,getLogoUrl());
	    stmt.setString(6,getCssUrl());
            stmt.execute();
            
            // Properly close the statement stream.
            stmt.close();            
        } catch (SQLException e) {
            // Convert the SQLException to a MasterListException.
            throw new MasterListException("Unable to insert the project: "+
                    getProjectId()+".",e);
        }

        // Now that the dataset has been inserted, associate its classifications.
        updateClassifications(conn,classifications);
    }
    
    /**
     * Get a project from the database.
     * @param projectId The project to be loaded from the database.
     * @return The project loaded from the database.
     * @throws MasterListException if there is a problem reading the data from
     * the database or if the project does not exist in the database.
     **/
    public static ProjectBean loadProject(String projectId) throws MasterListException {
        ProjectBean project = null;
        
        // Get a connection to the database from the connection pool.
        Connection conn = DatabaseAccessBean.getConnection();
        try {
            // Prepare the statement and read it from the database.
            String sql = "SELECT project_id,url,system_directory,home_page_url,logo_url,css_url FROM project WHERE project_id=?";
            PreparedStatement stmt = conn.prepareStatement(sql);
            stmt.setString(1,projectId);
            ResultSet results = stmt.executeQuery();
            
            // Load the project into a bean
            if (results.next()) {
                project = new ProjectBean();
                project.setProjectId(results.getString(1));
                project.setOriginalId(results.getString(1));
                project.setUrl(results.getString(2));
                project.setSystemDirectory(results.getString(3));
                project.setHomePageUrl(results.getString(4));
                project.setLogoUrl(results.getString(5));
		project.setCssUrl(results.getString(6));
            }
            
            // Close down the open streams.
            results.close();
            stmt.close();
        } catch (SQLException e) {
            throw new MasterListException("Unable to load the project: "+
                    projectId+".",e);
        }
                
        // Check to see if the project was loaded
        if (project == null) {
            throw new MasterListException("Cannot load project: "+projectId+"."+
                    "  The project cannot be found in the database.");
        } else {
            return project;
        }
    }
    
    /**
     * Set the list of classifications that are to be associated with this project.
     * @param classes The list of classifications to associate with the project.
     **/
    public void setClassifications(List<ClassificationBean> classes) {
        this.classifications = classes;
    }

    /**
     * Set the URL for the stylesheet that is to be used by this project.
     * @param cssUrl The project's CSS URL.
     **/
    public void setCssUrl(String cssUrl) {
        String oldUrl = this.cssUrl;
        this.cssUrl = cssUrl;
        firePropertyChange("css_url",oldUrl,this.cssUrl);
    }

    /**
     * Set the default stylesheet for the project.  This requires the project ID
     * to be set, otherwise the stylesheet will not be set.
     * @param evt The event that triggered the setting of the value.
     **/
    public void setDefaultCssUrl(ActionEvent evt) {
	if (getProjectId() == null || getProjectId().equals("")) { return; }
	else {
	    String url = ResourceBundle.getBundle("resources").getString("defaultCssUrl");
	    if (!url.endsWith("/")) { url += "/"; }
	    url += getProjectId().toLowerCase()+".css";
	    setCssUrl(url);
	}
    }
    
    /**
     * Set the default home page for the project.  This requires the project ID
     * to be set, otherwise the home page will not be set.
     * @param evt The event that triggered the setting of the value.
     **/
    public void setDefaultHomePageUrl(ActionEvent evt) {
        if (getProjectId() == null || getProjectId().equals("")) { return; }
        else {
            String url = ResourceBundle.getBundle("resources").
                    getString("defaultHomePageUrl");
            if (!url.endsWith("/")) { url += "/"; }
            url += getProjectId().toLowerCase();
            setHomePageUrl(url);
        }
    }
    
    /**
     * Set the default system directory for the project.  This requires the
     * project ID to be set, otherwise the system directory will not be set.
     * @param evt The event that triggered the setting of the system directory.
     **/
    public void setDefaultSystemDirectory(ActionEvent evt) {
        if (getProjectId() == null || getProjectId().equals("")) { return; }
        else {
            String dir = ResourceBundle.getBundle("resources").
                    getString("defaultSystemDirectory");
            if (!dir.endsWith("/")) { dir += "/"; }
            dir += getProjectId().toLowerCase();
            setSystemDirectory(dir);
        }
    }
    
    /**
     * Set the default URL for the project's Master List.  This requires the
     * project ID to be set, otherwise the URL will not be set.
     * @param evt The event that triggered the setting of the URL.
     **/
    public void setDefaultUrl(ActionEvent evt) {
        if (getProjectId() == null || getProjectId().equals("")) { return; }
        else {
            String url = ResourceBundle.getBundle("resources").
                    getString("defaultProjectUrl");
            url += getProjectId();
            setUrl(url);
        }
    }
    
    /**
     * Set the URL for the home page for the project.
     * @param homePageUrl The URL for the project's home page.
     **/
    public void setHomePageUrl(String homePageUrl) {
        String oldUrl = this.homePageUrl;
        this.homePageUrl = homePageUrl;
        firePropertyChange("home_page_url",oldUrl,this.homePageUrl);
    }
    
    /**
     * Set the URL for the logo for the project.
     * @param logoUrl The URL for the project's logo.
     **/
    public void setLogoUrl(String logoUrl) {
        String oldUrl = this.logoUrl;
        this.logoUrl = logoUrl;
        firePropertyChange("logo_url",oldUrl,this.logoUrl);
    }
    
    /**
     * Set the original project identifier for the project.
     * @param originalId The original project identifier.
     **/
    public void setOriginalId(String originalId) {
        String oldId = this.originalId;
        this.originalId = originalId;
        firePropertyChange("original_id",oldId,this.originalId);
    }

    /**
     * Set the identifier for the project.
     * @param projectId The new project id for the project.
     **/
    public void setProjectId(String projectId) {
        String oldId = this.projectId;
        this.projectId = projectId;
        firePropertyChange("project_id",oldId,this.projectId);
    }
    
    /**
     * Set the directory on the system where the output files are to be written.
     * @param systemDirectory The physical location of the project's master
     * list files.
     **/
    public void setSystemDirectory(String systemDirectory) {
        String oldDirectory = this.systemDirectory;
        this.systemDirectory = systemDirectory;
        firePropertyChange("system_directory",oldDirectory,this.systemDirectory);
    }
    
    /**
     * Set the URL where the project's Master List can be accessed.
     * @param url The project's Master List URL.
     **/
    public void setUrl(String url) {
        String oldUrl = this.url;
        this.url = url;
        firePropertyChange("url",oldUrl,this.url);
    }
    
    /**
     * Test the project values to make sure that they are valid and can be
     * handled correctly in the database.
     * @throws MasterListException if any value in the project is not valid.
     **/
    private void testProjectValidity() throws MasterListException {        
        // Make sure required values contain non-empty values.
        if (getProjectId() == null || getProjectId().equals("")) {
            throw new MasterListException("Project ID cannot be empty.");
        }
        if (getSystemDirectory() == null || getSystemDirectory().equals("")) {
            throw new MasterListException("System Directory cannot be empty.");
        }
        if (getUrl() == null || getUrl().equals("")) {
            throw new MasterListException("URL cannot be emtpy.");
        }
        if (getHomePageUrl() == null || getHomePageUrl().equals("")) {
            throw new MasterListException("Home Page URL cannot be empty.");
        }
        if (getLogoUrl() == null || getLogoUrl().equals("")) {
            throw new MasterListException("Logo URL cannot be empty.");
        }
        if (classifications.isEmpty()) {
            throw new MasterListException("Project must have at least one "+
					  "classification association.");
        }

        // Make sure the system directory exists on the system.
        File systemDir = new File(getSystemDirectory());
        if (!systemDir.isDirectory()) {
            throw new MasterListException("System Directory: "+
                    getSystemDirectory()+" cannot be found.");
        }
        
        // Make sure the URL is valid.
        try {
            URL url = new URL(getUrl());
        } catch (MalformedURLException e) {
            throw new MasterListException("Url: "+getUrl()+
                    " is not a valid URL.",e);
        }
        
        // Make sure the Home Page URL is valid.
        try {
            URL homePage = new URL(getHomePageUrl());
            homePage.getContent();
        } catch (MalformedURLException e) {
            throw new MasterListException("Home Page Url: "+getHomePageUrl()+
                    " is not a valid URL.",e);
        } catch (IOException e) {
            throw new MasterListException("Unable to access project home page "+
                    "at: "+getHomePageUrl(),e);
        }
        
        // Make sure the Logo URL is valid
        try {
            URL logo = new URL(getLogoUrl());
            logo.getContent();
        } catch (MalformedURLException e) {
            throw new MasterListException("Logo Url: "+getLogoUrl()+
                    " is not a valid URL.",e);
        } catch (IOException e) {
            throw new MasterListException("Unable to access project logo at: "+
                    getLogoUrl(),e)     ;       
        }

	// Make sure the CSS URL is valid.
    }
    
    /**
     * Get the String representation for the project.
     * @return The project's project identification value.
     **/
    public String toString() { return getProjectId(); }
    
    /**
     * Update the project in the database and associate/disassociated any 
     * classifications.
     * @param conn The connection to use to execute the SQL statements.
     * @throws MasterListException if there is a problem updating the project.
     **/
    public void update(Connection conn) throws MasterListException {
        testProjectValidity();
        
        try {
            // Prepare the statement and read it from the database.
            String sql = "UPDATE project SET project_id=?,url=?,system_directory=?,home_page_url=?,logo_url=?,css_url=? WHERE project_id=?";
            PreparedStatement stmt = conn.prepareStatement(sql);
            stmt.setString(1,getProjectId());
            stmt.setString(2,getUrl());
            stmt.setString(3,getSystemDirectory());
            stmt.setString(4,getHomePageUrl());
            stmt.setString(5,getLogoUrl());
	    stmt.setString(6,getCssUrl());
            stmt.setString(7,getOriginalId());
            stmt.execute();
            
            // Propertly close down the statement stream.
            stmt.close();
        } catch (SQLException e) {
            // Convert an SQLException into a MasterListException
            throw new MasterListException("Unable to update the project: "+
                    getOriginalId()+".",e);
        }
        
        // Associate/Disassociate the classifications with the data set.
        updateClassifications(conn,classifications);
    }
    
    /**
     * Associate the classifications in the list with the project and remove any
     * associations for classifications not in the list.
     * @param conn The connection to use to execute the statements.
     * @param classes The list of classifications to associate with the project.
     * @throws MasterListException when there is a problem associating a
     * classification with the project.
     **/
    protected void updateClassifications(Connection conn, List<ClassificationBean> classes) throws MasterListException {
        
        // Get the hidden status of the classifications so they can be reassigned
        // on the insert later.
        HashMap<Integer,Boolean> hideFlags = new HashMap<Integer,Boolean>();
        try {
            String sql = "SELECT class_id,hide_flag FROM project_classification WHERE project_id=?";
            PreparedStatement stmt = conn.prepareStatement(sql);
            stmt.setString(1,getProjectId());
            ResultSet results = stmt.executeQuery();
            while (results.next()) {
                hideFlags.put(results.getInt(1),results.getBoolean(2));
            }
            results.close();
            stmt.close();
        } catch (SQLException e) {
            throw new MasterListException("Unable to get classification hide status "+
                    "flags for project: "+getProjectId(),e);
        }
        
        // Remove all previous classification associates for the project.
        try {
            String sql = "DELETE FROM project_classification WHERE project_id=?";        
            PreparedStatement stmt = conn.prepareStatement(sql);
            stmt.setString(1,getProjectId());
            stmt.execute();
            stmt.close();
        } catch (SQLException e) {
            throw new MasterListException("Unable to remove previous classification "+
                    "associations with project: "+getProjectId(),e);
        }
        
        // Add the associations for the classifications to the project.
        try {
            String sql = "REPLACE INTO project_classification(project_id,class_id,hide_flag) VALUES(?,?,?)";
            PreparedStatement stmt = conn.prepareStatement(sql);
            while (!classes.isEmpty()) {
                ClassificationBean classification = classes.remove(0);
                stmt.setString(1,getProjectId());
                stmt.setInt(2,classification.getClassificationId());
                stmt.setBoolean(3,hideFlags.get(classification.getClassificationId()) == null ? false : hideFlags.get(classification.getClassificationId()));
                stmt.execute();
                
                // Add the parent classification to the list to be associated with the
                // project
                if (classification.hasParentClassification()) {
                    for (ClassificationBean parent: classification.getParentClassifications()) {
                        classes.add(ClassificationBean.loadClassification(parent.getClassificationId(),conn));
                    }
                }
            }
            stmt.close();
        } catch (SQLException e) {
            throw new MasterListException("Unable to associate the classifications "+
					  "with project: "+getProjectId(),e);
        }
    }
}
