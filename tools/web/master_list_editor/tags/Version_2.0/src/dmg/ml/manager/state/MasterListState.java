package dmg.ml.manager.state;

import dmg.jsf.model.TreeNode;
import dmg.ml.GeneralDatabaseAccessBean;
import dmg.ml.MasterListException;
import dmg.ml.bean.general.CategoryBean;
import dmg.ml.bean.general.CategoryProjectBean;
import dmg.ml.bean.general.DatasetBean;
import dmg.ml.bean.general.DatasetProjectBean;
import dmg.ml.bean.general.ProjectBean;
import dmg.ml.bean.general.ProjectCategoryTreeRootBean;
import dmg.ml.bean.general.ProjectDatasetListRootBean;
import dmg.ml.manager.GeneralManager;
import dmg.ml.manager.Selector;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.ResourceBundle;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.faces.component.UIComponent;
import javax.faces.component.UIParameter;
import javax.faces.event.ActionEvent;

/**
 * <p>The MasterListState is an abstract state used by a Master List manager. 
 * It defines the functions needed to be able to properly change to the next
 * state (which may be an update of the current state).</p>
 * <p>It is the <code>State</code> class in the <b>State</b> pattern defined
 * in <u>Design Patterns</u> book by Gamma, et al.</p>
 *
 * @author Joel Clawson
 */
public abstract class MasterListState {
    
    // The name of the file that contains the list of the data sets for the
    // public view.
    private static final String DATASET_LIST = "dataset_list.html";
    
    // The name of the file that is the main index for a project's Master List.
    private static final String INDEX_FILE = "index.html";
    
    // The prefix of the name of the file for the category menus for the
    // public view.
    private static final String MENU_PREFIX = "menu";

    /**
     * The manager that is using this state.
     **/
    protected GeneralManager manager;
    
    /**
     * Create a new instance of a MasterListState.
     * @param manager The manager that is using this state.
     **/
    public MasterListState(GeneralManager manager) { this.manager = manager; }
    
    /**
     * Save all information that is in this state to the database.  This could
     * be an insert, update, or delete depending on the state.
     * @param evt The event that triggered the state acceptance.
     * @return The state of the Master List after a successful completion of the
     * accept, or the current state if the function is not defined for the 
     * current state.
     * @throws MasterListException if there is a problem in the execution of the
     * accept process of the state.
     **/
    public abstract MasterListState accept(ActionEvent evt)
        throws MasterListException;

    /**
     * Cancel all changes that have been made in the current state and return
     * to the previous state.
     * @param evt The event that triggered the cancelation.
     * @return The state of the Master List before the current state, or the
     * current state if cancel is not defined for the current state.
     * @throws MasterListException if there is some problem returning to the
     * previous state.
     **/
    public abstract MasterListState cancel(ActionEvent evt)
        throws MasterListException;
    
    /**
     * Make a copy of the entry defined in the current state.
     * @param evt The event that triggered the entry duplication.
     * @return The state of the MasterList after a successful clone of the 
     * entry, or the current state if clone is not defined for the state.
     * @throws MasterListException when there is a problem cloning the entry
     * in the current state.
     **/
    public abstract MasterListState clone(ActionEvent evt)
        throws MasterListException;

    /**
     * Change to a delete state from the current state.
     * @param evt The event that caused the change in state.
     * @return A state that allows for the deletion of an entry from the
     * Master List, or the current state if a delete state cannot be reached
     * from the current state.
     * @throws MasterListException if there is a problem changing to the
     * delete state.
     **/
    public abstract MasterListState delete(ActionEvent evt)
        throws MasterListException;
    
    /**
     * Change to an edit state from the current state.
     * @param evt The event that caused the change in state.
     * @return A state that allows the editing of an entry from the Master List,
     * or the current state if an edit state cannot be reached from the 
     * current state.
     * @throws MasterListException if there is a problem changing to the edit
     * state.
     **/
    public abstract MasterListState edit(ActionEvent evt)
        throws MasterListException;
    
    /**
     * Find a UIParameter child component of a specified component with the
     * provided name.
     * @param comp The component that contains the child parameter.
     * @param name The name of the parameter to be found.
     * @return The UIParameter with the provided name for the specified 
     * component.
     * @throws MasterListException if a UIParameter with the provided name
     * cannot be found as a child of the component.
     **/
    protected UIParameter findParameter(UIComponent comp, String name)
            throws MasterListException {
        for (Iterator<UIComponent> kids =
                comp.getChildren().iterator(); kids.hasNext(); ) {
            UIComponent child = kids.next();
            if (child instanceof UIParameter && 
                    ((UIParameter)child).getName().equals(name)) {
                return (UIParameter)child;
            }
        }
        throw new MasterListException("Unable to find the parameter: "+name);
    }
    
    /**
     * Create the index file for the specified project.
     * @param project The project that is to have its index file created.
     * @throws MasterListException if there is a problem generating the index
     * file.
     **/
    protected void generateIndexFile(ProjectBean project) throws 
            MasterListException {
        File file = new File(project.getSystemDirectory(),INDEX_FILE);
        
        // Generate a regular expression pattern matcher to determine the
        // correct URL locations of the files used in the index frames.
        Pattern pattern = Pattern.compile("^.+(/master_list.*/.+)$");
        Matcher matcher = pattern.matcher(project.getSystemDirectory());
        if (!matcher.matches()) {
            throw new MasterListException("Unable to generate pattern match "+
                    "needed for generating the project's index file.");
        }
        
        try {
            PrintWriter out = new PrintWriter(new FileWriter(file));
                        
            
            out.print("<!-- This file was automatically generated by the ");
            out.print("Master List Editor.  Changes to this file will be lost");
            out.println(" if it is edited manually. -->");
            out.println("");
            out.println("<HTML>");
            out.println("   <HEAD>");
            out.print("      <TITLE>");
            if (project.getDisplayName() == null || project.getDisplayName().equals("")) {
                out.print(project.getProjectId());
            } else {
                out.print(project.getDisplayName());
            }
            out.println(" Data Access</TITLE>");
	    out.println("    <META http-equiv=\"Expires\" content=\"0\">");
            out.println("    <META http-equiv=\"Pragma\" content=\"no-cache\">");
            out.println("    <META http-equiv=\"Cache-Control\" content=\"no-cache\">");
            out.println("   </HEAD>");
            out.print("   <FRAMESET frameborder=\"0\" framespacing=\"0\" ");
            out.println("border=\"0\" cols=\"220,*\">");
            out.print("      <FRAME marginheight=\"5\" marginwidth=\"0\" ");
            out.print("frameborder=\"0\" src=\"");
            out.print(matcher.group(1));
            if (!matcher.group(1).endsWith("/")) {
                out.print("/");
            }
            out.print(MENU_PREFIX);
            out.println(".html\" name=\"type\" />");
            out.print("      <FRAME marginheight=\"5\" marginwidth=\"5\" ");
            out.print("frameborder=\"0\" src=\"");
            out.print(matcher.group(1));
            if (!matcher.group(1).endsWith("/")) {
                out.print("/");
            }
            out.print(DATASET_LIST);
            out.println("\" name=\"list\" />");
            out.println("   </FRAMESET>");
            out.println("   <NOFRAMES>");
            out.println("      <p>This page requires the use of frames.");
            out.println("   </NOFRAMES>");
            out.println("</HTML>");
            
            out.close();
        } catch (IOException e) {
            // Convert the IOException to a MasterListException
            throw new MasterListException("There was a problem generating the "+
                    INDEX_FILE+" for project: "+project.getProjectId(),e);
        }
    }
    
    /**
     * Get the category that is currently selected in the state.
     * @return The currently selected category, or <code>null</code> if the
     * category is not defined in the current state.
     **/
    public abstract CategoryBean getCategory();
    
    /**
     * Get the TreeNode that is the root of the category tree displayed in a JSF
     * menu.
     * @param treeState The container for the expansion state of the nodes in
     * the category tree.
     * @return The root node of the category tree or <code>null</code> if a 
     * category tree is not displayed in the current state.
     * @throws MasterListException if there is a problem generating the root
     * node for the category tree.
     **/
    public abstract TreeNode getCategoryTreeRoot(TreeState treeState)
        throws MasterListException;
    
    /**
     * Get the data set that is currently selected in the state.
     * @return The currently selecte data set or <code>null</code> if a data
     * set is not defined in the current state.
     **/
    public abstract DatasetBean getDataset();

    /**
     * Get the TreeNode that is the root of the data set list displayed in a JSF
     * page.
     * @param treeState The container for the expansion state of the nodes in 
     * the data set list.
     * @return The root node of the data set list of <code>null</code> if a
     * data set list is not displayed in the current state.
     * @throws MasterListException if there is a problem generating the root
     * node for the data set list.
     **/
    public abstract TreeNode getDatasetListRoot(TreeState treeState)
        throws MasterListException;
    
    /**
     * Get the project that is currently selected in the state.
     * @return The currently selected project or <code>null</code> if a project
     * is not defined in this state.
     **/
    public abstract ProjectBean getProject();

    /**
     * Get the Selector used by this state to include or exclude items.
     * @return The Selector being used by the state or <code>null</code> if the
     * state does not use a Selector.
     **/
    public abstract Selector getSelector();
    
    /**
     * Hide/unhide an entry that is defined in the state.
     * @param evt The event that triggered the hiding/unhiding of an entry.
     * @return The state of the Master List after successfully hiding/unhiding
     * the entry or the current state if an entry cannot be hidden from this
     * state.
     * @throws MasterListException if there is a problem hiding/unhiding of
     * the entry in the state. 
     **/
    public abstract MasterListState hide(ActionEvent evt)
        throws MasterListException;
    
    /**
     * Determine if this state's primary display is for category information.
     * @return <code>true</code> if this state is a category state,
     * <code>false</code> otherwise.
     **/
    public abstract boolean isCategoryState();

    /**
     * Determine if this state's primary display is for data set information.
     * @return <code>true</code> if this state is a data set state,
     * <code>false</code> otherwise.
     **/
    public abstract boolean isDatasetState();
    
    /**
     * Determine if this state's primary display is for deleting an entry from
     * the database.
     * @return <code>true</code> if this state is a delete state,
     * <code>false</code> otherwise.
     **/
    public abstract boolean isDeleteState();

    /**
     * Determine if this state's primary display is for editing an entry in the
     * database.
     * @return <code>true</code> if this state is an edit state,
     * <code>false</code> otherwise.
     **/
    public abstract boolean isEditState();
    
    /**
     * Determine if this state's primary display is for displaying a list of 
     * entries.
     * @return <code>true</code> if this state is a list state,
     * <code>false</code> otherwise.
     **/
    public abstract boolean isListState();

    /**
     * Generate the public data set list page for the specified project.
     * @param project The project the data set list is to be created for.
     * @throws MasterListException if there is a problem generating the list.
     **/
    protected void makeDatasetList(ProjectBean project) throws
            MasterListException {
        
        // Get the set of resources used in the JSF pages.
        ResourceBundle properties = ResourceBundle.getBundle("resources");
        
        // Get a connection to the database from the connection pool.
        Connection connection =
                GeneralDatabaseAccessBean.getInstance().getConnection();
        
        File datasetList = new File(project.getSystemDirectory(),DATASET_LIST);
        try {
            // Create the file output stream.
            PrintWriter out = new PrintWriter(new FileWriter(datasetList));
            
            // Generate an automated file warning.
            out.write("<!-- This page was automatically generated by the ");
            out.write("Master List Editor.  DO NOT EDIT BY HAND.  All changes");
            out.write(" will be lost during the next update. -->\n");
            
            out.write("<HTML>\n");
            out.write("<HEAD>\n");
            out.write("   <META http-equiv=\"Expires\" content=\"0\">\n");
            out.write("   <META http-equiv=\"Pragma\" content=\"no-cache\">\n");
            out.write("   <META http-equiv=\"Cache-Control\" content=\"no-cache\">\n");
            out.write("   <STYLE>\n");
            out.write("<!--\n");
            out.write("body { font-family: sans-serif; }\n");
            out.write("-->\n");
            out.write("   </STYLE>\n");
            out.write("</HEAD>\n");
            out.write("<BODY text=\"black\" bgcolor=\"white\"");
            out.write("link=\"#0000EE\" alink=\"#0000EE\" vlink=\"#0000EE\">");
            out.write("\n<CENTER>\n\n\n");
            
            // Generate the page Header.
            out.write("<HR size=\"3\" noshade />\n");
            out.write("<H1>");
            if (project.getDisplayName() == null || project.getDisplayName().equals("")) {
                out.write(project.getProjectId());
            } else {
                out.write(project.getDisplayName());
            }
            out.write(" Data Sets ");
            out.write("<IMG width=\"50\" height=\"50\" align=\"absmiddle\" ");
            out.write("alt=\"");
            out.write(properties.getString("eolIconAlt"));
            out.write("\" src=\"");
            out.write(properties.getString("eolIcon"));
            out.write("\" /></H1>\n");
            out.write("<HR size=\"3\" noshade />\n\n\n");

            // Start the Table of Data Sets.
            out.write("<TABLE border=\"1\" cellpadding=\"5\" width=\"98%\">\n");
            
            // Generate the Table Header
            out.write("   <TR>\n");
            out.write("      <TH bgcolor=\"#FFFCDA\">Data Set Name ");
            out.write("(Responsible Group/PIs shown in parentheses)</TH>\n");
            out.write("      <TH bgcolor=\"#FFFCDA\">Date Posted</TH>\n");
            out.write("      <TH bgcolor=\"#FFFCDA\">Documentation</TH>\n");
            out.write("   </TR>\n");
            
            ProjectDatasetListRootBean root = 
                    new ProjectDatasetListRootBean(null,project,null);
            for (Iterator<TreeNode> kids =
                    root.getChildren().iterator(); kids.hasNext(); ) {
                outputDatasetListChild(kids.next(),out,"",properties);
            }            
            
            out.write("</TABLE>\n");
            
            out.write("</CENTER>\n");
            out.write("</BODY>\n");
            out.write("</HTML>\n");
            
            // Propertly close the output stream.
            out.close();
        } catch (IOException e) {
            // Convert the IOException to a MasterListException
            throw new MasterListException("Unable to generate dataset list for"+
                    " project: "+project.getProjectId()+".",e);
        }
        
        // Properly close the connection.
        try { connection.close(); }
        catch (SQLException e) {}
    }
    
    /**
     * Determine if this state's primary display is for project information.
     * @return <code>true</code> if this state is a project state,
     * <code>false</code> otherwise.
     **/
    public abstract boolean isProjectState();
    
    /**
     * Write the specified node information to the output file.
     * @param node The node to be written to the file.
     * @param out The output stream to be written to.
     * @param categoryParentName The name of the parent category to the current
     * node.
     * @param properties The set of properties defining the imagery for the 
     * output file.
     * @throws IOException when there is a problem writing the data to the
     * output stream.
     **/
    private void outputDatasetListChild(TreeNode node, PrintWriter out,
            String categoryParentName, ResourceBundle properties) throws 
            IOException {

        // Handle Category Displays
        if (node instanceof CategoryProjectBean) {
            CategoryProjectBean category = (CategoryProjectBean)node;
            
            // Only show the category (and all of its children) if it is not
            // hidden.
            if (!category.isHidden()) {
                // Add a spacer before the new category
                out.write("   <TR>\n");
                out.write("      <TD colspan=\"3\">&nbsp;</TD>\n");
                out.write("   </TR>\n");

                // Generate the Category Header Line
                out.write("   <TR bgcolor=\"FFFCDA\">\n");
                out.write("      <TH align=\"left\" colspan=\"3\">");
                out.write("         <A name=\"category-");
                out.write(category.getCategoryId().toString());
                out.write("\"></A>\n");
            
                if (!categoryParentName.equals("")) {
                    out.write(categoryParentName);
                    out.write(": ");
                }       
                out.write(category.getName());
                out.write("</TH>\n");
                out.write("   </TR>\n");
                
                // Create the output entries for all of the kids of the category
                for (Iterator<TreeNode> itr = 
                        category.getChildren().iterator(); itr.hasNext(); ){
                    outputDatasetListChild(itr.next(),out,
                            categoryParentName.equals("") ? category.getName() :
                                categoryParentName+":"+category.getName(),
                            properties);
                }
            }            
        } 
        // Handle the data set display
        else if (node instanceof DatasetProjectBean) {
            DatasetProjectBean dataset = (DatasetProjectBean)node;
            // Only display the data set if it is not hidden.
            if (!dataset.isHidden()) {
                out.write("   <TR>\n");
                out.write("      <TD>");
                // Only output the URL link if it is known
                if (dataset.getUrl() == null || dataset.getUrl().equals("")) {
                    out.write(dataset.getName());
                } else {
                    out.write("<A href=\"");
		    if (dataset.getUrl().startsWith("ftp")) {
			String title = dataset.getName();
			title.replaceAll("\\s+","+");
			out.write(properties.getString("ftpPageUrl"));
			out.write("?"+dataset.getName());
			out.write("&"+dataset.getUrl());
		    } else {
			out.write(dataset.getUrl());
		    }
                    out.write("\" target=\"_top\">");
                    out.write(dataset.getName());
                    out.write("</A>");
                }
                out.write("</TD>\n");
                
                // Output the correct image/date set
                if (dataset.isInProgress()) {
                    out.write("      <TD align=\"center\" ");
                    out.write("style=\"color: #00CC00; font-weight: bold;\">");
                    /*
                    out.write("<IMG alt=\"");
                    out.write(properties.getString("inProgressIconAlt"));
                    out.write("\" src=\"");
                    out.write(properties.getString("inProgressIcon"));
                    out.write("\" />");
                     */
                    out.write(properties.getString("inProgressIconAlt"));                    
                    out.write("</TD>\n");
                } else if (dataset.isUpdated()) {
                    out.write("      <TD>\n");
                    out.write("         <TABLE cellpadding=\"0\" cellspacing=\"0\" width=\"100%\">\n");
                    out.write("            <TR>\n");
                    out.write("               <TD align=\"center\" ");
                    out.write("width=\"100%\">");
                    
                    out.write("<SPAN style=\"color: #CC0000; font-weight: bold;\">");
                    out.write(properties.getString("updatedIconAlt"));
                    out.write("</SPAN>\n");
                    /*
                    out.write("<IMG alt=\"");
                    out.write(properties.getString("updatedIconAlt"));
                    out.write("\" src=\"");
                    out.write(properties.getString("updatedIcon"));
                    out.write("\" />");
                     */
                    out.write("               </TD>\n");
                    out.write("            </TR>\n");
                    out.write("            <TR>\n");
                    out.write("               <TD align=\"center\" ");
                    out.write("width=\"100%\">");
                    out.write(dataset.getDateUpdated().toString());
                    out.write("               </TD>\n");
                    out.write("            </TR>\n");
                    out.write("         </TABLE>\n");
                    out.write("      </TD>\n");
                } else if (dataset.isNew()) {
                    out.write("      <TD>\n");
                    out.write("         <TABLE cellpadding=\"0\" cellspacing=\"0\" width=\"100%\">\n");
                    out.write("            <TR>\n");
                    out.write("               <TD align=\"center\" ");
                    out.write("width=\"100%\">\n");
                    
                    out.write("<SPAN style=\"color: #CC00CC; font-weight: bold;\">");
                    out.write(properties.getString("newIconAlt"));
                    out.write("</SPAN>\n");
                    /*
                    out.write("<IMG alt=\"");
                    out.write(properties.getString("newIconAlt"));
                    out.write("\" src=\"");
                    out.write(properties.getString("newIcon"));
                    out.write("\" />");
                     */
                    out.write("               </TD>\n");
                    out.write("            </TR>\n");
                    out.write("            <TR>\n");
                    out.write("               <TD align=\"center\" ");
                    out.write("width=\"100%\">");
                    out.write(dataset.getDatePosted().toString());
                    out.write("               </TD>\n");
                    out.write("            </TR>\n");
                    out.write("         </TABLE>\n");
                    out.write("      </TD>\n");
                } else if (dataset.isExpected()) {
                    out.write("      <TD>\n");
                    out.write("         <TABLE cellpadding=\"0\" cellspacing=\"0\" width=\"100%\">\n");
                    out.write("            <TR>\n");
                    out.write("               <TD align=\"center\" ");
                    out.write("width=\"100%\">\n");
                    
                    out.write("<SPAN style=\"color: #0000CC; font-weight: bold;\">");
                    out.write(properties.getString("expectedIconAlt"));
                    out.write("</SPAN>\n");
                    /*
                    out.write("<IMG alt=\"");
                    out.write(properties.getString("expectedIconAlt"));
                    out.write("\" src=\"");
                    out.write(properties.getString("expectedIcon"));
                    out.write("\" />");
                    */
                    out.write("               </TD>\n");
                    out.write("            </TR>\n");
                    out.write("            <TR>\n");
                    out.write("               <TD align=\"center\" ");
                    out.write("width=\"100%\">");
                    out.write(dataset.getDateExpected().toString());
                    out.write("               </TD>\n");
                    out.write("            </TR>\n");
                    out.write("         </TABLE>\n");
                    out.write("      </TD>\n");
                } else if (dataset.getDatePosted() != null) {
                    out.write("      <TD align=\"center\">");
                    out.write(dataset.getDatePosted().toString());
                    out.write("</TD>\n");
                } else {
                    out.write("      <TD>&nbsp;</TD>\n");
                }
                
                // Only output the document link if the data set has one
                if (dataset.getDocUrl() != null && 
                        !dataset.getDocUrl().equals("")) {
                    out.write("      <TD align=\"center\"><A href=\"");
                    out.write(dataset.getDocUrl());
                    out.write("\"><IMG alt=\"");
                    out.write(properties.getString("docIconAlt"));
                    out.write("\" border=\"0\" src=\"");
                    out.write(properties.getString("docIcon"));
                    out.write("\" /></A></TD>\n");
                } else {
                    out.write("      <TD>&nbsp;</TD>\n");                    
                }
                
                out.write("   </TR>\n");
            }
        } else {
            // Handle the case where an unknown TreeNode type comes into the 
            // tree.
            throw new IOException("Unknown node type of "+
                    node.getClass().getName());
        }
    }
    
    /**
     * Recreate all of the data set lists for all of the projects associated
     * with the specified data set.
     * @param dataset The data set that is causing the files to be regenerated.
     * @throws MasterListException when there is a problem regenenerating the
     * project data set lists.
     **/
    protected void regenerateFiles(DatasetBean dataset) throws
            MasterListException {
        // Loop through the projects associated with the data set.
        for (Iterator<ProjectBean> itr = 
                dataset.getProjects().iterator(); itr.hasNext(); ) {
            makeDatasetList(itr.next());
        }
    }
    
    /**
     * Recreate the category menus for the specified project.
     * @param project The project the menus are to be regenerated for.
     * @throws MasterListExeption when there is a problem generating the 
     * category menus for the project.
     **/
    protected void regenerateMenus(ProjectBean project) throws
            MasterListException {        
        TreeNode root = new ProjectCategoryTreeRootBean(null,project);
        
        // Create the list that will be used to determine which menus need
        // to be generated.
        List<TreeNode> menus = new ArrayList<TreeNode>();
        menus.add(root);

        // Define the variables that will be used by all of the menus.
        List<TreeNode> kids = root.getChildren();
        ResourceBundle properties = ResourceBundle.getBundle("resources");
        
        // Continue until all of the menus have been generated.
        while (!menus.isEmpty()) {
            // Remove the current working node so it won't be created again.
            TreeNode current = menus.remove(0);
            
            // Define the menu file name.
            File file = new File(project.getSystemDirectory(),
                    current.equals(root) ? MENU_PREFIX+".html" : 
                        MENU_PREFIX+"-"+current.getId()+".html");
            
            try {
                PrintWriter out = new PrintWriter(new FileWriter(file));
                
                out.println("<HTML>");
                out.write("<HEAD>\n");
		out.write("   <META http-equiv=\"Expires\" content=\"0\">\n");
                out.write("   <META http-equiv=\"Pragma\" content=\"no-cache\">\n");
                out.write("   <META http-equiv=\"Cache-Control\" content=\"no-cache\">\n");
                out.write("   <STYLE>\n");
                out.write("<!--\n");
                out.write("body { font-family: sans-serif; }\n");
                out.write("-->\n");
                out.write("   </STYLE>\n");
                out.write("</HEAD>\n");
                out.print("<BODY text=\"black\" bgcolor=\"FFFCDA\" ");
                out.print("link=\"#0000EE\" alink=\"#0000EE\" ");
                out.print("vlink=\"#0000EE\" style=\"padding: 5px;\">");

                // Generate the menu header
                out.println("<BR>");
                out.print("<CENTER><IMG alt=\"");
                out.print(project.getProjectId());
                out.print(" Logo\" src=\"");
                out.print(project.getLogoUrl());
                out.println("\" /></CENTER>");
                out.println("<BR>");
                out.println("<BR>\n\n");
                
                
                out.println("<TABLE cellspacing=\"2\" cellpadding=\"2\">");
                out.println("   <TR>");
                out.println("      <TH><B><U>DATA CATEGORIES</U></B></TH>");
                out.println("   </TR>");
                
                // Loop through all of the top level categories for the project.
                for (Iterator<TreeNode> itr = kids.iterator(); itr.hasNext();) {
                    CategoryProjectBean category = 
                            (CategoryProjectBean)itr.next();

                    if (!category.isHidden()) {
                        out.println("   <TR>");
                        out.print("      <TD><B><A href=\"");
                        if (category.isLeaf()) {
                            out.print(DATASET_LIST);
                            out.print("#category-");
                            out.print(category.getId());
                        } else if (category.equals(current)) {
                            out.print(MENU_PREFIX);
                            out.print(".html");
                        } else {
                            out.print(MENU_PREFIX);
                            out.print("-");
                            out.print(category.getId());
                            out.print(".html");
                        }
                        out.print("\" target=\"");
                        out.print(category.isLeaf() ? "list" : "_self");
                        out.print("\">");
                        out.print(category.getName());
                        out.println("</A></B></TD>");                    
                        out.println("   </TR>");

                        // Handle all of the subcategories of the current one.
                        if (current instanceof CategoryBean && 
                                category.equals(current)) {
                            for (Iterator<TreeNode> iter = 
                                    category.getChildren().iterator(); 
                                    iter.hasNext(); ) {
                                CategoryProjectBean cat =
                                        (CategoryProjectBean)iter.next();
                                if (!cat.isHidden()) {
                                    out.println("   <TR>");
                                    out.print("      <TD><LI><B><A href=\"");
                                    if (cat.isLeaf()) {
                                        out.print(DATASET_LIST);
                                        out.print("#category-");
                                        out.print(cat.getId());
                                    } else {
                                        out.print(MENU_PREFIX);
                                        out.print("-");
                                        out.print(cat.getId());
                                        out.print(".html");
                                    }
                                    out.print("\" target=\"");
                                    out.print(cat.isLeaf() ? "list" : "_self");
                                    out.print("\">");
                                    out.print(cat.getName());
                                    out.println("</A></B></LI></TD>");
                                    out.println("   </TR>");
                                }
                            }
                        }

                        // Add the current category if it needs to have its own
                        // menu created.
                        if (category.getParent().equals(current) && 
                                !category.isLeaf()) {
                            menus.add(category);
                        }
                    }
                }
                
                out.println("</TABLE>");
                
                // Add the footer to the end of the menu.
                out.println("<HR>");
                out.print("   <B><A href=\"");
                out.print(project.getHomePageUrl());
                out.print("\" target=\"_top\">Back to ");
                if (project.getDisplayName() == null || 
                        project.getDisplayName().equals("")) {
                    out.print(project.getProjectId());
                } else {
                    out.print(project.getDisplayName());
                }
                out.println("</A></B>");                
                out.println("<HR>");
                
                out.print("<B>Email comments & questions to <ADDRESS>");
                out.print("<A href=\"mailto:webmaster@eol.ucar.edu\">");
                out.println("webmaster@eol.ucar.edu</A></ADDRESS></B>");
                
                out.println("</BODY>");
                out.println("</HTML>");
                
                out.close();
            } catch (IOException e) {
                throw new MasterListException("There was a problem generating "+
                        "the file: "+file.getName()+" for project: "+
                        project.getProjectId(),e);
            }
        }
    }
        
    /**
     * Change the current state to a state that displays a list of categories.
     * @param evt The event that triggered the state change.
     * @return A state that lists categories or this state if a category
     * list cannot be reached from this state.
     * @throws MasterListException if there is a problem changing to a category
     * list state.
     **/
    public abstract MasterListState viewCategoryList(ActionEvent evt)
        throws MasterListException;
    
    /**
     * Change the current state to a state that displays a list of data sets.
     * @param evt The event that triggered the state change.
     * @return A state that lists data sets or the current state if a data set
     * list cannot be reached from this state.
     * @throws MasterListException if there is a problem changing to a data set
     * list state.
     **/
    public abstract MasterListState viewDatasetList(ActionEvent evt)
        throws MasterListException;

    /**
     * Change the current state to a state that displays a list of data sets for
     * the specified project.
     * @param projectId The if of the project the data set list is to display.
     * @return A state that lists data sets for the project or the current state
     * if a data set list cannot be reached from this state.
     * @throws MasterListException if there is a problem changing to the data
     * set list for the project.
     **/
    public abstract MasterListState viewDatasetList(String projectId)
        throws MasterListException;
    
    /**
     * Change the current state to a state that displays a list of projects. 
     * @param evt The event that triggered the state change.
     * @return A state that lists projects or the current state if a project 
     * list cannot be reached from this state. 
     * @throws MasterListException if there is a problem changing to a project
     * list state.
     **/
    public abstract MasterListState viewProjectList(ActionEvent evt)
        throws MasterListException;
}
