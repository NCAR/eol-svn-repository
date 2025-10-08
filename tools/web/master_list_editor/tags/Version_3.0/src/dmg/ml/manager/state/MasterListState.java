package dmg.ml.manager.state;

import dmg.jsf.model.TreeNode;
import dmg.ml.DatabaseAccessBean;
import dmg.ml.MasterListException;
import dmg.ml.bean.ClassificationBean;
import dmg.ml.bean.ClassificationProjectBean;
import dmg.ml.bean.ClassificationTypeBean;
import dmg.ml.bean.DatasetBean;
import dmg.ml.bean.DatasetProjectBean;
import dmg.ml.bean.PhaseBean;
import dmg.ml.bean.PhaseDatasetListBean;
import dmg.ml.bean.PhaseTreeRootBean;
import dmg.ml.bean.PhaseTypeBean;
import dmg.ml.bean.ProjectBean;
import dmg.ml.bean.ProjectClassificationTreeRootBean;
import dmg.ml.bean.ProjectDatasetListRootBean;
import dmg.ml.manager.MasterListManager;
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
    private static final String DATASET_LIST_PREFIX = "dataset_list";
    
    // The name of the file that is the main index for a project's Master List.
    private static final String INDEX_FILE = "index.html";
    
    // The prefix of the name of the file for the category menus for the
    // public view.
    private static final String MENU_PREFIX = "menu";

    /**
     * The manager that is using this state.
     **/
    protected MasterListManager manager;
    
    /**
     * Create a new instance of a MasterListState.
     * @param manager The manager that is using this state.
     **/
    public MasterListState(MasterListManager manager) { this.manager = manager; }
    
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
            out.print(project.getProjectId());
            out.println(" Data Access</TITLE>");
	    out.println("    <META http-equiv=\"Expires\" content=\"0\">");
            out.println("    <META http-equiv=\"Pragma\" content=\"no-cache\">");
            out.println("    <META http-equiv=\"Cache-Control\" content=\"no-cache\">");
            out.println("   </HEAD>");
            out.print("   <FRAMESET frameborder=\"0\" framespacing=\"0\" ");
            out.println("border=\"0\" cols=\"250,*\">");
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
            out.print(DATASET_LIST_PREFIX);
	    /*
            out.print("-");
            out.print(ClassificationTypeBean.getClassificationTypes(project).get(0).getName());
	    */
            out.println(".html\" name=\"list\" />");
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
     * Get the classification that is currently selected in the state.
     * @return The currently selected classification, or <code>null</code> if the
     * classification is not defined in the current state.
     **/
    public abstract ClassificationBean getClassification();
    
    /**
     * Get the classification type that is currently selected in the state.
     * @return The currently selected classification type, or <code>null</code> if the
     * classification type is not defined in the current state.
     **/
    public abstract ClassificationTypeBean getClassificationType();
    
    /**
     * Get the TreeNode that is the root of the classification tree displayed in a JSF
     * menu.
     * @param treeState The container for the expansion state of the nodes in
     * the classification tree.
     * @return The root node of the classification tree or <code>null</code> if a 
     * classification tree is not displayed in the current state.
     * @throws MasterListException if there is a problem generating the root
     * node for the classification tree.
     **/
    public abstract TreeNode getClassificationTreeRoot(TreeState treeState)
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
     * Get the phase that is currently selected in the state.
     * @return The currently selected phase or <code>null</code> if a phase
     * is not defined in this state.
     **/
    public abstract PhaseBean getPhase();
    
    /**
     * Get the TreeNode that is the root of the phase list displayed in a JSF
     * page.
     * @param treeState The container for the expansion state of the nodes in 
     * the phase list.
     * @return The root node of the phase list of <code>null</code> if a
     * phase list is not displayed in the current state.
     * @throws MasterListException if there is a problem generating the root
     * node for the phase list.
     **/
    public abstract TreeNode getPhaseTreeRoot(TreeState treeState) throws MasterListException;
    
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
     * Determine if this state's primary display is for classification information.
     * @return <code>true</code> if this state is a classification state,
     * <code>false</code> otherwise.
     **/
    public abstract boolean isClassificationState();

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
    protected void makeDatasetList(ProjectBean project) throws MasterListException {
        
        // Get the set of resources used in the JSF pages.
        ResourceBundle properties = ResourceBundle.getBundle("resources");
        
        // Get a connection to the database from the connection pool.
        Connection connection = DatabaseAccessBean.getConnection();

	File datasetList = new File(project.getSystemDirectory(),DATASET_LIST_PREFIX+".html");
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
	    if (project.getCssUrl() != null && !project.getCssUrl().equals("")) {
		out.write("   <LINK rel=\"stylesheet\" type=\"text/css\" href=\"");
		out.write(project.getCssUrl());
		out.write("\" />\n");
	    } else {
		out.write("   <STYLE>\n");
		out.write("<!--\n");
		out.write("body { font-family: sans-serif; }\n");
		out.write("-->\n");
		out.write("   </STYLE>\n");
	    }
	    out.write("</HEAD>\n");
	    out.write("<BODY text=\"black\" bgcolor=\"white\"");
	    out.write("link=\"#0000EE\" alink=\"#0000EE\" vlink=\"#0000EE\">");
	    out.write("\n<CENTER>\n\n\n");
	    
	    // Generate the page Header.
	    out.write("<HR size=\"3\" noshade />\n");
	    out.write("<H1>");
	    out.write(project.getProjectId());
	    out.write(" Data Sets ");
	    out.write("<IMG width=\"50\" height=\"50\" align=\"absmiddle\" ");
	    out.write("alt=\"");
	    out.write(properties.getString("eolIconAlt"));
	    out.write("\" src=\"");
	    out.write(properties.getString("eolIcon"));
	    out.write("\" /></H1>\n");
	    out.write("<HR size=\"3\" noshade />\n\n\n");
	    
	    // Start the Table of Data Sets.
	    out.write("<TABLE border=\"1\" cellpadding=\"5\" width=\"99%\">\n");
	    
	    // Generate the Table Header
	    out.write("   <TR>\n");
	    out.write("      <TH bgcolor=\"#FFFCDA\">Data Set Name ");
	    out.write("(Responsible Group/PIs shown in parentheses)</TH>\n");
	    out.write("      <TH bgcolor=\"#FFFCDA\">Date Posted</TH>\n");
	    out.write("      <TH bgcolor=\"#FFFCDA\">Info</TH>\n");
	    out.write("   </TR>\n");
	    
	    List<ClassificationTypeBean> types = ClassificationTypeBean.getClassificationTypes(project);
	    
	    for (ClassificationTypeBean type: types) {
		
		TreeNode root = new ProjectDatasetListRootBean(null,project,type,null);
		for (Iterator<TreeNode> itr = root.getChildren().iterator(); itr.hasNext(); ) {
		    TreeNode child = itr.next();
		    
                    for (Iterator<TreeNode> kids = child.getChildren().iterator(); kids.hasNext(); ) {
                        outputDatasetListChild(kids.next(),out,"",properties);
                    }            
		}
	    }

	    if (!PhaseBean.getPhaseList(project).isEmpty()) {
		for (TreeNode child: (new ProjectDatasetListRootBean(null,project,new PhaseBean(null,project))).getChildren()) {
		    outputDatasetListChild(child,out,"",properties);
		}	    
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
        
    }
    
    /**
     * Determine if this state's primary display is for phase information.
     * @return <code>true</code> if this state is a phase state,
     * <code>false</code> otherwise.
     **/
    public abstract boolean isPhaseState();
    
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
        if (node instanceof ClassificationProjectBean) {
            ClassificationProjectBean classification = (ClassificationProjectBean)node;
            
            // Only show the category (and all of its children) if it is not
            // hidden.
            if (!classification.isHidden()) {
                // Add a spacer before the new category
                out.write("   <TR>\n");
                out.write("      <TD colspan=\"3\">&nbsp;</TD>\n");
                out.write("   </TR>\n");

                // Generate the Category Header Line
                out.write("   <TR bgcolor=\"FFFCDA\">\n");
                out.write("      <TH align=\"left\" colspan=\"3\" style=\"font-size: 13pt;\">");
                out.write("         <A name=\"category-");
                out.write(classification.getClassificationId().toString());
                out.write("\"></A>\n");
            
                if (!categoryParentName.equals("")) {
                    out.write(categoryParentName);
                    out.write(": ");
                }       
                out.write(classification.getName());
                out.write("</TH>\n");
                out.write("   </TR>\n");
                
                // Create the output entries for all of the kids of the category
                for (Iterator<TreeNode> itr = 
                        classification.getChildren().iterator(); itr.hasNext(); ){
                    outputDatasetListChild(itr.next(),out,
                            categoryParentName.equals("") ? classification.getName() :
                                categoryParentName+":"+classification.getName(),
                            properties);
                }
            }            
        }
        // Handle the phases
        else if (node instanceof PhaseBean) {
            PhaseBean phase = (PhaseBean)node;
            
            // Only show the category (and all of its children) if it is not
            // hidden.
            if (!phase.isHidden()) {
                // Add a spacer before the new category
                out.write("   <TR>\n");
                out.write("      <TD colspan=\"3\" style=\"font-size: 14pt;\">&nbsp;</TD>\n");
                out.write("   </TR>\n");

                // Generate the Category Header Line
                out.write("   <TR bgcolor=\"FFFCDA\">\n");
                out.write("      <TH align=\"left\" colspan=\"3\">");
                out.write("         <A name=\"phase-");
                out.write(phase.getPhaseId().toString());
                out.write("\"></A>\n");
            
                if (!categoryParentName.equals("")) {
                    out.write(categoryParentName);
                    out.write(": ");
                }       
                out.write(phase.getName());
                out.write("</TH>\n");
                out.write("   </TR>\n");
                
                // Create the output entries for all of the kids of the category
                for (Iterator<TreeNode> itr = phase.getChildren().iterator(); itr.hasNext(); ){
                    outputDatasetListChild(itr.next(),out,
                            categoryParentName.equals("") ? phase.getName() :
                                categoryParentName+":"+phase.getName(),
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
                out.write("      <TD style=\"font-size: 12pt;\">");
                // Only output the URL link if it is known
                if (dataset.getUrl() == null || dataset.getUrl().equals("")) {
                    out.write(dataset.getName());
                    if (dataset.getAuthorPi() != null && !dataset.getAuthorPi().equals("")) {
                        out.write(" [");
                        out.write(dataset.getAuthorPi());
                        out.write("]");
                    }                } else {
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
                    if (dataset.getAuthorPi() != null && !dataset.getAuthorPi().equals("")) {
                        out.write(" [");
                        out.write(dataset.getAuthorPi());
                        out.write("]");
                    }
                    out.write("</A>");
                }
                out.write("</TD>\n");

                boolean displayedInfo = false;
                out.write("      <TD align=\"center\">\n");
                if (dataset.isPreliminary()) {
                    out.write("      <SPAN style=\"color: #990000; font-style: italic; font-weight: bold;\">");
                    out.write(properties.getString("preliminaryIconAlt"));
                    out.write("</SPAN>\n");
                    out.write("      <BR />\n");
                }
                
                // Output the correct image/date set
                if (dataset.isInProgress()) {
                    out.write("      <SPAN ");
                    out.write("style=\"color: #00CC00; font-weight: bold;\">");
                    out.write(properties.getString("inProgressIconAlt"));                    
                    out.write("</SPAN>\n");
                    displayedInfo = true;
                } else if (dataset.isUpdated()) {
                    out.write("<SPAN style=\"color: #CC0000; font-weight: bold;\">");
                    out.write(properties.getString("updatedIconAlt"));
                    out.write("</SPAN>\n");
                    out.write("<BR />\n");
                    out.write("<SPAN style=\"color: #CC0000; font-weight: bold;\">");
                    out.write(dataset.getDateUpdated().toString());
                    out.write("</SPAN>\n");
                    out.write("<BR />\n");
                    displayedInfo = true;
                } else if (dataset.isNew()) {
                    out.write("<SPAN style=\"color: #CC00CC; font-weight: bold;\">");
                    out.write(properties.getString("newIconAlt"));
                    out.write("<BR />\n");
                    out.write(dataset.getDatePosted().toString());
                    out.write("</SPAN>\n");
                    out.write("<BR />\n");
                    displayedInfo = true;
                } else if (dataset.getDatePosted() != null) {
                    out.write(dataset.getDatePosted().toString());
                    displayedInfo = true;
                }

                if (dataset.getDateExpected() != null && !dataset.getDateExpected().equals("") && !dataset.getDateExpected().equals("0000-00-00")) {
                    if (displayedInfo && dataset.isPreliminary()) {
                        out.write("<HR />");
                    }
                    if (!displayedInfo || (displayedInfo && dataset.isPreliminary())) {
                        out.write("<SPAN style=\"color: #0000CC; font-weight: bold;\">");
                        out.write(properties.getString(displayedInfo ? "updateExpectedIconAlt" : "expectedIconAlt"));
                        out.write("<BR />\n");
                        out.write(dataset.getDateExpected());
                        out.write("</SPAN>\n");
                        out.write("<BR />\n");
                        displayedInfo = true;
                    }
                }
                
                // Write out an empty space to fill in the cell when there is no date.
                if (!displayedInfo) { out.write("&nbsp;"); }
                
                out.write("      </TD>\n");
                                
                
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

    private void processMenuClassification(PrintWriter out, ClassificationProjectBean category, TreeNode current, int depth, ClassificationBean parent) throws MasterListException {
	if (!category.isHidden()) {
	    out.println("   <TR>");
	    out.print("      <TD style=\"padding-left: "+(15 * depth)+"px; font-size: 11pt;\"><LI><B><A HREF=\"");
	    if (category.isLeaf()) {
		out.print(DATASET_LIST_PREFIX);
/*
		out.print("-");
		out.print(category.getType());
*/
		out.print(".html#category-");
		out.print(category.getClassificationId());
	    } else if (category.equals(current)) {
		out.print(MENU_PREFIX);
		if (parent != null) {
		    out.print("-");
		    out.print(parent.getClassificationId());
		}
		out.print(".html");
	    } else {
		out.print(DATASET_LIST_PREFIX);
/*
		out.print("-");
		out.print(category.getType());
*/
		out.print(".html#category-");
		out.print(category.getClassificationId());

		out.print("\" onclick=\"javascript: location.href='");
		out.print(MENU_PREFIX);
		out.print("-");
		out.print(category.getClassificationId());
		out.print(".html';");
	    }
	    out.print("\" target=\"");
	    out.print(category.isLeaf() || !category.equals(current) ? "list" : "_self");
	    out.print("\">");
	    out.print(category.getName());
	    out.println("</A></B></LI></TD>");
	    out.println("   </TR>");

	    for (TreeNode child: category.getChildren()) {
		if (category.equals(current) || (current instanceof ClassificationBean && category.ancestorOf((ClassificationBean)current))) {
		    processMenuClassification(out,(ClassificationProjectBean)child,current,depth+1,category);
		}
	    }
	}
	
    }
    
    /**
     * Recreate the category menus for the specified project.
     * @param project The project the menus are to be regenerated for.
     * @throws MasterListExeption when there is a problem generating the 
     * category menus for the project.
     **/
    protected void regenerateMenus(ProjectBean project) throws MasterListException {        
        TreeNode root = new ProjectClassificationTreeRootBean(null,project);
        
        // Create the list that will be used to determine which menus need
        // to be generated.
        List<TreeNode> menus = new ArrayList<TreeNode>();
        menus.add(root);

        // Define the variables that will be used by all of the menus.
        List<TreeNode> types = root.getChildren();
        ResourceBundle properties = ResourceBundle.getBundle("resources");
        
        if (!PhaseBean.getPhaseList(project).isEmpty()) {
            types.add(new PhaseTypeBean(null,project));
        }

	for (ClassificationProjectBean classification: ClassificationProjectBean.getClassificationList(project)) {
	    if (!classification.getChildren().isEmpty()) {
		menus.add(classification);
	    }
	}
        
        // Continue until all of the menus have been generated.
        while (!menus.isEmpty()) {
            // Remove the current working node so it won't be created again.
            TreeNode current = menus.remove(0);
            
            // Define the menu file name.
            File file = new File(project.getSystemDirectory(),
                    current.equals(root) ? MENU_PREFIX+".html" : MENU_PREFIX+"-"+current.getId()+".html");
            
            try {
                PrintWriter out = new PrintWriter(new FileWriter(file));
                
                out.println("<HTML>");
                out.write("<HEAD>\n");
		out.write("   <META http-equiv=\"Expires\" content=\"0\">\n");
                out.write("   <META http-equiv=\"Pragma\" content=\"no-cache\">\n");
                out.write("   <META http-equiv=\"Cache-Control\" content=\"no-cache\">\n");
                if (project.getCssUrl() != null && !project.getCssUrl().equals("")) {
                    out.write("    <LINK rel=\"stylesheet\" type=\"text/css\" href=\"");
                    out.write(project.getCssUrl());
                    out.write("\" />\n");
                }
                out.write("   <STYLE>\n");
                out.write("<!--\n");
                out.write("a:link,a:visited { text-decoration: none; }\n");
                out.write("a:hover { text-decoration: underline; }\n");
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
                

                for (Iterator<TreeNode> typeItr = types.iterator(); typeItr.hasNext(); ) {
                    ClassificationTypeBean type = (ClassificationTypeBean)typeItr.next();
                    List<TreeNode> kids = type.getChildren();
/*
                    out.println("   <TR>");
                    out.print("      <TH ALIGN=\"left\">&nbsp;</TH>");
                    out.println("   </TR>");
*/
                    
                    out.println("   <TR>");
                    out.print("      <TH ALIGN=\"left\"><B><U>DATA BY ");
                    out.print(type.getName().toUpperCase());
                    out.println("</U></B></TH>");
                    out.println("   </TR>");

		    for (TreeNode child: type.getChildren()) {
			if (child instanceof ClassificationProjectBean) {
			    processMenuClassification(out,(ClassificationProjectBean)child,current,1,null);
                        } else if (child instanceof PhaseBean) {
                            PhaseBean phase = (PhaseBean)child;
			    
                            if (!phase.isHidden()) {
                                out.println("   <TR>");
                                out.print("      <TD style=\"padding-left: 15px\"><LI><B><A href=\"");
                                if (phase.isLeaf()) {
                                    out.print(DATASET_LIST_PREFIX);
/*
                                    out.print("-");
                                    out.print(phase.getType());
*/
                                    out.print(".html#phase-");
                                    out.print(phase.getId());
                                } else if (phase.equals(current)) {
                                    out.print(MENU_PREFIX);
                                    out.print(".html");
                                } else {
                                    out.print(MENU_PREFIX);
                                    out.print("-");
                                    out.print(phase.getId());
                                    out.print(".html");
                                }
                                out.print("\" target=\"");
                                out.print(phase.isLeaf() ? "list" : "_self");
                                out.print("\">");
                                out.print(phase.getName());
                                out.println("</A></B></LI></TD>");                    
                                out.println("   </TR>");
                            }
                        }
                    }

                    out.println("   <TR>");
                    out.print("      <TH ALIGN=\"left\">&nbsp;</TH>");
                    out.println("   </TR>");


                }
                
                out.println("</TABLE>");
                
                // Add the footer to the end of the menu.
                out.println("<HR>");
                out.print("   <B><A href=\"");
                out.print(project.getHomePageUrl());
                out.print("\" style=\"color: #006600;\" target=\"_top\">Back to ");
                out.print(project.getProjectId());
                out.println("</A></B>");                
                out.println("<HR>");
                
                out.print("<B style=\"font-size: 11pt;\">Email comments & questions to <ADDRESS>");
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
     * Change the current state to a state that displays a list of classifications.
     * @param evt The event that triggered the state change.
     * @return A state that lists classifications or this state if a classification
     * list cannot be reached from this state.
     * @throws MasterListException if there is a problem changing to a classification
     * list state.
     **/
    public abstract MasterListState viewClassificationList(ActionEvent evt)
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
     * Change the current state to a state that displays a list of phases. 
     * @param evt The event that triggered the state change.
     * @return A state that lists phases or the current state if a phase
     * list cannot be reached from this state. 
     * @throws MasterListException if there is a problem changing to a phase
     * list state.
     **/
    public abstract MasterListState viewPhaseList(ActionEvent evt) throws MasterListException;
    
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
