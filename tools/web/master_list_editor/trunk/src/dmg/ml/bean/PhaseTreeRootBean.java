package dmg.ml.bean;

import dmg.jsf.TreeException;
import dmg.jsf.model.TreeNode;
import dmg.ml.DatabaseAccessBean;
import dmg.ml.MasterListException;
import dmg.ml.manager.state.TreeState;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

/**
 * The PhaseTreeRootBean is a special bean that is the root for a tree
 * that displays datasets by phase.
 *
 * @author Joel Clawson
 */
public class PhaseTreeRootBean extends MasterListBean<PhaseTreeRootBean> {

    private ProjectBean project;
    
    /**
     * Create a new instance of a PhaseTreeRootBean.
     * @param state The container that holds the node expansion states for the tree.
     **/
    public PhaseTreeRootBean(TreeState state) {
        super(state);
        if (state != null && !state.isExpansionSet(this)) {
            setExpanded(true);
        }
    }
    
    /**
     * Create a new instance of a PhaseTreeRootBean.
     * @param state The container that holds the node expansion states for the tree.
     * @param project The project that the tree is being generated for.
     **/
    public PhaseTreeRootBean(TreeState state, ProjectBean project) {
        this(state);
        this.project = project;
    }
    
    /**
     * Get the list of phases that are children to this node.
     * @return The child phases of this root node.
     * @throws TreeException when there is a problem loading the phase information.
     **/
    public List<TreeNode> getChildren() throws TreeException {
        List<TreeNode> projects = new ArrayList<TreeNode>();
        try {
            Connection conn = DatabaseAccessBean.getConnection();
            if (project == null) {
                String sql = "SELECT DISTINCT(phase.project_id),system_directory,url,home_page_url,logo_url,css_url FROM phase JOIN project ON phase.project_id=project.project_id ORDER BY phase.project_id";
                PreparedStatement stmt = conn.prepareStatement(sql);
                ResultSet results = stmt.executeQuery();
                while (results.next()) {
                    ProjectPhaseBean project = new ProjectPhaseBean(treeState);
                    project.setProjectId(results.getString(1));
                    project.setOriginalId(results.getString(1));
                    project.setSystemDirectory(results.getString(2));
                    project.setUrl(results.getString(3));
                    project.setHomePageUrl(results.getString(4));
                    project.setLogoUrl(results.getString(5));
                    project.setCssUrl(results.getString(6));
                    project.setParent(this);
                    projects.add(project);
                }
                results.close();
                stmt.close();
            } else {
                String sql = "SELECT phase_id,project_id,name,hide_flag FROM phase WHERE project_id=?";
                PreparedStatement stmt = conn.prepareStatement(sql);
                stmt.setString(1,project.getProjectId());
                ResultSet results = stmt.executeQuery();
                while (results.next()) {
                    PhaseBean phase = new PhaseBean(treeState,project);
                    phase.setPhaseId(results.getInt(1));
                    phase.setName(results.getString(3));
                    phase.setHidden(results.getBoolean(4));
                    phase.setParent(this);
                    projects.add(phase);
                }
                results.close();
                stmt.close();
            }
        } catch (SQLException e) {
            System.err.println(e.getMessage());
            throw new TreeException(e.getMessage());
        } catch (MasterListException e) {
            System.err.println(e.getMessage());
            throw new TreeException(e.getMessage());
        }
        
        return projects;
    }

    public void delete(Connection conn) throws MasterListException {}

    protected String getClosedFacetName() { return "closedPhaseRoot"; }

    protected String getLeafFacetName() { return ""; }

    protected String getOpenFacetName() { return "openPhaseRoot"; }

    public void insert(Connection conn) throws MasterListException {}

    public void update(Connection conn) throws MasterListException {}

    public int compareTo(PhaseTreeRootBean o) { return 0; }

    public Integer getId() { return 0; }
}
