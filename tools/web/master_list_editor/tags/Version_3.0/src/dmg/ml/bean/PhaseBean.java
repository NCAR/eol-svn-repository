package dmg.ml.bean;

import dmg.jsf.TreeException;
import dmg.jsf.model.TreeNode;
import dmg.ml.DatabaseAccessBean;
import dmg.ml.MasterListException;
import dmg.ml.manager.Selectable;
import dmg.ml.manager.state.TreeState;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

/**
 * The PhaseBean class is the representation of a phase in the Master List.
 *
 * @author Joel Clawson
 */
public class PhaseBean extends MasterListBean<PhaseBean> implements Selectable {
    
    private Integer phaseId;
    private String name,projectId;
    private Boolean associated;
    private Boolean hidden;
    
    /**
     * Create a new instance of a PhaseBean.
     **/
    public PhaseBean() { super(null); }
    
    /**
     * Create a new instance of a PhaseBean.
     * @param project The project the phase is a part of.
     **/
    public PhaseBean(ProjectBean project) { this(null,project); }
    
    /**
     * Create a new instance of a PhaseBean.
     * @param projectId The id of the project the phase is a part of.
     **/
    public PhaseBean(String projectId) { this(null,projectId); }
    
    /**
     * Create a new instance of a PhaseBean.
     * @param state The container for the tree expansion states that the phase is in.
     * @param project The project the phase is a part of.
     **/
    public PhaseBean(TreeState state, ProjectBean project) {
        this(state,project.getProjectId());
    }
    
    /**
     * Create a new instance of a PhaseBean.
     * @param state The container for the tree expansion states that the phase is in.
     * @param project The id of the project the phase is a part of.
     **/
    public PhaseBean(TreeState state, String project) {
        super(state);
        this.projectId = projectId;
    }

    /**
     * Delete the phase from the database.
     * @param conn The connection to use to delete the phase.
     * @throws MasterListException when there is a problem deleting the phase.
     **/
    public void delete(Connection conn) throws MasterListException {
        try {
            String sql = "DELETE FROM phase WHERE phase_id=?";
            PreparedStatement stmt = conn.prepareStatement(sql);
            stmt.setInt(1,getPhaseId());
            stmt.executeUpdate();
            stmt.close();
        } catch (SQLException e) {
            throw new MasterListException("Unable to delete phase: "+getPhaseId(),e);
        }
    }

    /**
     * Get the name of the facet used by JSF for displaying the phase when it is closed in a tree.
     * @return The phase's closed facet name.
     **/
    protected String getClosedFacetName() { return "closedPhase"; }

    /**
     * Get the name of the facet used by JSF for displaying the phase when it is a leaf in a tree.
     * @return The phase's leaf facet name.
     **/
    protected String getLeafFacetName() { return "leafPhase"; }

    /**
     * Get the name of the facet used by JSF for displaying the phase when it is open in a tree.
     * @return The phase's open facet name.
     **/
    protected String getOpenFacetName() { return "openPhase"; }

    /**
     * Insert the phase into the database.
     * @param conn The connection to use to insert the phase.
     * @throws MasterListException when there is a problem inserting the phase.
     **/
    public void insert(Connection conn) throws MasterListException {
        try {
            String sql = "INSERT INTO phase(project_id,name,hide_flag) VALUES(?,?,?)";
            PreparedStatement stmt = conn.prepareStatement(sql,PreparedStatement.RETURN_GENERATED_KEYS);
            stmt.setString(1,getProjectId());
            stmt.setString(2,getName());
            stmt.setBoolean(3,isHidden());
            stmt.executeUpdate();

            ResultSet keys = stmt.getGeneratedKeys();
            keys.next();
            setPhaseId(keys.getInt(1));
            
            stmt.close();
        } catch (SQLException e) {
            throw new MasterListException("Unable to insert a new phase.",e);
        }
    }

    /**
     * Update the phase in the database.
     * @param conn The connection to use to update the phase.
     * @throws MasterListException when there is a problem updating the phase.
     **/
    public void update(Connection conn) throws MasterListException {
        try {
            String sql = "UPDATE phase SET project_id=?,name=?,hide_flag=? WHERE phase_id=?";
            PreparedStatement stmt = conn.prepareStatement(sql);
            stmt.setString(1,getProjectId());
            stmt.setString(2,getName());
            stmt.setBoolean(3,isHidden());
            stmt.setInt(4,getPhaseId());
            stmt.executeUpdate();
            stmt.close();
        } catch (SQLException e) {
            throw new MasterListException("Unable to update phase: "+getPhaseId(),e);
        }
    }

    /**
     * Compare this phase to the specified phase.
     * @param phase The phase to be compared with this phase.
     * @return A negative integer, zero, or a positive integer if this phase is
     * less than, equal to, or greater than the specified phase.
     **/
    public int compareTo(PhaseBean phase) {
        if (getProjectId().compareTo(phase.getProjectId()) == 0) {
            return getName().compareTo(phase.getName());
        } else {
            return getProjectId().compareTo(phase.getProjectId());
        }
    }

    /**
     * Get the list of children of the phase.
     * @return An empty list.
     * @throws TreeException should never be thrown.
     **/
    public List<TreeNode> getChildren() throws TreeException {
        return new ArrayList<TreeNode>();
    }

    /**
     * Get the unique id for the phase.
     * @return The phase's id.
     **/
    public Integer getPhaseId() { return phaseId; }
    
    /**
     * Get the id of the project associated with the phase.
     * @return The phase's project id.
     **/
    public String getProjectId() { return projectId; }
    
    /**
     * Set the unique id for the phase.
     * @param phaseId The phase's unique id.
     **/
    public void setPhaseId(Integer phaseId) { this.phaseId = phaseId; }
    
    /**
     * Get the list of the phases associated with the specified project.
     * @param project The project that is to have its phases listed.
     * @return The list of the phases associated with the project.
     * @throws MasterListException when there is a problem reading in the phases.
     **/
    public static List<PhaseBean> getPhaseList(ProjectBean project) throws MasterListException {
        List<PhaseBean> phaseList = new ArrayList<PhaseBean>();

        Connection conn = DatabaseAccessBean.getConnection();
        try {
            String sql = "SELECT phase_id,name,hide_flag FROM phase WHERE project_id=?";
            PreparedStatement stmt = conn.prepareStatement(sql);
            stmt.setString(1,project.getProjectId());
            ResultSet results = stmt.executeQuery();
            while (results.next()) {
                PhaseBean phase = new PhaseBean(project);
                phase.setPhaseId(results.getInt(1));
                phase.setName(results.getString(2));
                phase.setHidden(results.getBoolean(3));
                phaseList.add(phase);
            }
            results.close();
            stmt.close();
        } catch (SQLException e) {
            throw new MasterListException("Unable to load phases for project: "+project.getProjectId(),e);
        }
        
        return phaseList;
    }

    /**
     * Load the phase from the database.
     * @param phaseId The id of the phase to be loaded.
     * @return The phase specified by the phase id.
     * @throws MasterListException when there is a problem loading the phase.
     **/
    public static PhaseBean loadPhase(Integer phaseId) throws MasterListException {
        Connection conn = DatabaseAccessBean.getConnection();
        PhaseBean phase = null;
        try {
            String sql = "SELECT phase_id,project_id,name,hide_flag FROM phase WHERE phase_id=?";
            PreparedStatement stmt = conn.prepareStatement(sql);
            stmt.setInt(1,phaseId);
            ResultSet result = stmt.executeQuery();
            if (result.next()) {
                phase = new PhaseBean();
                phase.setPhaseId(result.getInt(1));
                phase.setProjectId(result.getString(2));
                phase.setName(result.getString(3));
                phase.setHidden(result.getBoolean(4));
            }
            result.close();
            stmt.close();
        } catch (SQLException e) {
            throw new MasterListException("Unable to load phase: "+phaseId,e);
        }
        return phase;
    }
    
    /**
     * Change the hide status of the phase.
     * @param hideFlag <code>true</code> if the phase is to be hidden,
     * <code>false</code> if is not.
     * @throws MasterListException when there is a problem changing the hide
     * flag for the phase.
     **/
    public void updateHideStatus(boolean hideFlag) throws MasterListException {
        try {
            // Get a connection to the database from the connection pool
            Connection conn = DatabaseAccessBean.getConnection();

            // Create the statement and execute it to save the hidden status.
            String sql = "UPDATE phase SET hide_flag=? WHERE phase_id=?";
            PreparedStatement stmt = conn.prepareStatement(sql);
            stmt.setBoolean(1,hideFlag);
            stmt.setInt(2,getPhaseId());
            stmt.executeUpdate();

            // Properly close the open streams.
            stmt.close();
        } catch (SQLException e) {
            throw new MasterListException("There was a problem changing the" +
                    " hidden status for phase: "+getName()+".",e);
        }
    }

    /**
     * Determine if this phase is associated with any datasets.
     * @return <code>true</code> if the phase is associated with at least one data set,
     * <code>false</code> otherwise.
     * @throws MasterListException when there is a problem determing if the phase is
     * associated to a dataset.
     **/
    public boolean isAssociated() throws MasterListException {
        if (associated == null) {
            try {
                Connection conn = DatabaseAccessBean.getConnection();
                String sql = "SELECT COUNT(dataset_id) > 0 FROM dataset_phase WHERE phase_id=?";
                PreparedStatement stmt = conn.prepareStatement(sql);
                stmt.setInt(1,getPhaseId());
                ResultSet results = stmt.executeQuery();
                if (results.next()) {
                    associated = results.getBoolean(1);
                }
                results.close();
                stmt.close();
            } catch (SQLException e) {
                throw new MasterListException("Unable to determine dataset associations for phase: "+getProjectId()+":"+getName(),e);
            }
        }
        return associated;
    }

    /**
     * Set the flag that marks the phase as associated to a dataset.
     * @param associated <code>true</code> if the phase is associated to a dataset,
     * <code>false</code> otherwise.
     **/
    public void setAssociated(Boolean associated) {
        this.associated = associated;
    }
    
    /**
     * Set the project id for the phase.
     * @param projectId The id of the project associated with the phase.
     **/
    public void setProjectId(String projectId) {
        this.projectId = projectId;
    }

    /**
     * Get the unique id for the phase.
     * @return The id of the phase.
     **/
    public Integer getId() { return getPhaseId(); }

    /**
     * Get the name of the phase.
     * @return The phase name.
     **/
    public String getName() { return name; }
    
    /**
     * Set the name of the phase.
     * @param name The phase's name.
     **/
    public void setName(String name) { this.name = name; }

    /**
     * Get the list of parents of the phase.
     * @return An empty list.
     * @throws MasterListException should never be thrown.
     **/
    public List<Selectable> getParents() throws MasterListException {
        return new ArrayList<Selectable>();
    }

    /**
     * Get the unique id used by a Selector for the phase.
     * @return The phases selectable id.
     **/
    public Integer getSelectableId() { return (getType()+getPhaseId()).hashCode(); }

    /**
     * Get the type of classification for the phase.
     * @return The phase type classification.
     **/
    public String getType() { return PhaseTypeBean.TYPE_NAME; }

    /**
     * Determine if this phase has any parents.
     * @return <code>false</code> since a phase cannot have parents.
     **/
    public boolean hasParent() throws MasterListException { return false; }
    
    /**
     * Determine if this phase is hidden in the database.
     * @return <code>true</code> if the phase is hidden,
     * <code>false</code> if it is not.
     **/
    public boolean isHidden() { return hidden == null ? false : hidden.booleanValue(); }
    
    /**
     * Set the flag that marks the phase as hidden.
     * @param hidden <code>true</code> if the dataset is to be hidden,
     * <code>false</code> if it is not.
     **/
    public void setHidden(Boolean hidden) { this.hidden = hidden; }

    /**
     * Set the flag that marks the phase as hidden.
     * @param hidden <code>true</code> if the dataset is to be hidden,
     * <code>false</code> if it is not.
     **/
    public void setHidden(boolean hidden) { setHidden(new Boolean(hidden)); }
}
