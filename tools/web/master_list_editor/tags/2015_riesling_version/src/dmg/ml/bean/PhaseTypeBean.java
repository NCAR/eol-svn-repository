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
 * The PhaseTypeBean is a specialized ClassificationTypeBean used
 * exclusively for phases.
 *
 * @author Joel Clawson
 */
public class PhaseTypeBean extends ClassificationTypeBean {
    
    /**
     * The name of the Phase Type.
     **/
    public static final String TYPE_NAME = "Phase";
    
    /**
     * Create a new instance of a PhaseTypeBean.
     * @param treeState The container that holds the tree node expansion states.
     * @param project The project the phases of this type belong to.
     **/
    public PhaseTypeBean(TreeState treeState, ProjectBean project) {
        super(treeState,project);
        setName(TYPE_NAME);
    }

    /**
     * Get the list of phases for the project.
     * @return The list of phases associated to the project.
     * @throws TreeException when there is a problem loading the phases.
     **/
    public List<TreeNode> getChildren() throws TreeException {
        List<TreeNode> phaseList = new ArrayList<TreeNode>();
        
        try {
            Connection conn = DatabaseAccessBean.getConnection();
            String sql = "SELECT phase_id,project_id,name,hide_flag FROM phase WHERE project_id=?";
            PreparedStatement stmt = conn.prepareStatement(sql);
            stmt.setString(1,getProject().getProjectId());
            ResultSet results = stmt.executeQuery();
            while (results.next()) {
                PhaseBean phase = new PhaseBean(treeState,getProject());
                phase.setPhaseId(results.getInt(1));
                phase.setName(results.getString(3));
                phase.setHidden(results.getBoolean(4));
                phase.setParent(this);
                phaseList.add(phase);
            }
            results.close();
            stmt.close();
        } catch (SQLException e) {
            throw new TreeException(e.getMessage());
        } catch (MasterListException e) {
            throw new TreeException(e.getMessage());
        }
        
        return phaseList;
    }
}
