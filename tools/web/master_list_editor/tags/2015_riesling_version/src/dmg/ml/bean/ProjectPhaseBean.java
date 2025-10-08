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
 * The ProjectPhaseBean class is a ProjectBean associated to phases.
 *
 * @author Joel Clawson
 */
public class ProjectPhaseBean extends ProjectBean {

    /**
     * Create a new instance of a ProjectPhaseBean.
     * @param state The container for holding the tree expansion states.
     **/    
    public ProjectPhaseBean(TreeState state) { super(state); }
    
    /**
     * Create a new instance of a ProjectPhaseBean.
     * @param state The container for holding the tree expansion states.
     * @param project The project represented by this bean.
     **/    
    public ProjectPhaseBean(TreeState state, ProjectBean project) { 
        super(state);
        setProjectId(project.getProjectId());
        setOriginalId(project.getOriginalId());
        setSystemDirectory(project.getSystemDirectory());
        setUrl(project.getUrl());
        setHomePageUrl(project.getHomePageUrl());
        setLogoUrl(project.getLogoUrl());
        setCssUrl(project.getCssUrl());
    }

    /**
     * Get the list of phases associated to this project.
     * @return The list of phases associated to this project.
     * @throws TreeException when there is a problem loading the project's phases.
     **/
    public List<TreeNode> getChildren() throws TreeException {
        List<TreeNode> phases = new ArrayList<TreeNode>();
        
        try {
            Connection conn = DatabaseAccessBean.getConnection();
            String sql = "SELECT phase_id,project_id,name,hide_flag FROM phase WHERE project_id=? ORDER BY name";
            PreparedStatement stmt = conn.prepareStatement(sql);
            stmt.setString(1,getProjectId());
            
            ResultSet results = stmt.executeQuery();
            while (results.next()) {
                PhaseBean phase = new PhaseBean(treeState,this);
                phase.setPhaseId(results.getInt(1));
                phase.setName(results.getString(3));
                phase.setHidden(results.getBoolean(4));
                phase.setParent(this);
                phases.add(phase);
            }
            results.close();
            stmt.close();
        } catch (SQLException e) {
            throw new TreeException(e.getMessage());
        } catch (MasterListException e) {
            throw new TreeException(e.getMessage());
        }
        
        return phases;
    }    
}
