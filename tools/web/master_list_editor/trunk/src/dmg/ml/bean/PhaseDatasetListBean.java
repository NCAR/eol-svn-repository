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
 * The PhaseDatasetListBean class is a specialized PhaseBean used for displaying
 * a phase tree with dataset children.
 *
 * @author Joel Clawson
 */
public class PhaseDatasetListBean extends PhaseBean {
    
    private ProjectBean project;
    
    /**
     * Create a new instance of a PhaseDatasetListBean.
     * @param treeState The container for the tree expansion states that the phase is in.
     * @param project The project the phase is a part of.
     **/
    public PhaseDatasetListBean(TreeState treeState, ProjectBean project) {
        super(treeState,project);
        this.project = project;
    }

    /**
     * Get the list of datasets that are associated with this phase.
     * @return The list of datasets associated with this phase.
     * @throws MasterListException when there is a problem reading in the datasets.
     **/
    public List<TreeNode> getChildren() throws TreeException {
        List<TreeNode> children = new ArrayList<TreeNode>();
        try {
            Connection conn = DatabaseAccessBean.getConnection();
            String sql = "SELECT dataset.dataset_id,name,url,doc_url,author_pi,date_expected,date_updated,date_posted,hide_flag,in_progress_flag,preliminary_flag FROM dataset JOIN dataset_phase ON dataset.dataset_id=dataset_phase.dataset_id JOIN dataset_project ON dataset.dataset_id=dataset_project.dataset_id WHERE phase_id=? AND project_id=? ORDER BY dataset.name,author_pi";
            PreparedStatement stmt = conn.prepareStatement(sql);
            stmt.setInt(1,getPhaseId());
            stmt.setString(2,project.getProjectId());
            ResultSet results = stmt.executeQuery();
            while (results.next()) {
                DatasetProjectBean dataset = new DatasetProjectBean(treeState,project);
                dataset.setDatasetId(results.getString(1));
                dataset.setName(results.getString(2));
                dataset.setUrl(results.getString(3));
                dataset.setDocUrl(results.getString(4));
                dataset.setAuthorPi(results.getString(5));
                if (results.getDate(6) != null) { dataset.setDateExpected(results.getString(6)); }
                if (results.getDate(7) != null) { dataset.setDateUpdated(results.getString(7)); }
                if (results.getDate(8) != null) { dataset.setDatePosted(results.getString(8)); }
                dataset.setHidden(results.getBoolean(9));
                dataset.setInProgress(results.getBoolean(10));
                dataset.setPreliminary(results.getBoolean(11));
                dataset.setParent(this);
                children.add(dataset);
            }
            results.close();
            stmt.close();
        } catch (MasterListException e) {
            throw new TreeException(e.getMessage());
        } catch (SQLException e) {
            throw new TreeException(e.getMessage());
        }
        return children;
    }
}
