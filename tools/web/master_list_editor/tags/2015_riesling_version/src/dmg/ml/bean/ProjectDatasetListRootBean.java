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
import java.sql.Types;
import java.util.ArrayList;
import java.util.List;

/**
 * <p>The ProjectDatasetListRootBean class is a specialized ProjectBean that
 * defines the root node of the dataset list tree.  It defines its children
 * to be classifications that have both data sets and subclassifications as children.</p>
 *
 * @author Joel Clawson
 */
public class ProjectDatasetListRootBean extends ProjectBean {

    private ClassificationBean classification;
    private ClassificationTypeBean classificationType;
    private PhaseBean phase;

    
    /**
     * Create a new instance of a ProjectDatasetListRootBean.
     * @param state The container for the expansion states of the nodes in the
     * tree.
     * @param project The project that is this root node.
     **/
    public ProjectDatasetListRootBean(TreeState state, ProjectBean project) {
        this(state,project,null,null);
    }
    
    /**
     * Create a new instance of a ProjectDatasetListRootBean.
     * @param state The container for the expansion states of the nodes in the
     * tree.
     * @param project The project that is this root node.
     * @param phase The phase if the tree root is to start with a specific phase.
     **/
    public ProjectDatasetListRootBean(TreeState state, ProjectBean project, PhaseBean phase) {
        this(state,project);
        this.phase = phase;
    }
    
    /**
     * Create a new instance of a ProjectDatasetListRootBean.
     * @param state The container for the expansion states of the nodes in the
     * tree.
     * @param project The project that is this root node.
     * @param classification The classification if the tree root is to start with a 
     * specific classification.
     **/
    public ProjectDatasetListRootBean(TreeState state, ProjectBean project, ClassificationTypeBean classificationType, ClassificationBean classification) {
        super(state);
        setProjectId(project.getProjectId());
        setOriginalId(project.getOriginalId());
        setUrl(project.getUrl());
        setSystemDirectory(project.getSystemDirectory());
        setHomePageUrl(project.getHomePageUrl());
        setLogoUrl(project.getLogoUrl());
        setCssUrl(project.getCssUrl());
        setExpanded(true);
        this.classificationType = classificationType;
        this.classification = classification;
    }
    
    /**
     * Get the list of classifications that have no parent classifications and are 
     * associated with this project.
     * @return The list of classifications associated with this project.
     * @throws TreeException when there is a problem generating the classifications
     * for this project.
     **/
    public List<TreeNode> getChildren() throws TreeException {
        List<TreeNode> children = new ArrayList<TreeNode>();

        try {
            // Get a connection to the database from the connection pool.
            Connection conn = DatabaseAccessBean.getConnection();

            if (classification != null) {
                String sql = "SELECT classification.class_id,classification.name,classification.type_id,classification_type.name,hide_flag FROM classification JOIN classification_type ON classification.type_id=classification_type.type_id JOIN project_classification ON classification.class_id=project_classification.class_id WHERE project_id=? AND classification.class_id=? ORDER BY classification.name";
                PreparedStatement stmt = conn.prepareStatement(sql);
                stmt.setString(1,getProjectId());
                stmt.setInt(2,classification.getClassificationId());
                
                ResultSet results = stmt.executeQuery();
                while (results.next()) {
                    ClassificationDatasetListBean bean = new ClassificationDatasetListBean(treeState,this);
                    bean.setClassificationId(results.getInt(1));
                    bean.setName(results.getString(2));
                    bean.setTypeId(results.getInt(3));
                    bean.setTypeName(results.getString(4));
                    bean.setHidden(results.getBoolean(5));
                    bean.setParent(this);
                    children.add(bean);
                }
                results.close();
                stmt.close();
            } else if (classificationType != null) {
                if (classificationType instanceof PhaseTypeBean) {
                    String sql = "SELECT phase_id,name,hide_flag FROM phase WHERE project_id=?";
                    PreparedStatement stmt = conn.prepareStatement(sql);
                    stmt.setString(1,getProjectId());
                    ResultSet results = stmt.executeQuery();
                    while (results.next()) {
                        PhaseBean phase = new PhaseBean(treeState,this);
                        phase.setPhaseId(results.getInt(1));
                        phase.setName(results.getString(2));
                        phase.setHidden(results.getBoolean(3));
                        phase.setParent(this);
                        children.add(phase);
                    }
                    results.close();
                    stmt.close();
                } else {
                    String sql = "SELECT DISTINCT(classification_type.type_id),classification_type.name FROM classification_type JOIN classification ON classification_type.type_id=classification.type_id JOIN project_classification ON classification.class_id=project_classification.class_id WHERE project_id=? AND classification_type.type_id=? ORDER BY classification_type.name";
                    PreparedStatement stmt = conn.prepareStatement(sql);
                    stmt.setString(1,getProjectId());
                    stmt.setInt(2,classificationType.getTypeId());

                    ResultSet results = stmt.executeQuery();
                    while (results.next()) {
                        ClassificationTypeDatasetListBean type = new ClassificationTypeDatasetListBean(treeState,this);
                        type.setTypeId(results.getInt(1));
                        type.setName(results.getString(2));
                        type.setParent(this);
                        children.add(type);
                    }
                    results.close();
                    stmt.close();
                }
            } else if (phase != null && (phase.getPhaseId() == null || phase.getPhaseId() == 0)) {
                String sql = "SELECT phase_id,project_id,name,hide_flag FROM phase WHERE project_id=?";
                PreparedStatement stmt = conn.prepareStatement(sql);
                stmt.setString(1,getProjectId());
                ResultSet results = stmt.executeQuery();
                while (results.next()) {
                    PhaseDatasetListBean bean = new PhaseDatasetListBean(treeState,this);
                    bean.setPhaseId(results.getInt(1));
                    bean.setName(results.getString(3));
                    bean.setHidden(results.getBoolean(4));
                    bean.setParent(this);
                    children.add(bean);
                }
                results.close();
                stmt.close();
            } else if (phase != null) {
                String sql = "SELECT phase_id,project_id,name,hide_flag FROM phase WHERE phase_id=?";
                PreparedStatement stmt = conn.prepareStatement(sql);
                stmt.setInt(1,phase.getPhaseId());
                ResultSet results = stmt.executeQuery();
                while (results.next()) {
                    PhaseDatasetListBean bean = new PhaseDatasetListBean(treeState,this);
                    bean.setPhaseId(results.getInt(1));
                    bean.setName(results.getString(3));
                    bean.setHidden(results.getBoolean(4));
                    bean.setParent(this);
                    children.add(bean);
                }
                results.close();
                stmt.close();
            } else {
                String sql = "SELECT dataset.dataset_id,name,url,doc_url,date_expected,date_updated,preliminary_flag,author_pi,date_posted,hide_flag,in_progress_flag FROM dataset JOIN dataset_project ON dataset.dataset_id=dataset_project.dataset_id WHERE project_id=? ORDER BY dataset.dataset_id";
                PreparedStatement stmt = conn.prepareStatement(sql);
                stmt.setString(1,getProjectId());
                
                ResultSet results = stmt.executeQuery();
                while (results.next()) {
                    DatasetProjectBean dataset = new DatasetProjectBean(treeState,this);
                    dataset.setDatasetId(results.getString(1));
                    dataset.setName(results.getString(2));
                    dataset.setUrl(results.getString(3));
                    dataset.setDocUrl(results.getString(4));
                    dataset.setDateExpected(results.getString(5));
                    dataset.setDateUpdated(results.getString(6));
                    dataset.setPreliminary(results.getBoolean(7));
                    dataset.setAuthorPi(results.getString(8));
                    dataset.setDatePosted(results.getString(9));
                    dataset.setHidden(results.getBoolean(10));
                    dataset.setInProgress(results.getBoolean(11));
                    dataset.setParent(this);
                    children.add(dataset);
                }
                results.close();
                stmt.close();
            }
        } catch (MasterListException e) {
            // Convert a MasterListException to a TreeException.
            throw new TreeException(e.getMessage());
        } catch (SQLException e) {
            // Convert a SQLException to a TreeException.
            throw new TreeException(e.getMessage());
        }
                
        return children;
    }
}
