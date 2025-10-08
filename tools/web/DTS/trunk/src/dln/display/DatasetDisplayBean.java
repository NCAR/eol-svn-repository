package dln.display;

import dln.beans.*;
import dln.util.*;
import java.util.*;

/**
 * The DatasetDisplayBean is a JSP session state for maintaining display information for the
 * data set list and data set view.  It maintains which field is currently sorted and in which direction,
 * what list is currently (was last) viewed along with data set and note filters.
 * 
 * @author jclawson
 * @author Dan Sullivan
 */
public class DatasetDisplayBean {

	private boolean datasetView;
	private boolean reverseSort, showChecked, showDocumented, showLoaded, showMaster;
	private int datasetCount, displayView, sortField;
	private int selectedAuthorId, selectedNoteTypeId;
	private String displayId;

	/** Constant for the project list display view. */
	public static final int PROJECT_DISPLAY_VIEW = 1;
	/** Constant for the ingest contact display view. */
	public static final int INGEST_CONTACT_DISPLAY_VIEW = 2;
	/** Constant for the load contact display view. */
	public static final int LOAD_CONTACT_DISPLAY_VIEW = 3;
	/** Constant for the check contact display view. */
	public static final int CHECK_CONTACT_DISPLAY_VIEW = 4;

	
	/** Constant for the data set ID sort field. */
	public static final int DATASET_ID_SORT = 0;
	/** Constant for the data set name sort field. */
	public static final int NAME_SORT = 1;
	/** Constant for the date sort field. */
	public static final int DATE_SORT = 2;
	/** Constant for the ingest contact sort. */
	public static final int INGEST_CONTACT_SORT = 3;
	/** Constant for the load contact sort. */
	public static final int LOAD_CONTACT_SORT = 4;
	/** Constant for the check contact sort. */
	public static final int CHECK_CONTACT_SORT = 5;
	/** Constant for the questions sort. */
	public static final int QUESTION_SORT = 6;
	
	/**
	 * Get the database ID of the author to be shown in the data set view note list.
	 * @return The author's database ID.
	 */
	public int getAuthorFilterSelection() { return selectedAuthorId; }

	/**
	 * Get the number of data sets in the listing.
	 * @return The number of data sets in the listing.
	 */
	public int getDatasetCount() { return datasetCount; }
	
	/**
	 * Get the ID of the item currently being displayed in the view.  This will be either
	 * the project ID in a project list or the contact ID in one of the task lists.
	 * @return The ID of the currently displayed item.
	 */
	public String getDisplayId() { return displayId; }
	
	/**
	 * Get the ID of the current display view.
	 * @return The display view's ID.
	 */
	public int getDisplayView() { return displayView; }
	
	/**
	 * Get the database ID of the note type to be shown in the data set view
	 * note list.
	 * @return The note type database ID.
	 */
	public int getNoteTypeFilterSelection() { return selectedNoteTypeId; }

	/**
	 * Determine if the display should show data sets that have been checked.
	 * @return <code>true</code> if the list shows data sets that have been checked,
	 * <code>false</code> if it does not.
	 */
	public boolean getShowChecked() { return showChecked; }

	/**
	 * Determine if the display should show data sets that have documentation.
	 * @return <code>true</code> if the list shows data sets that have documentation,
	 * <code>false</code> if it does not.
	 */
	public boolean getShowDocumented() { return showDocumented; }

	/**
	 * Determine if the display should show data sets that have been loaded.
	 * @return <code>true</code> if the list shows data sets that have been loaded,
	 * <code>false</code> if it does not.
	 */
	public boolean getShowLoaded() { return showLoaded; }

	/**
	 * Determine if the display should show data sets that are in the project's master list.
	 * @return <code>true</code> if the list shows data sets that are in the project's master list,
	 * <code>false</code> if it does not.
	 */
	public boolean getShowMaster() { return showMaster; }

	/**
	 * Determine if the current display is a checker list.
	 * @return <code>true</code> if the display is a checker list,
	 * <code>false</code> if it is not.
	 */
	public boolean isDisplayCheckerList() { return displayView == CHECK_CONTACT_DISPLAY_VIEW; }
	
	/**
	 * Determine if the current display is a data set detail view.
	 * @return <code>true</code> if the display is a data set detail view,
	 * <code>false</code> if it is not.
	 */
	public boolean isDisplayDatasetView() { return datasetView; }
	
	/**
	 * Determine if the current display is an ingester list.
	 * @return <code>true</code> if the display is an ingester list,
	 * <code>false</code> if it is not.
	 */
	public boolean isDisplayIngesterList() { return displayView == INGEST_CONTACT_DISPLAY_VIEW; }
	
	/**
	 * Determine if the current display is a loader list.
	 * @return <code>true</code> if the display is a loader list,
	 * <code>false</code> if it is not.
	 */
	public boolean isDisplayLoaderList() { return displayView == LOAD_CONTACT_DISPLAY_VIEW; }
	
	/**
	 * Determine if the current display is a project list.
	 * @return <code>true</code> if the display is a project list,
	 * <code>false</code> if it is not.
	 */
	public boolean isDisplayProjectList() { return displayView == PROJECT_DISPLAY_VIEW; }
	
	/**
	 * Determine if the current display is sorted in reverse order.
	 * @return <code>true</code> if the list is sorted in reverse order,
	 * <code>false</code> if it is sorted in the natural sort order.
	 */
	public boolean isReverseSort() { return reverseSort; }
	
	/**
	 * Determine if the current display is sorted by the data set check contact.
	 * @return <code>true</code> if the list is sorted by the data set check contact,
	 * <code>false</code> if it is not.
	 */
	public boolean isSortByChecker() { return sortField == CHECK_CONTACT_SORT; }
	
	/**
	 * Determine if the current display is sorted by the data set entry date.
	 * @return <code>true</code> if the list is sorted by the data set entry date,
	 * <code>false</code> if it is not.
	 */
	public boolean isSortByDate() { return sortField == DATE_SORT; }
	
	/**
	 * Determine if the current display is sorted by the data set ID.
	 * @return <code>true</code> if the list is sorted by the data set ID,
	 * <code>false</code> if it is not.
	 */
	public boolean isSortById() { return sortField == DATASET_ID_SORT; }

	/**
	 * Determine if the current display is sorted by the data set ingest contact.
	 * @return <code>true</code> if the list is sorted by the data set ingest contact,
	 * <code>false</code> if it is not.
	 */
	public boolean isSortByIngester() { return sortField == INGEST_CONTACT_SORT; }
	
	/**
	 * Determine if the current display is sorted by the data set load contact.
	 * @return <code>true</code> if the list is sorted by the data set load contact,
	 * <code>false</code> if it is not.
	 */
	public boolean isSortByLoader() { return sortField == LOAD_CONTACT_SORT; }
	
	/**
	 * Determine if the current display is sorted by the data set name.
	 * @return <code>true</code> if the list is sorted by the data set name,
	 * <code>false</code> if it is not.
	 */
	public boolean isSortByName() { return sortField == NAME_SORT; }
	
	/**
	 * Determine if the current display is sorted by the data set questions status.
	 * @return <code>true</code> if the list is sorted by the data set questions status,
	 * <code>false</code> if it is not.
	 */
	public boolean isSortByQuestions() { return sortField == QUESTION_SORT; }
	
	/**
	 * Set the database ID of the author to be shown in the data set view note list.
	 * @param id The author's database ID.
	 */
	public void setAuthorFilterSelection(int id) { selectedAuthorId = id; }
	
	/**
	 * Set the display view to the check contact list.
	 * @param checkContactId The database ID of the check contact.
	 */
	public void setCheckDisplayView(int checkContactId) {
		if(!isDisplayCheckerList()) {
			setShowChecked(false);
			setShowDocumented(true);
			setShowLoaded(true);
		}
		setDisplayView(CHECK_CONTACT_DISPLAY_VIEW);
		setDisplayId(Integer.toString(checkContactId));
	}
	
	/**
	 * Set the display view to the data set details view.
	 */
	public void setDatasetDisplayView() {
		if (!isDisplayDatasetView()) {
			setNoteTypeFilterSelection(-1);
			setAuthorFilterSelection(-1);
		}
		datasetView = true;
	}
	
	/**
	 * Set the number of data sets displayed in the view.
	 * @param count The number of data set in the list.
	 */
	private void setDatasetCount(int count) { datasetCount = count; }
	
	/**
	 * Set the ID of the object being displayed in the view.
	 * @param displayId The displayed view's object ID.
	 */
	private void setDisplayId(String displayId) { this.displayId = displayId; }
	
	/**
	 * Set the ID of the view being displayed.
	 * @param viedId The display's view id.
	 */
	private void setDisplayView(int viewId) {
		datasetView = false;
		displayView = viewId;
	}
	
	/**
	 * Set the display view to the ingest contact list.
	 * @param ingestContactId The database ID of the ingest contact.
	 */
	public void setIngestDisplayView(int ingestContactId) {
		if (!isDisplayIngesterList()) {
			setShowChecked(true);
			setShowDocumented(true);
			setShowLoaded(true);
		}
		setDisplayView(INGEST_CONTACT_DISPLAY_VIEW);
		setDisplayId(Integer.toString(ingestContactId));
	}
	
	/**
	 * Set the display view to the listing before entering the data set view.
	 */
	public void setListView() { datasetView = false; }
	
	/**
	 * Set the display view to the load contact list.
	 * @param loadContactId The database ID of the load contact.
	 */
	public void setLoadDisplayView(int loadContactId) {
		if (!isDisplayLoaderList()) {
			setShowChecked(true);
			setShowDocumented(true);
			setShowLoaded(false);
		}
		setDisplayView(LOAD_CONTACT_DISPLAY_VIEW);
		setDisplayId(Integer.toString(loadContactId));
	}
	
	/**
	 * Set the ID of the note type that is to be displayed in the data set view.
	 * @param id The note type ID.
	 */
	public void setNoteTypeFilterSelection(int id) { selectedNoteTypeId = id; }
	
	/**
	 * Set the display view to the project list.
	 * @param projectId The ID of the project.
	 */
	public void setProjectDisplayView(String projectId) {
		if (!isDisplayProjectList() && !isDisplayDatasetView()) {
			setShowChecked(true);
			setShowDocumented(true);
			setShowLoaded(true);
			setShowMaster(true);
		}
		setDisplayView(PROJECT_DISPLAY_VIEW);
		setDisplayId(projectId);
	}

	/**
	 * Set the flag to mark the sorting as a reverse sort.
	 * @param reverseSort <code>true</code> to set the sort as a reverse sort, <code>false</code>
	 * to set the sort as a natural sort order.
	 */
	private void setReverseSort(boolean reverseSort) { this.reverseSort = reverseSort; }
	
	/**
	 * Mark the filter to show/hide checked data sets in the display list.
	 * @param show <code>true</code> if checked data sets are to be displayed,
	 * <code>false</code> if they are not.
	 */
	public void setShowChecked(boolean show) { showChecked = show; }

	/**
	 * Mark the filter to show/hide documented data sets in the display list.
	 * @param show <code>true</code> if the documented data sets are to be displayed,
	 * <code>false</code> if they are not.
	 */
	public void setShowDocumented(boolean show) { showDocumented = show; }
	
	/**
	 * Mark the filter to show/hide loaded data sets in the display list.
	 * @param show <code>true</code> if loaded data sets are to be displayed,
	 * <code>false</code> if they are not.
	 */
	public void setShowLoaded(boolean show) { showLoaded = show; }

	/**
	 * Mark the filter to show/hide master list data sets in the display list.
	 * @param show <code>true</code> if master list data sets are to be displayed,
	 * <code>false</code> if they are not.
	 */
	public void setShowMaster(boolean show) { showMaster = show; }

	/**
	 * Set the field that is to be sorted and flip the sort direction if the sort field is the
	 * currently sorted field.
	 * @param fieldId The ID of the field to be sorted.
	 */
	public void setSortField(int fieldId) {
		setReverseSort(sortField == fieldId ? !isReverseSort() : false);
		sortField = fieldId;
	}
	
	/**
	 * Update the specified data set list with the filters and sort it in the expected order.
	 * @param datasets The list of data sets to be filtered and sorted.
	 * @param users The mapping of users.
	 * @param statuses The mapping of statuses.
	 * @return The list of filtered and sorted data sets.
	 */
	public List<DatasetBean> update(List<DatasetBean> datasets, Map<Integer, UserBean> users, Map<Integer, StatusBean> statuses) {
		// A container for holding data sets to be included in the final list.
		TreeMap<String, DatasetBean> mapping = new TreeMap<String, DatasetBean>();
		

		for (Iterator<DatasetBean> itr = datasets.iterator(); itr.hasNext(); ) {
			DatasetBean dataset = itr.next();
			// Apply the filters.
			if ((!getShowChecked() && dataset.isChecked(statuses)) ||
				(!getShowLoaded() && dataset.isLoaded(statuses)) ||
				(getDisplayView() == PROJECT_DISPLAY_VIEW && !getShowMaster() && dataset.isInMasterList(getDisplayId())) ||
				(!getShowDocumented() && dataset.isDocumented()) ||
				(getDisplayView() == PROJECT_DISPLAY_VIEW && mapping.containsKey(dataset.getDatasetId()) && 
						mapping.get(dataset.getDatasetId()).getEntryDate().compareTo(dataset.getEntryDate()) > 0)) {
				// Don't put the data set in the mapping.
				itr.remove();
			} 
			// The data set made it through, so add it to the mapping.
			else {
				mapping.put(dataset.getDatasetId(), dataset);
			}
		}

		// Only limit the data set version for the project as it should only show the most
		// recent version in the list.  The task contacts will show all versions of all data sets.
		if (getDisplayView() == PROJECT_DISPLAY_VIEW) {
			datasets = new ArrayList<DatasetBean>(mapping.values());
		}
		
		// Sort the data set list.
		if (isSortByDate()) {
			Collections.sort(datasets, new DatasetEntryDateComparator(isReverseSort()));
		} else if (isSortByName()) {
			Collections.sort(datasets, new DatasetNameComparator(isReverseSort()));
		} else if (isSortByLoader()) {
			Collections.sort(datasets, new DatasetLoaderComparator(isReverseSort(), users));
		} else if (isSortByChecker()) {
			Collections.sort(datasets, new DatasetCheckerComparator(isReverseSort(), users));
		} else if (isSortByIngester()) {
			Collections.sort(datasets, new DatasetIngesterComparator(isReverseSort(), users));
		} else if (isSortByQuestions()) {
			Collections.sort(datasets, new DatasetQuestionsComparator(isReverseSort()));
		} else {
			Collections.sort(datasets, new DatasetIdComparator(isReverseSort()));
		}
				
		// Set the number of data sets in the list for later use.
		setDatasetCount(datasets.size());
		
		return datasets;
	}
}
