package dln.beans;

/******************************************************************************

DatasetBean.java: The DatasetBean class is a simple container class to hold
one entry in the dln:dataset table.  Each attribute has its own get and
set functions.

Functions to note:
-----------------
	public int populate( ResultSet rs ) throws SQLException
		-Populates this dataset from the given result set

	public int populate( int index, ResultSet rs ) throws SQLException
		-Populates this dataset from the given result set starting at the given 
			index

	public DatasetBean getDBPreparedBean()
		-Returns a new DatasetBean formatted to be entered into the database,
			sets various fields to null instead of the empty string and applies
			some defaults.

	public static String formatTextArea( String notes, int lineMax )
		-Formats the given string to shrink the size of line to linMax.  This
			is used to format the notes field to make things look pretty

Author: Dan Sullivan
Date: ??
******************************************************************************/

import java.io.Serializable;
import java.sql.*;
import java.util.ArrayList;
import java.util.List;

public class DatasetBean implements Serializable {

    private boolean master_flag,readme_flag;
    private Date entered_date;
    private int checked_status_id,checker_id,ext_contact_id,int_contact_id,loaded_status_id,loader_id,note_id;
    private String archive_dir,dataset_id,ingest_dir,load_dir,name,note,remote_url,display_project,dataset_type;
    private List projects,links;

    public DatasetBean() { 
	projects = new ArrayList(); 
	links = new ArrayList();
    }

    public void addLink(XlinkBean link) { links.add(link); }

    public void addProject(String project_id) { if (!hasProject(project_id)) { projects.add(project_id); }  }

    public String getArchiveDirectory() { return archive_dir == null ? "" : archive_dir; }

    public int getCheckedStatusId() { return checked_status_id; }

    public int getCheckerId() { return checker_id; }

    public String getDatasetId() { return dataset_id == null ? "" : dataset_id; }

    public String getDatasetType() { return dataset_type == null ? "Source" : dataset_type; }

    public String getDisplayProject() { return display_project == null ? (String)projects.get(0) : display_project; }

    public Date getEnteredDate() { return entered_date; }

    public int getExternalContactId() { return ext_contact_id; }

    public String getIngestDirectory() { return ingest_dir == null ? "" : ingest_dir; }

    public int getInternalContactId() { return int_contact_id; }

    public List getLinks() { return links; }

    public String getLoadDirectory() { return load_dir == null ? "" : load_dir; }

    public int getLoadedStatusId() { return loaded_status_id; }

    public int getLoaderId() { return loader_id; }

    public boolean getMaster() { return master_flag; }

    public String getName() { return name == null ? "" : name; }

    public String getNote() { return note == null ? "" : note; }

    public int getNoteId() { return note_id; }

    public List getProjects() { return projects; }

    public boolean getReadme() { return readme_flag; }

    public String getRemoteUrl() { return remote_url; }

    public boolean hasProject(String project_id) { return projects.contains(project_id); }

    public void setArchiveDirectory(String archive_dir) { this.archive_dir = archive_dir; }

    public void setCheckedStatusId(int checked_status_id) { this.checked_status_id = checked_status_id; }

    public void setCheckerId(int checker_id) { this.checker_id = checker_id; }

    public void setDatasetId(String dataset_id) { this.dataset_id = dataset_id; }

    public void setDatasetType(String dataset_type) { this.dataset_type = dataset_type; }

    public void setDisplayProject(String display_project) { this.display_project = display_project; }

    public void setEnteredDate(Date entered_date) { this.entered_date = entered_date; }

    public void setExternalContactId(int ext_contact_id) { this.ext_contact_id = ext_contact_id; }

    public void setIngestDirectory(String ingest_dir) { this.ingest_dir = ingest_dir; }

    public void setInternalContactId(int int_contact_id) { this.int_contact_id = int_contact_id; }

    public void setLoadDirectory(String load_dir) { this.load_dir = load_dir; }

    public void setLoadedStatusId(int loaded_status_id) { this.loaded_status_id = loaded_status_id; }

    public void setLoaderId(int loader_id) { this.loader_id = loader_id; }

    public void setMaster(boolean master_flag) { this.master_flag = master_flag; }

    public void setName(String name) { this.name = name; }

    public void setNote(String note) { this.note = note; }

    public void setNoteId(int note_id) { this.note_id = note_id; }

    public void setReadme(boolean readme_flag) { this.readme_flag = readme_flag; }

    public void setRemoteUrl(String remote_url) { this.remote_url = remote_url; }

    /*
	public DatasetBean getDBPreparedBean()
	{
		DatasetBean ds = new DatasetBean();

		ds.setLoaderBean( loaderBean );
		ds.setCheckerBean( checkerBean );
		ds.setIntContactBean( intContactBean );

		//		ds.setDsid( dsid );
		ds.stormId = blankToNull( stormId );
		ds.title =  blankToNull( title );
		ds.notes = blankToNull( notes );
		ds.project = blankToNull( project );
		ds.loader = blankToNull( loader );
		ds.checker = blankToNull( checker );
		ds.intContact = blankToNull( intContact );
		ds.extContact = blankToNull( extContact );
		ds.extEmail = blankToNull( extEmail );
		ds.ingest = blankToNull( ingest );
		ds.archive = blankToNull( archive );
		ds.remoteUrl = blankToNull( remoteUrl );

		if( date == null )
			ds.setDate( new java.sql.Date( new java.util.Date().getTime() ) );
		else
			ds.setDate( date );

		ds.setReadme( readme );
		ds.setMaster( master );
		ds.setChecked( checked );
		ds.setLoaded( loaded );

		return ds;
	}

	public String blankToNull( String str )
	{
		if( str.equals( "" ) )
			str = null;

		return str;
	}

	public static String formatTextArea( String notes, int lineMax )
	{
		int max = lineMax;

		if( notes == null )
			return notes;

		String ret = new String();
		if( notes.length() <= max )
			return notes;
		
		StringTokenizer lines = new StringTokenizer( notes, "\r\n" );
		while( lines.hasMoreTokens() )
		{
			String ret_line = new String();
			String line = lines.nextToken();

			if( line.length() > max + 1 )
			{
				StringTokenizer words = new StringTokenizer( line, " " );
				int len = 0;
				while( words.hasMoreTokens() )
				{
					String word = words.nextToken();
	
					if( len + word.length() > max )
					{
						ret_line = ret_line + "\n";
						len = 0;
					}

					ret_line = ret_line + word + " ";
					len += word.length() + 1;
				}
			}
			else
				ret_line = line;
			ret = ret + "\n" + ret_line;
		}

		return ret;
	}
    */
}
