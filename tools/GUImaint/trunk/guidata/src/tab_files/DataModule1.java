package tab_files;

import java.awt.*;

import com.borland.dx.dataset.*;
import com.borland.dx.sql.dataset.*;

/**
 * <p>Title: GUImaint for CODIAC</p>
 * <p>Description: MySQL db version</p>
 * <p>Copyright: Copyright (c) 2004, 2005, 2006, 2007, 2008</p>
 * <p>Company: NCAR/EOL</p>
 * @author Don Stott
 * @version 2.20
 */

public class DataModule1 implements DataModule {
  private static DataModule1 myDM;
  Database db_codiac = new Database();

// ParameterRows
  ParameterRow paramRowUser = new ParameterRow();
  ParameterRow paramRowSearchID = new ParameterRow();
  ParameterRow paramRowID = new ParameterRow();
  ParameterRow paramRowProjID = new ParameterRow();
  ParameterRow paramRowFormat = new ParameterRow();
  ParameterRow paramRowFile = new ParameterRow();

// CODIAC Queries
  QueryDataSet queryContact = new QueryDataSet();
  QueryDataSet queryDataset1 = new QueryDataSet();
  QueryDataSet queryDatasetProj = new QueryDataSet();
  QueryDataSet queryProjDataset = new QueryDataSet();
  QueryDataSet queryInternalContact = new QueryDataSet();
  QueryDataSet queryRevContact = new QueryDataSet();
  QueryDataSet queryProjectID = new QueryDataSet();
  QueryDataSet queryProject = new QueryDataSet();
  QueryDataSet queryDatasetOptions = new QueryDataSet();
  QueryDataSet queryReadme = new QueryDataSet();
  QueryDataSet queryFile = new QueryDataSet();
  QueryDataSet queryFormat = new QueryDataSet();
  QueryDataSet queryFormatConv = new QueryDataSet();
  QueryDataSet queryFrequency = new QueryDataSet();
  QueryDataSet queryPlots = new QueryDataSet();
  QueryDataSet queryDatasetPI = new QueryDataSet();
  QueryDataSet queryDatasetUser = new QueryDataSet();
  QueryDataSet queryUser = new QueryDataSet();
  QueryDataSet querySpatial = new QueryDataSet();
  QueryDataSet queryProjName = new QueryDataSet();
  QueryDataSet queryPlotIDs = new QueryDataSet();
  QueryDataSet queryTape = new QueryDataSet();
  QueryDataSet queryCategory = new QueryDataSet();
  QueryDataSet queryPlatform = new QueryDataSet();
  QueryDataSet queryDSCategory = new QueryDataSet();
  QueryDataSet queryDSPlatform = new QueryDataSet();
  QueryDataSet queryMedium = new QueryDataSet();
  QueryDataSet queryPassword = new QueryDataSet();
  QueryDataSet queryDSUser = new QueryDataSet();
  QueryDataSet queryPswdUser = new QueryDataSet();
  QueryDataSet queryCatDS = new QueryDataSet();
  QueryDataSet queryProjPrefix = new QueryDataSet();
  QueryDataSet queryThisPrefix = new QueryDataSet();
  QueryDataSet queryDSReferences = new QueryDataSet();
  QueryDataSet queryXlink = new QueryDataSet();
  QueryDataSet queryDatasetXlink = new QueryDataSet();
  QueryDataSet queryProjXlink = new QueryDataSet();
  QueryDataSet queryXlinkProj = new QueryDataSet();
  QueryDataSet queryXlinkName = new QueryDataSet();

// Dataset Views
  DataSetView contactDataSetView = new DataSetView();

// Columns
  Column username = new Column();
  Column dataset_id = new Column();
  Column proj_id = new Column();
  Column format_id = new Column();
  Column time_sel = new Column();
  Column spatial_type = new Column();
  Column contact_type = new Column();
  Column host = new Column();
  Column purpose = new Column();
  Column ds_id = new Column();
  Column intContact = new Column();
  Column search_id = new Column();
  Column dp_dataset_id = new Column();
  Column dp_project_id = new Column();
  Column int_contact = new Column();
  Column project_id = new Column();
  Column format = new Column();
  Column source_format = new Column();
  Column target_format = new Column();
  Column srcContact = new Column();
  Column freq_id = new Column();
  Column displayContact = new Column();
  Column rowrevContact = new Column();
  Column parent_project_id = new Column();
  Column begindate = new Column();
  Column enddate = new Column();
  Column file_begin = new Column();
  Column file_end = new Column();
  Column spatialtype = new Column();
  Column proj_begin = new Column();
  Column proj_end = new Column();
  Column archive_date_file = new Column();
  Column file_reviseTime = new Column();
  Column begin_readme = new Column();
  Column end_readme = new Column();
  Column archive_date = new Column();
  Column revise_time = new Column();
  Column plotType_id = new Column();
  Column time_select = new Column();
  Column stn_select = new Column();
  Column plots_reviseTime = new Column();
  Column projID = new Column();
  Column formatIDfk = new Column();
  Column pi_contact_id = new Column();
  Column purpose_type = new Column();
  Column onlineorderable = new Column();
  Column host_name = new Column();
  Column size = new Column();
  Column use = new Column();
  Column tape_beginDate = new Column();
  Column tape_endDate = new Column();
  Column tape_archiveDate = new Column();
  Column formatID = new Column();
  Column primary_med_id = new Column();
  Column backup_med_id = new Column();
  Column dsIDfk = new Column();
  Column dsID = new Column();
  Column tapeID = new Column();
  Column tape_dsID = new Column();
  Column categoryID = new Column();
  Column platformID = new Column();
  Column user_name = new Column();
  Column parent_category_id = new Column();
  Column size_kb = new Column();
  Column directory = new Column();
  Column filename = new Column();
  Column directory_file = new Column();
  Column x_subset = new Column();
  Column y_subset = new Column();
  Column z_subset = new Column();
  Column t_subset = new Column();
  Column p_subset = new Column();
  Column stnid_subset = new Column();
  Column order_allow_compress = new Column();
  Column order_max_size_gb = new Column();
  Column order_directory_levels = new Column();
  Column event_subset = new Column();
  Column hide = new Column();
  Column minlat = new Column();
  Column minlon = new Column();
  Column maxlat = new Column();
  Column maxlon = new Column();
  Column order_stnid_list_prog = new Column();
  Column row_revise_contact_id = new Column();
  Column platform_id = new Column();
  Column ds_xlink_id = new Column();
  Column proj_xlink_id = new Column();
  Column date_entered = new Column();
  Column host_name_doc = new Column();
  Column directory_doc = new Column();
  Column begin_date_doc = new Column();
  Column end_date_doc = new Column();
  Column hostname_file = new Column();
  Column format_id_doc = new Column();
  Column archive_date_doc = new Column();
  Column purpose_doc = new Column();
  Column offlineorderable = new Column();
  Column column1 = new Column();

  public DataModule1() {
    try {
      jbInit();
      queryDataset1.open();
      queryDSPlatform.open();
      queryDatasetProj.open();
    }
    catch(Exception e) {
      e.printStackTrace();
    }
  }
  private void jbInit() throws Exception {
    column1.setColumnName("hide");
    column1.setDataType(com.borland.dx.dataset.Variant.BOOLEAN);
    column1.setDefault("");
    column1.setTableName("file");
    column1.setServerColumnName("hide");
    column1.setSqlType(-7);

//-----------------------------------------------------------------------
//  dataset columns with properties specially set
//-----------------------------------------------------------------------

//-------------------------
//  dataset1 columns
//-------------------------

    begindate.setColumnName("begin_date");
    begindate.setDataType(com.borland.dx.dataset.Variant.TIMESTAMP);
    begindate.setDisplayMask("yyyy-MM-dd HH:mm:ss");
    begindate.setEditMask("yyyy-MM-dd HH:mm");
    begindate.setDefault("0001-01-01 01:01:01");
    begindate.setTableName("dataset");
    begindate.setServerColumnName("begin_date");
    begindate.setSqlType(93);

    enddate.setColumnName("end_date");
    enddate.setDataType(com.borland.dx.dataset.Variant.TIMESTAMP);
    enddate.setDisplayMask("yyyy-MM-dd HH:mm:ss");
    enddate.setEditMask("yyyy-MM-dd HH:mm:ss");
    enddate.setDefault("9999-12-31 23:59:59");
    enddate.setTableName("dataset");
    enddate.setServerColumnName("end_date");
    enddate.setSqlType(93);

    minlat.setColumnName("minlat");
    minlat.setDataType(com.borland.dx.dataset.Variant.BIGDECIMAL);
    minlat.setDefault("-90");
    minlat.setPrecision(7);
    minlat.setScale(5);
    minlat.setTableName("dataset");
    minlat.setServerColumnName("minlat");
    minlat.setSqlType(3);

    minlon.setColumnName("minlon");
    minlon.setDataType(com.borland.dx.dataset.Variant.BIGDECIMAL);
    minlon.setDefault("-180");
    minlon.setPrecision(8);
    minlon.setScale(5);
    minlon.setTableName("dataset");
    minlon.setServerColumnName("minlon");
    minlon.setSqlType(3);

    maxlat.setColumnName("maxlat");
    maxlat.setDataType(com.borland.dx.dataset.Variant.BIGDECIMAL);
    maxlat.setDefault("90");
    maxlat.setPrecision(7);
    maxlat.setScale(5);
    maxlat.setTableName("dataset");
    maxlat.setServerColumnName("maxlat");
    maxlat.setSqlType(3);

    maxlon.setColumnName("maxlon");
    maxlon.setDataType(com.borland.dx.dataset.Variant.BIGDECIMAL);
    maxlon.setDefault("180");
    maxlon.setPrecision(8);
    maxlon.setScale(5);
    maxlon.setTableName("dataset");
    maxlon.setServerColumnName("maxlon");
    maxlon.setSqlType(3);

    freq_id.setColumnName("frequency_id");
    freq_id.setDataType(com.borland.dx.dataset.Variant.SHORT);
    freq_id.setDefault("1");
    freq_id.setTableName("dataset");
    freq_id.setServerColumnName("frequency_id");
    freq_id.setSqlType(5);

    spatialtype.setColumnName("spatial_type");
    spatialtype.setDataType(com.borland.dx.dataset.Variant.STRING);
    spatialtype.setPrecision(8);
    spatialtype.setTableName("dataset");
    spatialtype.setServerColumnName("spatial_type");
    spatialtype.setSqlType(1);

    displayContact.setColumnName("displayed_contact_id");
    displayContact.setDataType(com.borland.dx.dataset.Variant.SHORT);
    displayContact.setDefault("1");
    displayContact.setTableName("dataset");
    displayContact.setServerColumnName("displayed_contact_id");
    displayContact.setSqlType(5);

    srcContact.setColumnName("source_contact_id");
    srcContact.setDataType(com.borland.dx.dataset.Variant.SHORT);
    srcContact.setRowId(false);
    srcContact.setTableName("dataset");
    srcContact.setWidth(6);
    srcContact.setServerColumnName("source_contact_id");
    srcContact.setSqlType(5);

    intContact.setColumnName("internal_contact_id");
    intContact.setDataType(com.borland.dx.dataset.Variant.SHORT);
    intContact.setDefault("1");
    intContact.setTableName("dataset");
    intContact.setServerColumnName("internal_contact_id");
    intContact.setSqlType(5);

    onlineorderable.setColumnName("onlineorderable");
    onlineorderable.setDataType(com.borland.dx.dataset.Variant.BYTE);
    onlineorderable.setDefault("");
    onlineorderable.setTableName("dataset");
    onlineorderable.setServerColumnName("onlineorderable");
    onlineorderable.setSqlType(-6);

    offlineorderable.setColumnName("offlineorderable");
    offlineorderable.setDataType(com.borland.dx.dataset.Variant.BYTE);
    offlineorderable.setTableName("dataset");
    offlineorderable.setServerColumnName("offlineorderable");
    offlineorderable.setSqlType(-7);

    hide.setColumnName("hide");
    hide.setDataType(com.borland.dx.dataset.Variant.BYTE);
    hide.setDefault("1");
    hide.setTableName("dataset");
    hide.setServerColumnName("hide");
    hide.setSqlType(-6);

    rowrevContact.setColumnName("row_revise_contact_id");
    rowrevContact.setDataType(com.borland.dx.dataset.Variant.SHORT);
    rowrevContact.setDefault("1");
    rowrevContact.setTableName("dataset");
    rowrevContact.setServerColumnName("row_revise_contact_id");
    rowrevContact.setSqlType(5);

//-------------------------
//  dataset_proj columns
//-------------------------

    dp_dataset_id.setCaption("dataset ID");
    dp_dataset_id.setColumnName("dataset_id");
    dp_dataset_id.setDataType(com.borland.dx.dataset.Variant.STRING);
    dp_dataset_id.setPreferredOrdinal(0);
    dp_dataset_id.setSqlType(0);

    dp_project_id.setCaption("project_id");
    dp_project_id.setColumnName("project_id");
    dp_project_id.setDataType(com.borland.dx.dataset.Variant.STRING);
    dp_project_id.setPreferredOrdinal(1);
    dp_project_id.setServerColumnName("project_id");
    dp_project_id.setSqlType(0);

//-------------------------
//  project columns
//-------------------------

    projID.setColumnName("project_id");
    projID.setDataType(com.borland.dx.dataset.Variant.STRING);
    projID.setPrecision(15);
    projID.setRowId(true);
    projID.setTableName("project");
    projID.setServerColumnName("project_id");
    projID.setSqlType(12);

    proj_begin.setColumnName("begin_date");
    proj_begin.setDataType(com.borland.dx.dataset.Variant.TIMESTAMP);
    proj_begin.setDisplayMask("yyyy-MM-dd");
    proj_begin.setEditMask("yyyy-MM-dd HH:mm");
    proj_begin.setDefault("0001-01-01 01:01:01");
    proj_begin.setTableName("project");
    proj_begin.setServerColumnName("begin_date");
    proj_begin.setSqlType(93);

    proj_end.setColumnName("end_date");
    proj_end.setDataType(com.borland.dx.dataset.Variant.TIMESTAMP);
    proj_end.setDisplayMask("yyyy-MM-dd");
    proj_end.setEditMask("yyyy-MM-dd HH:mm");
    proj_end.setDefault("9999-12-31 23:59:59");
    proj_end.setTableName("project");
    proj_end.setServerColumnName("end_date");
    proj_end.setSqlType(93);

    int_contact.setColumnName("internal_contact_id");
    int_contact.setDataType(com.borland.dx.dataset.Variant.SHORT);
    int_contact.setDefault("1");
    int_contact.setTableName("project");
    int_contact.setServerColumnName("internal_contact_id");
    int_contact.setSqlType(5);

    parent_project_id.setColumnName("parent_project_id");
    parent_project_id.setDataType(com.borland.dx.dataset.Variant.STRING);
    parent_project_id.setPrecision(15);
    parent_project_id.setTableName("project");
    parent_project_id.setServerColumnName("parent_project_id");
    parent_project_id.setSqlType(12);

//---------------------------------
// codiac dataset options columns
//---------------------------------

    order_allow_compress.setColumnName("order_allow_compress");
    order_allow_compress.setDataType(com.borland.dx.dataset.Variant.BYTE);
    order_allow_compress.setDefault("1");
    order_allow_compress.setTableName("codiac_dataset_options");
    order_allow_compress.setServerColumnName("order_allow_compress");
    order_allow_compress.setSqlType(-6);

    t_subset.setColumnName("t_subset");
    t_subset.setDataType(com.borland.dx.dataset.Variant.BYTE);
    t_subset.setDefault("1");
    t_subset.setTableName("codiac_dataset_options");
    t_subset.setServerColumnName("t_subset");
    t_subset.setSqlType(-6);

    event_subset.setColumnName("event_subset");
    event_subset.setDataType(com.borland.dx.dataset.Variant.BYTE);
//    event_subset.setDataType(com.borland.dx.dataset.Variant.BOOLEAN);
    event_subset.setDefault("0");
    event_subset.setTableName("codiac_dataset_options");
    event_subset.setServerColumnName("event_subset");
    event_subset.setSqlType(-6);
//    event_subset.setSqlType(-7);

    order_directory_levels.setColumnName("order_directory_levels");
    order_directory_levels.setDataType(com.borland.dx.dataset.Variant.SHORT);
    order_directory_levels.setDefault("0");
    order_directory_levels.setTableName("codiac_dataset_options");
    order_directory_levels.setServerColumnName("order_directory_levels");
    order_directory_levels.setSqlType(5);

    order_max_size_gb.setColumnName("order_max_size_gb");
    order_max_size_gb.setDataType(com.borland.dx.dataset.Variant.SHORT);
    order_max_size_gb.setDefault(null);
    order_max_size_gb.setTableName("codiac_dataset_options");
    order_max_size_gb.setServerColumnName("order_max_size_gb");
    order_max_size_gb.setSqlType(5);

//-------------------------
// readme columns (these duplicate the file columns, but with different names)
//-------------------------

    end_date_doc.setColumnName("end_date");
    end_date_doc.setDataType(com.borland.dx.dataset.Variant.TIMESTAMP);
    end_date_doc.setDisplayMask("yyyy-MM-dd HH:mm");
    end_date_doc.setEditMask("yyyy-MM-dd HH:mm:ss");
    end_date_doc.setDefault("9999-12-31 23:59:59");
    end_date_doc.setTableName("file");
    end_date_doc.setServerColumnName("end_date");
    end_date_doc.setSqlType(93);

    begin_date_doc.setColumnName("begin_date");
    begin_date_doc.setDataType(com.borland.dx.dataset.Variant.TIMESTAMP);
    begin_date_doc.setDisplayMask("yyyy-MM-dd HH:mm");
    begin_date_doc.setEditMask("yyyy-MM-dd HH:mm:ss");
    begin_date_doc.setDefault("0001-01-01 01:01:01");
    begin_date_doc.setTableName("file");
    begin_date_doc.setServerColumnName("begin_date");
    begin_date_doc.setSqlType(93);

    directory_doc.setColumnName("directory");
    directory_doc.setDataType(com.borland.dx.dataset.Variant.STRING);
    directory_doc.setDefault("/net/archive/data/");
    directory_doc.setPrecision(255);
    directory_doc.setTableName("file");
    directory_doc.setServerColumnName("directory");
    directory_doc.setSqlType(12);

    host_name_doc.setColumnName("host");
    host_name_doc.setDataType(com.borland.dx.dataset.Variant.STRING);
    host_name_doc.setDefault("localhost");
    host_name_doc.setPrecision(10);
    host_name_doc.setTableName("file");
    host_name_doc.setServerColumnName("host");
    host_name_doc.setSqlType(1);

    format_id_doc.setColumnName("format_id");
    format_id_doc.setDataType(com.borland.dx.dataset.Variant.SHORT);
    format_id_doc.setTableName("file");
    format_id_doc.setServerColumnName("format_id");
    format_id_doc.setSqlType(5);

    purpose_doc.setColumnName("purpose");
    purpose_doc.setDataType(com.borland.dx.dataset.Variant.STRING);
    purpose_doc.setEditMask("");
    purpose_doc.setDefault("doc");
    purpose_doc.setPrecision(4);
    purpose_doc.setTableName("file");
    purpose_doc.setServerColumnName("purpose");
    purpose_doc.setSqlType(1);

    archive_date_doc.setColumnName("data_archive_date");
    archive_date_doc.setDataType(com.borland.dx.dataset.Variant.TIMESTAMP);
    archive_date_doc.setDisplayMask("yyyy-MM-dd");
    archive_date_doc.setEditMask("yyyy-MM-dd");
    archive_date_doc.setDefault("2006-09-12 12:00:00");
    archive_date_doc.setTableName("file");
    archive_date_doc.setServerColumnName("data_archive_date");
    archive_date_doc.setSqlType(93);

//-------------------------
// file columns
//-------------------------

    dsID.setColumnName("dataset_id");
    dsID.setDataType(com.borland.dx.dataset.Variant.STRING);
    dsID.setPrecision(15);
    dsID.setTableName("file");
    dsID.setServerColumnName("dataset_id");
    dsID.setSqlType(12);

    hostname_file.setColumnName("host");
    hostname_file.setDataType(com.borland.dx.dataset.Variant.STRING);
    hostname_file.setDefault("localhost");
    hostname_file.setPrecision(10);
    hostname_file.setTableName("file");
    hostname_file.setServerColumnName("host");
    hostname_file.setSqlType(1);

    directory_file.setColumnName("directory");
    directory_file.setDataType(com.borland.dx.dataset.Variant.STRING);
    directory_file.setDefault("/net/archive/data/");
    directory_file.setPrecision(255);
    directory_file.setTableName("file");
    directory_file.setServerColumnName("directory");
    directory_file.setSqlType(12);

    filename.setColumnName("filename");
    filename.setDataType(com.borland.dx.dataset.Variant.STRING);
    filename.setPrecision(255);
    filename.setTableName("file");
    filename.setServerColumnName("filename");
    filename.setSqlType(12);

    file_begin.setColumnName("begin_date");
    file_begin.setDataType(com.borland.dx.dataset.Variant.TIMESTAMP);
    file_begin.setDisplayMask("yyyy-MM-dd HH:mm");
    file_begin.setEditMask("yyyy-MM-dd HH:mm:ss");
    file_begin.setDefault("0001-01-01 00:00:00");
    file_begin.setTableName("file");
    file_begin.setServerColumnName("begin_date");
    file_begin.setSqlType(93);

    file_end.setColumnName("end_date");
    file_end.setDataType(com.borland.dx.dataset.Variant.TIMESTAMP);
    file_end.setDisplayMask("yyyy-MM-dd HH:mm");
    file_end.setEditMask("yyyy-MM-dd HH:mm:ss");
    file_end.setDefault("9999-12-31 23:59:59");
    file_end.setTableName("file");
    file_end.setServerColumnName("end_date");
    file_end.setSqlType(93);

    format.setColumnName("format_id");
    format.setDataType(com.borland.dx.dataset.Variant.SHORT);
    format.setTableName("file");
    format.setServerColumnName("format_id");
    format.setSqlType(5);

    purpose_type.setColumnName("purpose");
    purpose_type.setDataType(com.borland.dx.dataset.Variant.STRING);
    purpose_type.setDefault("data");
    purpose_type.setPrecision(4);
    purpose_type.setTableName("file");
    purpose_type.setServerColumnName("purpose");
    purpose_type.setSqlType(1);

    archive_date_file.setColumnName("data_archive_date");
    archive_date_file.setDataType(com.borland.dx.dataset.Variant.TIMESTAMP);
    archive_date_file.setDisplayMask("yyyy-MM-dd");
    archive_date_file.setEditMask("yyyy-MM-dd");
    archive_date_file.setDefault("2009-05-25 12:00:00");
    archive_date_file.setTableName("file");
    archive_date_file.setServerColumnName("data_archive_date");
    archive_date_file.setSqlType(93);

    file_reviseTime.setColumnName("row_revise_time");
    file_reviseTime.setDataType(com.borland.dx.dataset.Variant.TIMESTAMP);
    file_reviseTime.setDisplayMask("yyyy-MM-dd HH:mm:ss");
    file_reviseTime.setTableName("file");
    file_reviseTime.setServerColumnName("row_revise_time");
    file_reviseTime.setSqlType(93);

//---------------------------
// format_conversion columns
//---------------------------

    dsIDfk.setCaption("dataset ID");
    dsIDfk.setColumnName("dataset_id");
    dsIDfk.setDataType(com.borland.dx.dataset.Variant.STRING);
    dsIDfk.setPrecision(15);
    dsIDfk.setRowId(true);
    dsIDfk.setTableName("format_conversions");
    dsIDfk.setServerColumnName("dataset_id");
    dsIDfk.setSqlType(12);

    source_format.setCaption("source format");
    source_format.setColumnName("source_format_id");
    source_format.setDataType(com.borland.dx.dataset.Variant.SHORT);
    source_format.setRowId(true);
    source_format.setTableName("format_conversions");
    source_format.setVisible(com.borland.jb.util.TriStateProperty.TRUE);
    source_format.setServerColumnName("source_format_id");
    source_format.setSqlType(5);

    target_format.setCaption("target format");
    target_format.setColumnName("target_format_id");
    target_format.setDataType(com.borland.dx.dataset.Variant.SHORT);
    target_format.setRowId(true);
    target_format.setTableName("format_conversions");
    target_format.setVisible(com.borland.jb.util.TriStateProperty.TRUE);
    target_format.setServerColumnName("target_format_id");
    target_format.setSqlType(5);

//------------------------------------
//  codiac dataset plots columns
//------------------------------------

    plotType_id.setColumnName("codiac_plot_type_id");
    plotType_id.setDataType(com.borland.dx.dataset.Variant.BYTE);
    plotType_id.setRowId(true);
    plotType_id.setTableName("codiac_dataset_plots");
    plotType_id.setServerColumnName("codiac_plot_type_id");
    plotType_id.setSqlType(-6);

    time_select.setBackground(Color.white);
    time_select.setColumnName("time_sel_level");
    time_select.setDataType(com.borland.dx.dataset.Variant.STRING);
    time_select.setPrecision(2);
    time_select.setTableName("codiac_dataset_plots");
    time_select.setWidth(12);
    time_select.setServerColumnName("time_sel_level");
    time_select.setSqlType(1);

    stn_select.setBackground(Color.white);
    stn_select.setColumnName("stn_select");
    stn_select.setDataType(com.borland.dx.dataset.Variant.STRING);
    stn_select.setPrecision(12);
    stn_select.setTableName("codiac_dataset_plots");
    stn_select.setServerColumnName("stn_select");
    stn_select.setSqlType(1);

    plots_reviseTime.setColumnName("row_revise_time");
    plots_reviseTime.setDataType(com.borland.dx.dataset.Variant.TIMESTAMP);
    plots_reviseTime.setDisplayMask("yyyy-MM-dd HH:mm:ss");
    plots_reviseTime.setTableName("codiac_dataset_plots");
    plots_reviseTime.setServerColumnName("row_revise_time");
    plots_reviseTime.setSqlType(93);

//---------------------------
//  pi_contact columns
//---------------------------

    pi_contact_id.setColumnName("pi_contact_id");
    pi_contact_id.setDataType(com.borland.dx.dataset.Variant.SHORT);
    pi_contact_id.setRowId(true);
    pi_contact_id.setTableName("dataset_pi");
    pi_contact_id.setServerColumnName("pi_contact_id");
    pi_contact_id.setSqlType(5);

//---------------------------
//  tape columns
//---------------------------

    tapeID.setCaption("tape ID");
    tapeID.setColumnName("tape_id");
    tapeID.setDataType(com.borland.dx.dataset.Variant.INT);
    tapeID.setRowId(true);
    tapeID.setTableName("tape");
    tapeID.setServerColumnName("tape_id");
    tapeID.setSqlType(4);

    tape_dsID.setCaption("dataset ID");
    tape_dsID.setColumnName("dataset_id");
    tape_dsID.setDataType(com.borland.dx.dataset.Variant.STRING);
    tape_dsID.setPrecision(15);
    tape_dsID.setTableName("tape");
    tape_dsID.setServerColumnName("dataset_id");
    tape_dsID.setSqlType(12);

    tape_beginDate.setColumnName("begin_date");
    tape_beginDate.setDataType(com.borland.dx.dataset.Variant.TIMESTAMP);
    tape_beginDate.setDisplayMask("yyyy-MM-dd HH:mm");
    tape_beginDate.setEditMask("yyyy-MM-dd HH:mm");
    tape_beginDate.setTableName("tape");
    tape_beginDate.setServerColumnName("begin_date");
    tape_beginDate.setSqlType(93);

    tape_endDate.setColumnName("end_date");
    tape_endDate.setDataType(com.borland.dx.dataset.Variant.TIMESTAMP);
    tape_endDate.setDisplayMask("yyyy-MM-dd HH:mm");
    tape_endDate.setEditMask("yyyy-MM-dd HH:mm");
    tape_endDate.setTableName("tape");
    tape_endDate.setServerColumnName("end_date");
    tape_endDate.setSqlType(93);

    formatID.setCaption("format name");
    formatID.setColumnName("format_id");
    formatID.setDataType(com.borland.dx.dataset.Variant.SHORT);
    formatID.setTableName("tape");
    formatID.setServerColumnName("format_id");
    formatID.setSqlType(5);

    size_kb.setColumnName("size_kb");
    size_kb.setDataType(com.borland.dx.dataset.Variant.INT);
    size_kb.setDisplayMask("###,###,###");
    size_kb.setTableName("tape");
    size_kb.setServerColumnName("size_kb");
    size_kb.setSqlType(4);

    primary_med_id.setColumnName("primary_medium_id");
    primary_med_id.setDataType(com.borland.dx.dataset.Variant.BYTE);
    primary_med_id.setTableName("tape");
    primary_med_id.setServerColumnName("primary_medium_id");
    primary_med_id.setSqlType(-6);

    backup_med_id.setColumnName("backup_medium_id");
    backup_med_id.setDataType(com.borland.dx.dataset.Variant.BYTE);
    backup_med_id.setTableName("tape");
    backup_med_id.setServerColumnName("backup_medium_id");
    backup_med_id.setSqlType(-6);

    tape_archiveDate.setColumnName("data_archive_date");
    tape_archiveDate.setDataType(com.borland.dx.dataset.Variant.TIMESTAMP);
    tape_archiveDate.setDisplayMask("yyyy-MM-dd");
    tape_archiveDate.setEditMask("yyyy-MM-dd");
    tape_archiveDate.setDefault("NOW");
    tape_archiveDate.setTableName("tape");
    tape_archiveDate.setServerColumnName("data_archive_date");
    tape_archiveDate.setSqlType(93);

//---------------------------
//  DScategory columns
//---------------------------

    categoryID.setColumnName("category_id");
    categoryID.setDataType(com.borland.dx.dataset.Variant.SHORT);
    categoryID.setSortPrecision(-1);
    categoryID.setRowId(true);
    categoryID.setTableName("dataset_category");
    categoryID.setServerColumnName("category_id");
    categoryID.setSqlType(5);

//---------------------------
//  DSplatform columns
//---------------------------

    platformID.setColumnName("platform_id");
    platformID.setDataType(com.borland.dx.dataset.Variant.SHORT);
    platformID.setRowId(true);
    platformID.setTableName("dataset_platform");
    platformID.setServerColumnName("platform_id");
    platformID.setSqlType(5);

//---------------------------
//  dataset user columns
//---------------------------

    user_name.setColumnName("username");
    user_name.setDataType(com.borland.dx.dataset.Variant.STRING);
    user_name.setPrecision(15);
    user_name.setRowId(true);
    user_name.setTableName("dataset_user");
    user_name.setServerColumnName("username");
    user_name.setSqlType(12);

//---------------------------
//   category columns
//---------------------------

    parent_category_id.setColumnName("parent_category_id");
    parent_category_id.setDataType(com.borland.dx.dataset.Variant.SHORT);
    parent_category_id.setTableName("category");
    parent_category_id.setServerColumnName("parent_category_id");
    parent_category_id.setSqlType(5);

//---------------------------
//   dataset xlinks columns
//---------------------------

    ds_xlink_id.setColumnName("xlink_id");
    ds_xlink_id.setDataType(com.borland.dx.dataset.Variant.SHORT);
    ds_xlink_id.setRowId(true);
    ds_xlink_id.setTableName("dataset_xlinks");
    ds_xlink_id.setServerColumnName("xlink_id");
    ds_xlink_id.setSqlType(5);

//---------------------------
//  project xlink columns
//---------------------------

    proj_xlink_id.setColumnName("xlink_id");
    proj_xlink_id.setDataType(com.borland.dx.dataset.Variant.SHORT);
    proj_xlink_id.setRowId(true);
    proj_xlink_id.setTableName("project_xlinks");
    proj_xlink_id.setServerColumnName("xlink_id");
    proj_xlink_id.setSqlType(5);

//-----------------------------------------------------------------------
//  parameterRow columns with properties specially set
//-----------------------------------------------------------------------

//---------------------------
//  paramRowSearchID columns
//---------------------------

    search_id.setCaption("search_id");
    search_id.setColumnName("search_id");
    search_id.setDataType(com.borland.dx.dataset.Variant.STRING);
    search_id.setPreferredOrdinal(0);
    search_id.setServerColumnName("NewColumn1");
    search_id.setSqlType(0);

//---------------------------
//  paramRowFile columns
//---------------------------

    ds_id.setCaption("ds_id");
    ds_id.setColumnName("ds_id");
    ds_id.setDataType(com.borland.dx.dataset.Variant.STRING);
    ds_id.setPreferredOrdinal(2);
    ds_id.setServerColumnName("NewColumn1");
    ds_id.setSqlType(0);

    purpose.setCaption("purpose");
    purpose.setColumnName("purpose");
    purpose.setDataType(com.borland.dx.dataset.Variant.STRING);
    purpose.setPreferredOrdinal(1);
    purpose.setServerColumnName("NewColumn2");
    purpose.setSqlType(0);

    host.setCaption("host");
    host.setColumnName("host");
    host.setDataType(com.borland.dx.dataset.Variant.STRING);
    host.setPreferredOrdinal(0);
    host.setServerColumnName("NewColumn1");
    host.setSqlType(0);

//---------------------------
//  paramRowUser columns
//---------------------------

    username.setCaption("username");
    username.setColumnName("username");
    username.setDataType(com.borland.dx.dataset.Variant.STRING);
    username.setPreferredOrdinal(0);
    username.setServerColumnName("NewColumn1");
    username.setSqlType(0);

    contact_type.setCaption("contact_type");
    contact_type.setColumnName("contact_type");
    contact_type.setDataType(com.borland.dx.dataset.Variant.STRING);
    contact_type.setPreferredOrdinal(1);
    contact_type.setServerColumnName("primary_name");
    contact_type.setSqlType(0);

//---------------------------
//  paramRowID columns
//---------------------------

    dataset_id.setCalcType(com.borland.dx.dataset.CalcType.NO_CALC);
    dataset_id.setCaption("dataset_id");
    dataset_id.setColumnName("dataset_id");
    dataset_id.setDataType(com.borland.dx.dataset.Variant.STRING);
    dataset_id.setPreferredOrdinal(0);
    dataset_id.setTableName("");
    dataset_id.setServerColumnName("NewColumn1");
    dataset_id.setSqlType(0);

    spatial_type.setCaption("spatial_type");
    spatial_type.setColumnName("spatial_type");
    spatial_type.setDataType(com.borland.dx.dataset.Variant.STRING);
    spatial_type.setPreferredOrdinal(1);
    spatial_type.setServerColumnName("NewColumn1");
    spatial_type.setSqlType(0);

//---------------------------
//  paramRowFormat column
//---------------------------

    format_id.setCaption("format_id");
    format_id.setColumnName("format_id");
    format_id.setDataType(com.borland.dx.dataset.Variant.STRING);
    format_id.setPreferredOrdinal(0);
    format_id.setServerColumnName("NewColumn1");
    format_id.setSqlType(0);

//---------------------------
//  paramRowProjID columns
//---------------------------

    proj_id.setCaption("proj_id");
    proj_id.setColumnName("proj_id");
    proj_id.setDataType(com.borland.dx.dataset.Variant.STRING);
    proj_id.setDefault("IHOP_2002");
    proj_id.setPreferredOrdinal(0);
    proj_id.setServerColumnName("NewColumn1");
    proj_id.setSqlType(0);

//--------------------------------------------------------------------------------------------
// set columns
//--------------------------------------------------------------------------------------------
    paramRowProjID.setColumns(new Column[] {proj_id});
    paramRowFormat.setColumns(new Column[] {format_id});
    paramRowID.setColumns(new Column[] {dataset_id, spatial_type});
    paramRowUser.setColumns(new Column[] {username, contact_type});
    paramRowFile.setColumns(new Column[] {purpose, host, ds_id});
    paramRowSearchID.setColumns(new Column[] {search_id});

    queryDataset1.setColumns(new Column[] {begindate, enddate, minlat, minlon, maxlat, maxlon, freq_id, spatialtype, displayContact, srcContact, intContact, onlineorderable, offlineorderable, hide, rowrevContact});
    queryDatasetProj.setColumns(new Column[] {dp_dataset_id, dp_project_id});
    queryFile.addLoadListener(new DataModule1_queryFile_loadAdapter(this));
    queryPlots.setColumns(new Column[] {plotType_id, time_select, stn_select, plots_reviseTime});
    queryProject.setColumns(new Column[] {projID, proj_begin, proj_end, int_contact, parent_project_id});
    queryDatasetPI.setColumns(new Column[] {pi_contact_id});
    queryFormatConv.setColumns(new Column[] {dsIDfk, source_format, target_format});
    queryDSCategory.setColumns(new Column[] {categoryID});
    queryDSPlatform.setColumns(new Column[] {platformID});
    queryDSUser.setColumns(new Column[] {user_name});
    queryCatDS.setColumns(new Column[] {parent_category_id});
    queryTape.setColumns(new Column[] {tapeID, tape_dsID, tape_beginDate, tape_endDate, formatID, size_kb, primary_med_id, backup_med_id, tape_archiveDate});
    queryDatasetXlink.setColumns(new Column[] {ds_xlink_id});
    queryProjXlink.setColumns(new Column[] {proj_xlink_id});
    queryReadme.setColumns(new Column[] {host_name_doc, directory_doc, begin_date_doc, end_date_doc, format_id_doc, purpose_doc, archive_date_doc});
    queryDatasetOptions.setColumns(new Column[] {t_subset, order_allow_compress, event_subset, order_max_size_gb, order_directory_levels});


// CODIAC DataSetView
    contactDataSetView.setStorageDataSet(queryContact);
    contactDataSetView.setSort(new com.borland.dx.dataset.SortDescriptor("", new String[] {"contact_short_name"}, new boolean[] {false, }, null, Sort.CASEINSENSITIVE));

// CODIAC PickLists
    displayContact.setPickList(new com.borland.dx.dataset.PickListDescriptor(contactDataSetView, new String[] {"CONTACT_ID"}, new String[] {"CONTACT_SHORT_NAME", "PERSON_NAME"}, new String[] {"DISPLAYED_CONTACT_ID"}, "CONTACT_SHORT_NAME", false));
    int_contact.setPickList(new com.borland.dx.dataset.PickListDescriptor(queryInternalContact, new String[] {"CONTACT_ID"}, new String[] {"PERSON_NAME"}, new String[] {"INTERNAL_CONTACT_ID"}, "PERSON_NAME", false));
    intContact.setPickList(new com.borland.dx.dataset.PickListDescriptor(queryInternalContact, new String[] {"CONTACT_ID"}, new String[] {"PERSON_NAME"}, new String[] {"INTERNAL_CONTACT_ID"}, "PERSON_NAME", false));
    srcContact.setPickList(new com.borland.dx.dataset.PickListDescriptor(contactDataSetView, new String[] {"CONTACT_ID"}, new String[] {"CONTACT_SHORT_NAME", "PERSON_NAME"}, new String[] {"SOURCE_CONTACT_ID"}, "CONTACT_SHORT_NAME", false));
    pi_contact_id.setPickList(new com.borland.dx.dataset.PickListDescriptor(queryContact, new String[] {"CONTACT_ID"}, new String[] {"CONTACT_SHORT_NAME", "PERSON_NAME"}, new String[] {"PI_CONTACT_ID"}, "PERSON_NAME", false));
    rowrevContact.setPickList(new com.borland.dx.dataset.PickListDescriptor(queryRevContact, new String[] {"CONTACT_ID"}, new String[] {"PERSON_NAME"}, new String[] {"ROW_REVISE_CONTACT_ID"}, "PERSON_NAME", false));
    freq_id.setPickList(new com.borland.dx.dataset.PickListDescriptor(queryFrequency, new String[] {"FREQUENCY_ID"}, new String[] {"NAME"}, new String[] {"FREQUENCY_ID"}, "NAME", false));
    target_format.setPickList(new com.borland.dx.dataset.PickListDescriptor(queryFormat, new String[] {"FORMAT_ID"}, new String[] {"FORMAT_ID", "FULL_NAME", "THREDDS_NAME"}, new String[] {"TARGET_FORMAT_ID"}, "FULL_NAME", false));
    source_format.setPickList(new com.borland.dx.dataset.PickListDescriptor(queryFormat, new String[] {"FORMAT_ID"}, new String[] {"FORMAT_ID", "FULL_NAME", "THREDDS_NAME"}, new String[] {"SOURCE_FORMAT_ID"}, "FULL_NAME", false));
    format.setPickList(new com.borland.dx.dataset.PickListDescriptor(queryFormat, new String[] {"FORMAT_ID"}, new String[] {"FORMAT_ID", "SHORT_NAME", "FULL_NAME"}, new String[] {"FORMAT_ID"}, "FULL_NAME", false));
    formatID.setPickList(new com.borland.dx.dataset.PickListDescriptor(queryFormat, new String[] {"FORMAT_ID"}, new String[] {"FULL_NAME"}, new String[] {"FORMAT_ID"}, "FULL_NAME", false));
    format_id_doc.setPickList(new com.borland.dx.dataset.PickListDescriptor(queryFormat, new String[] {"FORMAT_ID"}, new String[] {"FORMAT_ID", "SHORT_NAME", "FULL_NAME"}, new String[] {"FORMAT_ID"}, "FULL_NAME", false));
    formatIDfk.setPickList(new com.borland.dx.dataset.PickListDescriptor(queryFormat, new String[] {"FORMAT_ID"}, new String[] {"FORMAT_ID", "FULL_NAME"}, new String[] {"FORMAT_ID"}, "FULL_NAME", false));
    project_id.setPickList(new com.borland.dx.dataset.PickListDescriptor(queryProjectID, new String[] {"PROJECT_ID"}, new String[] {"PROJECT_ID"}, new String[] {"PROJECT_ID"}, null, false));
    parent_project_id.setPickList(new com.borland.dx.dataset.PickListDescriptor(queryProjectID, new String[] {"PROJECT_ID"}, new String[] {"PROJECT_ID", "PARENT_PROJECT_ID"}, new String[] {"PARENT_PROJECT_ID"}, null, false));
    projID.setPickList(new com.borland.dx.dataset.PickListDescriptor(queryProjName, new String[] {"PROJECT_ID"}, new String[] {"PROJECT_ID"}, new String[] {"PROJECT_ID"}, "PROJECT_ID", false));
    plotType_id.setPickList(new com.borland.dx.dataset.PickListDescriptor(queryPlotIDs, new String[] {"CODIAC_PLOT_TYPE_ID"}, new String[] {"NAME", "CODIAC_PLOT_TYPE_ID"}, new String[] {"CODIAC_PLOT_TYPE_ID"}, "NAME", false));
    spatialtype.setPickList(new com.borland.dx.dataset.PickListDescriptor(querySpatial, new String[] {"SPATIAL_TYPE"}, new String[] {"SPATIAL_TYPE"}, new String[] {"SPATIAL_TYPE"}, "SPATIAL_TYPE", false));
    user_name.setPickList(new com.borland.dx.dataset.PickListDescriptor(queryPswdUser, new String[] {"USERNAME"}, new String[] {"USERNAME"}, new String[] {"USERNAME"}, "USERNAME", false));
    platformID.setPickList(new com.borland.dx.dataset.PickListDescriptor(queryPlatform, new String[] {"PLATFORM_ID"}, new String[] {"PLATFORM_ID", "NAME"}, new String[] {"PLATFORM_ID"}, "NAME", false));
    categoryID.setPickList(new com.borland.dx.dataset.PickListDescriptor(queryCategory, new String[] {"CATEGORY_ID"}, new String[] {"CATEGORY_ID", "NAME"}, new String[] {"CATEGORY_ID"}, "NAME", false));
    backup_med_id.setPickList(new com.borland.dx.dataset.PickListDescriptor(queryMedium, new String[] {"MEDIUM_ID"}, new String[] {"MEDIUM_NAME"}, new String[] {"BACKUP_MEDIUM_ID"}, "MEDIUM_NAME", false));
    primary_med_id.setPickList(new com.borland.dx.dataset.PickListDescriptor(queryMedium, new String[] {"MEDIUM_ID"}, new String[] {"MEDIUM_NAME"}, new String[] {"PRIMARY_MEDIUM_ID"}, "MEDIUM_NAME", false));
    parent_category_id.setPickList(new com.borland.dx.dataset.PickListDescriptor(queryCategory, new String[] {"CATEGORY_ID"}, new String[] {"NAME"}, new String[] {"PARENT_CATEGORY_ID"}, "NAME", false));
    dp_project_id.setPickList(new com.borland.dx.dataset.PickListDescriptor(queryProjName, new String[] {"PROJECT_ID"}, new String[] {"PROJECT_ID"}, new String[] {"PROJECT_ID"}, "PROJECT_ID", false));
    ds_xlink_id.setPickList(new com.borland.dx.dataset.PickListDescriptor(queryXlinkName, new String[] {"XLINK_ID"}, new String[] {"HREF", "TITLE", "PURPOSE", "XLINK_ID"}, new String[] {"XLINK_ID"}, "HREF", false));
    proj_xlink_id.setPickList(new com.borland.dx.dataset.PickListDescriptor(queryXlinkName, new String[] {"XLINK_ID"}, new String[] {"HREF", "TITLE", "PURPOSE", "XLINK_ID"}, new String[] {"XLINK_ID"}, "HREF", false));

 // CODIAC Queries
    queryDataset1.setQuery(new com.borland.dx.sql.dataset.QueryDescriptor(db_codiac, "SELECT * FROM dataset WHERE dataset_id LIKE :search_id", paramRowSearchID, true, Load.ALL));
    queryReadme.setQuery(new com.borland.dx.sql.dataset.QueryDescriptor(db_codiac, "SELECT * FROM file WHERE dataset_id = :dataset_id AND (purpose = \'doc\' OR purpose = \'eula\')", paramRowID, true, Load.ALL));
    queryFrequency.setQuery(new com.borland.dx.sql.dataset.QueryDescriptor(db_codiac, "SELECT * FROM frequency", null, true, Load.ALL));
    queryDatasetOptions.setQuery(new com.borland.dx.sql.dataset.QueryDescriptor(db_codiac, "SELECT * FROM codiac_dataset_options WHERE dataset_id = :dataset_id", queryDataset1, true, Load.ALL));
    queryFormatConv.setQuery(new com.borland.dx.sql.dataset.QueryDescriptor(db_codiac, "SELECT * FROM format_conversions WHERE dataset_id = :dataset_id", queryDataset1, true, Load.ALL));
    queryPlots.setQuery(new com.borland.dx.sql.dataset.QueryDescriptor(db_codiac, "SELECT * FROM codiac_dataset_plots WHERE dataset_id = :dataset_id", queryDataset1, true, Load.ALL));
    queryFormat.setSort(new com.borland.dx.dataset.SortDescriptor("", new String[] {"full_name"}, new boolean[] {false, }, null, Sort.CASEINSENSITIVE));
    queryFormat.setQuery(new com.borland.dx.sql.dataset.QueryDescriptor(db_codiac, "SELECT * FROM format", null, true, Load.ALL));
    queryContact.setQuery(new com.borland.dx.sql.dataset.QueryDescriptor(db_codiac, "SELECT * FROM contact", null, true, Load.ALL));
    queryRevContact.setQuery(new com.borland.dx.sql.dataset.QueryDescriptor(db_codiac, "SELECT contact_id, person_name FROM contact WHERE active_editor = 1 AND person_name REGEXP \'.+\' ", null, true, Load.ALL));
    queryInternalContact.setSort(new com.borland.dx.dataset.SortDescriptor("", new String[] {"person_name"}, new boolean[] {false, }, null, 0));
    queryInternalContact.setQuery(new com.borland.dx.sql.dataset.QueryDescriptor(db_codiac, "SELECT contact_id, person_name FROM contact WHERE active_editor = 1 AND person_name REGEXP \'.+\' ", null, true, Load.ALL));
    queryUser.setQuery(new com.borland.dx.sql.dataset.QueryDescriptor(db_codiac, "SELECT * FROM contact WHERE contact_short_name = :username", paramRowUser, true, Load.ALL));
    queryDatasetUser.setQuery(new com.borland.dx.sql.dataset.QueryDescriptor(db_codiac, "SELECT * FROM dataset_user WHERE dataset_id = :dataset_id", paramRowID, true, Load.ALL));
    queryDatasetPI.setQuery(new com.borland.dx.sql.dataset.QueryDescriptor(db_codiac, "SELECT * FROM dataset_pi WHERE dataset_id = :dataset_id", paramRowID, true, Load.ALL));
    queryDatasetProj.setQuery(new com.borland.dx.sql.dataset.QueryDescriptor(db_codiac, "SELECT * FROM dataset_project WHERE dataset_id = :dataset_id", queryDataset1, true, Load.ALL));
    queryProjDataset.setSort(new com.borland.dx.dataset.SortDescriptor("", new String[] {"dataset_id"}, new boolean[] {true, }, null, Sort.CASEINSENSITIVE));
    queryProjDataset.setQuery(new com.borland.dx.sql.dataset.QueryDescriptor(db_codiac, "SELECT * FROM dataset_project WHERE project_id = :proj_id", paramRowProjID, true, Load.ALL));
    queryProjectID.setQuery(new com.borland.dx.sql.dataset.QueryDescriptor(db_codiac, "SELECT project_id, parent_project_id  FROM project", null, true, Load.ALL));
    queryProject.setQuery(new com.borland.dx.sql.dataset.QueryDescriptor(db_codiac, "SELECT * FROM project WHERE project_id = :proj_id", paramRowProjID, true, Load.ALL));
    queryFile.setQuery(new com.borland.dx.sql.dataset.QueryDescriptor(db_codiac, "SELECT * FROM file WHERE dataset_id = :ds_id AND purpose = \'data\'", paramRowFile, true, Load.AS_NEEDED));
    querySpatial.setQuery(new com.borland.dx.sql.dataset.QueryDescriptor(db_codiac, "SELECT DISTINCT spatial_type FROM dataset", null, true, Load.ALL));
    queryPlotIDs.setQuery(new com.borland.dx.sql.dataset.QueryDescriptor(db_codiac, "SELECT DISTINCT name, codiac_plot_type_id FROM codiac_plot_type", null, true, Load.ALL));
    queryPlotIDs.setSort(new com.borland.dx.dataset.SortDescriptor("", new String[] {"codiac_plot_type_id"}, new boolean[] {false, }, null, 0));
    queryProjName.setQuery(new com.borland.dx.sql.dataset.QueryDescriptor(db_codiac, "SELECT DISTINCT project_id FROM project order by project_id", null, true, Load.ALL));
    queryPswdUser.setQuery(new com.borland.dx.sql.dataset.QueryDescriptor(db_codiac, "SELECT username FROM data_passwd", null, true, Load.ALL));
    queryPassword.setQuery(new com.borland.dx.sql.dataset.QueryDescriptor(db_codiac, "SELECT * FROM data_passwd WHERE username = :username", queryDSUser, true, Load.ALL));
    queryDSUser.setQuery(new com.borland.dx.sql.dataset.QueryDescriptor(db_codiac, "SELECT * FROM dataset_user WHERE dataset_id = :dataset_id", queryDataset1, true, Load.ALL));
    queryDSPlatform.setQuery(new com.borland.dx.sql.dataset.QueryDescriptor(db_codiac, "SELECT * FROM dataset_platform WHERE dataset_id = :dataset_id", paramRowID, true, Load.ALL));
    queryDSCategory.setQuery(new com.borland.dx.sql.dataset.QueryDescriptor(db_codiac, "SELECT * FROM dataset_category WHERE dataset_id = :dataset_id", paramRowID, true, Load.ALL));
    queryPlatform.setQuery(new com.borland.dx.sql.dataset.QueryDescriptor(db_codiac, "SELECT * FROM platform", null, true, Load.ALL));
    queryPlatform.setSort(new com.borland.dx.dataset.SortDescriptor("", new String[] {"name"}, new boolean[] {false, }, null, Sort.CASEINSENSITIVE));
    queryCategory.setQuery(new com.borland.dx.sql.dataset.QueryDescriptor(db_codiac, "SELECT category_id, name FROM category", null, true, Load.ALL));
    queryMedium.setQuery(new com.borland.dx.sql.dataset.QueryDescriptor(db_codiac, "SELECT * FROM medium", null, true, Load.ALL));
    queryMedium.setSort(new com.borland.dx.dataset.SortDescriptor("", new String[] {"medium_name"}, new boolean[] {false, }, null, Sort.CASEINSENSITIVE));
    queryTape.setQuery(new com.borland.dx.sql.dataset.QueryDescriptor(db_codiac, "SELECT * FROM tape WHERE dataset_id = :dataset_id", queryDataset1, true, Load.ALL));
    queryCatDS.setQuery(new com.borland.dx.sql.dataset.QueryDescriptor(db_codiac, "SELECT * FROM category", null, true, Load.ALL));
    queryThisPrefix.setQuery(new com.borland.dx.sql.dataset.QueryDescriptor(db_codiac, "SELECT dataset_id_prefix from dataset_prefix_project where project_id = :project_id", queryProject, true, Load.ALL));
    queryProjPrefix.setSort(new com.borland.dx.dataset.SortDescriptor("", new String[] {"dataset_id_prefix"}, new boolean[] {false, }, null, Sort.SORT_AS_INSERTED));
    queryProjPrefix.setQuery(new com.borland.dx.sql.dataset.QueryDescriptor(db_codiac, "SELECT * from dataset_prefix_project", null, true, Load.ALL));
    queryDSReferences.setQuery(new com.borland.dx.sql.dataset.QueryDescriptor(db_codiac, "SELECT * from dataset_references where dataset_id = :dataset_id", paramRowID, true, Load.ALL));
    queryXlinkName.setQuery(new com.borland.dx.sql.dataset.QueryDescriptor(db_codiac, "SELECT * FROM xlink", null, true, Load.ALL));
    queryXlinkProj.setQuery(new com.borland.dx.sql.dataset.QueryDescriptor(db_codiac, "SELECT * FROM xlink", null, true, Load.ALL));
    queryProjXlink.setQuery(new com.borland.dx.sql.dataset.QueryDescriptor(db_codiac, "SELECT * FROM project_xlinks WHERE project_id=:proj_id", paramRowProjID, true, Load.ALL));
    queryXlink.setQuery(new com.borland.dx.sql.dataset.QueryDescriptor(db_codiac, "SELECT * FROM xlink", null, true, Load.ALL));
    queryDatasetXlink.setQuery(new com.borland.dx.sql.dataset.QueryDescriptor(db_codiac, "SELECT * FROM dataset_xlinks WHERE dataset_id=:dataset_id", queryDataset1, true, Load.ALL));
    use.setColumnName("purpose");

// MasterLinks
    queryXlink.setMasterLink(new com.borland.dx.dataset.MasterLinkDescriptor(queryDatasetXlink, new String[] {"xlink_id"}, new String[] {"xlink_id"}, false, true, true));
    queryXlinkProj.setMasterLink(new com.borland.dx.dataset.MasterLinkDescriptor(queryProjXlink, new String[] {"xlink_id"}, new String[] {"xlink_id"}, false, true, true));

//--------------------------------------------------------------------------------------------
//    connect to zedi on tsunami
//--------------------------------------------------------------------------------------------
//    db_codiac.setConnection(new com.borland.dx.sql.dataset.ConnectionDescriptor("jdbc:mysql://tsunami.eol.ucar.edu:3306/zedi", "guimaint", "gooey-up", false, "com.mysql.jdbc.Driver"));
//--------------------------------------------------------------------------------------------
//    connect to zedi8 on tsunami with new fields
//--------------------------------------------------------------------------------------------
    db_codiac.setConnection(new com.borland.dx.sql.dataset.ConnectionDescriptor("jdbc:mysql://tsunami.eol.ucar.edu:3306/zedi8", "guimaint", "gooey-up", false, "com.mysql.jdbc.Driver"));
//--------------------------------------------------------------------------------------------
//    connect to riesling with database same as zedi on tsunami
//--------------------------------------------------------------------------------------------
//    db_codiac.setConnection(new com.borland.dx.sql.dataset.ConnectionDescriptor("jdbc:mysql://riesling.eol.ucar.edu:3306/zedi7", "guimaint", "gooey-up", false, "com.mysql.jdbc.Driver"));
//--------------------------------------------------------------------------------------------
//    connect to the MySQL server on tsunami with the newly updated db
//--------------------------------------------------------------------------------------------
//    db_codiac.setConnection(new com.borland.dx.sql.dataset.ConnectionDescriptor("jdbc:mysql://riesling.eol.ucar.edu:3306/zedi8", "guimaint", "gooey-up", false, "com.mysql.jdbc.Driver"));
//--------------------------------------------------------------------------------------------

  }

  public static DataModule1 getDataModule() {
    if (myDM == null) {
      myDM = new DataModule1();
    }
    return myDM;
  }
  public com.borland.dx.sql.dataset.Database getDatabase1() {
    return db_codiac;
  }
  public com.borland.dx.sql.dataset.QueryDataSet getQueryContact() {
    return queryContact;
  }
  public com.borland.dx.dataset.ParameterRow getParamRowUser() {
    return paramRowUser;
  }
  public com.borland.dx.dataset.ParameterRow getParamRowID() {
    return paramRowID;
  }

  public com.borland.dx.sql.dataset.QueryDataSet getQueryDataset1() {
    return queryDataset1;
  }
  public com.borland.dx.dataset.ParameterRow getParamRowProjID() {
    return paramRowProjID;
  }
  public com.borland.dx.sql.dataset.QueryDataSet getQueryDatasetProj() {
    return queryDatasetProj;
  }
  public com.borland.dx.sql.dataset.QueryDataSet getQueryProject() {
    return queryProject;
  }
  public com.borland.dx.sql.dataset.QueryDataSet getQueryDatasetOptions() {
    return queryDatasetOptions;
  }
  public com.borland.dx.sql.dataset.QueryDataSet getQueryDatasetPI() {
    return queryDatasetPI;
  }
  public com.borland.dx.sql.dataset.QueryDataSet getQueryDatasetUser() {
    return queryDatasetUser;
  }
  public com.borland.dx.sql.dataset.QueryDataSet getQueryFile() {
    return queryFile;
  }
  public com.borland.dx.sql.dataset.QueryDataSet getQueryFormat() {
    return queryFormat;
  }
  public com.borland.dx.dataset.ParameterRow getParamRowFormat() {
    return paramRowFormat;
  }
  public com.borland.dx.sql.dataset.QueryDataSet getQueryPlots() {
    return queryPlots;
  }
  public com.borland.dx.sql.dataset.QueryDataSet getQueryFormatConv() {
    return queryFormatConv;
  }
  public com.borland.dx.sql.dataset.QueryDataSet getQueryFrequency() {
    return queryFrequency;
  }
  public com.borland.dx.dataset.ParameterRow getParamRowFile() {
    return paramRowFile;
  }
  public com.borland.dx.sql.dataset.QueryDataSet getQueryInternalContact() {
    return queryInternalContact;
  }
  public com.borland.dx.sql.dataset.QueryDataSet getQueryUser() {
    return queryUser;
  }
  public com.borland.dx.dataset.ParameterRow getParamRowSearchID() {
    return paramRowSearchID;
  }
  public com.borland.dx.sql.dataset.QueryDataSet getQueryProjectID() {
    return queryProjectID;
  }

  void queryFile_dataLoaded(LoadEvent e) {
    PhysDirPanel.findFilePanel.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
  }
  public com.borland.dx.sql.dataset.QueryDataSet getQueryReadme() {
    return queryReadme;
  }
  public com.borland.dx.sql.dataset.QueryDataSet getQueryRevContact() {
    return queryRevContact;
  }
  public com.borland.dx.sql.dataset.QueryDataSet getQuerySpatial() {
    return querySpatial;
  }
  public com.borland.dx.sql.dataset.QueryDataSet getQueryProjName() {
    return queryProjName;
  }
  public com.borland.dx.sql.dataset.QueryDataSet getQueryPlotIDs() {
    return queryPlotIDs;
  }
  public com.borland.dx.sql.dataset.QueryDataSet getQueryProjDataset() {
    return queryProjDataset;
  }
  public com.borland.dx.sql.dataset.QueryDataSet getQueryTape() {
    return queryTape;
  }
  public com.borland.dx.sql.dataset.QueryDataSet getQueryMedium() {
    return queryMedium;
  }
  public com.borland.dx.sql.dataset.QueryDataSet getQueryCategory() {
    return queryCategory;
  }
  public com.borland.dx.sql.dataset.QueryDataSet getQueryPlatform() {
    return queryPlatform;
  }
  public com.borland.dx.sql.dataset.QueryDataSet getQueryDSCategory() {
    return queryDSCategory;
  }
  public com.borland.dx.sql.dataset.QueryDataSet getQueryDSPlatform() {
    return queryDSPlatform;
  }
  public com.borland.dx.sql.dataset.QueryDataSet getQueryPassword() {
    return queryPassword;
  }
  public com.borland.dx.sql.dataset.QueryDataSet getQueryDSUser() {
    return queryDSUser;
  }
  public com.borland.dx.sql.dataset.QueryDataSet getQueryPswdUser() {
    return queryPswdUser;
  }
  public com.borland.dx.sql.dataset.QueryDataSet getQueryCatDS() {
    return queryCatDS;
  }
  public com.borland.dx.dataset.DataSetView getContactDataSetView() {
    return contactDataSetView;
  }
  public com.borland.dx.sql.dataset.QueryDataSet getQueryProjPrefix() {
    return queryProjPrefix;
  }
  public com.borland.dx.sql.dataset.QueryDataSet getQueryThisPrefix() {
    return queryThisPrefix;
  }
  public com.borland.dx.sql.dataset.QueryDataSet getQueryDSReferences() {
    return queryDSReferences;
  }
  public com.borland.dx.sql.dataset.QueryDataSet getQueryXlink() {
    return queryXlink;
  }
  public com.borland.dx.sql.dataset.QueryDataSet getQueryDatasetXlink() {
    return queryDatasetXlink;
  }
  public com.borland.dx.sql.dataset.QueryDataSet getQueryProjXlink() {
    return queryProjXlink;
  }
  public com.borland.dx.sql.dataset.QueryDataSet getQueryXlinkProj() {
    return queryXlinkProj;
  }
  public com.borland.dx.sql.dataset.QueryDataSet getQueryXlinkName() {
    return queryXlinkName;
  }

}

class DataModule1_queryFile_loadAdapter implements com.borland.dx.dataset.LoadListener {
  DataModule1 adaptee;

  DataModule1_queryFile_loadAdapter(DataModule1 adaptee) {
    this.adaptee = adaptee;
  }
  public void dataLoaded(LoadEvent e) {
    adaptee.queryFile_dataLoaded(e);
  }
}
