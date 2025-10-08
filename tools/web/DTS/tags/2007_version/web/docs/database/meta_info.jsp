<html>
<head>
    <title>DTS Metadata Information</title>
    <link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/css/docs.css" />
</head>

<h1>DTS Metadata to be Tracked</h1>

<a name="top"></a>
<h2>Contents</h2>
<ul>
    <li><a href="#definitions">Definitions</a></li>
    <li><a href="#project">Project</a></li>
    <li><a href="#dataset">Data Set</a></li>
    <li><a href="#user">User</a></li>
    <li><a href="#software">Software</a></li>
</ul>
<hr />

<a name="definitions"></a>
<h2>Definitions</h2>
<p>Any of the <span class="obsolete">attributes</span> in the DLN &amp; IVEN sections are considered obsolute/unnecessary and will not be carried over to the DTS.  There are a few other attributes from the DLN &amp; IVEN columns that do not map directly to the DTS.  They are still included, but may be through some other mechanism.  (i.e. The raw data attributes of the dataset will be included from the source dataset information.)</p>


<p>There are three primary objects that will be maintained by the DTS:  projects, datasets, and software.  A project is a project as we are accustomed to using it and does not really change.  A dataset is any collection of data and includes source data, imagery, composites, and loose collections of other datasets (this will be explained more in a moment).  Software is any package (code, documentation, and any other file required to perform the software's task) that performs a certain task or collection of tasks (a conversion, tool for checking, etc.)</p>
<p>Therea are also a couple of terms that are used in this document that exist in the new form of the DTS as datasets.  These are products and groups.  Product is from IVEN and is a collection of processed datasets that usually form a composite.  A group is a product that is not a composite.  It is a collection of prcessed datasets that have some relation to each other, but do not form a composite.</p>
<p>In the DTS, both the composite and group product types will be treated as a dataset.  Composites truly are datasets and will require all the same metadata to specify the work done to generate the composite.  A group may not generate a public dataset or define all the same metadata as a typical dataset, but will have the metadata fields available to define any special/common information to the group dataset.</p>

<hr />

<a name="project"></a>
<h2>Project</h2>
<table align="center" border="1" width="90%">
  <tr>
    <th>In DLN & IVEN</th>
    <th>To Be in DTS</th>
  </tr>
  <tr>
    <td valign="top">
      <ul>
        <li>Project ID</li>
        <li>Dataset Prefix</li>
        <li>Active Flag</li>
        <li>Full Name</li>
        <li>Begin Date</li>
        <li>End Date</li>
        <li>Minimum Latitude</li>
        <li>Maximum Latitude</li>
        <li>Minimum Longitude</li>
        <li>Maximum Longitude</li>
        <li>Charge Number</li>
        <li class="obsolete">Link Source</li>
        <li class="obsolete">Link Target</li>
        <li>Project Notes</li>
        <li>Product(s)</li>
        <li>Dataset(s)</li>
      </ul>
    </td>
    <td valign="top">
      <ul>
        <li>Project ID</li>
        <li>Dataset Prefix</li>
        <li>Active Flag</li>
        <li>Full Name</li>
        <li>Begin Date</li>
        <li>End Date</li>
        <li>Minimum Latitude</li>
        <li>Maximum Latitude</li>
        <li>Minimum Longitude</li>
        <li>Maximum Longitude</li>
        <li>Charge Number</li>
        <li>Project Notes</li>
        <li>Dataset(s)</li>
      </ul>
    </td>
  </tr>
</table>
<p>The active flag is used to keep the primary list of projects to a workable size.  All projects will be available, but may require more clicks to get to their primary pages.  The inactive projects would only appear on an all projects list or past projects list and would not be available in some menus/selections.</p>
<p>The link source and target in IVEN are to allow the tool to access the work space (/net/work) that are not web accessible.  Typically, a sym-link was created per project to go to the top level of the work space for the project.  This could be an issue since this would make the entire project area available to the tool instead of just the specific software packages.  Therefore, it is not being carried into the DTS.  The functionality will still be there, but will be up to the individual user to create a sym-link in the correct spot on the dmg web server for each software package that is to web accessible to the DTS.</p>

<p><a href="#top">Top</a></p>
<hr />


<a name="dataset"></a>
<h2>Data Set</h2>
<table align="center" border="1" width="90%">
  <tr>
    <th>In DLN & IVEN</th>
    <th>To be in DTS</th>
  </tr>
  <tr>
    <td valign="top">
      <ul>
        <li>Dataset ID</li>
        <li>Title/Name</li>
        <li>Entry Date</li>
        <li>Ingest/Data to Load Location</li>
        <li>Archive Location</li>
        <li>External Contact</li>
        <li>External Email</li>
        <li>Internal Contact</li>
        <li>Loading Contact</li>
        <li>Checking Contact</li>
        <li>In Master List Flag</li>
        <li>Has Documentation Flag</li>
        <li>Loading Status</li>
        <li>Checking Status</li>
        <li>Remote Link</li>
        <li>Dataset Notes</li>
        <li>Parent Product</li>
        <li>Processing Contact</li>
        <li>Processing Status</li>
        <li>Exclude from Product Flag</li>
        <li>Processing Questions Flag</li>
        <li>Processing Notes</li>
        <li>Raw Data Directory</li>
        <li>Raw Data Documentation</li>
        <li>Final Data Directory</li>
        <li>Station List File Location</li>
        <li>Software Location</li>
        <li>How To File Location</li>
        <li>Data Plot Location</li>
        <li class="obsolete">Id Type</li>
        <li>Platform Type</li>
        <li class="obsolete">Code in Best Software Flag</li>
      </ul>
    </td>
    <td valign="top">
      <ul>
        <li>Dataset ID</li>
        <li>Title/Name</li>
        <li>Entry Date</li>
        <li>Ingest Type</li>
        <li>Ingest Data Location</li>
        <li>Data to Load Location</li>
        <li>Archive Location</li>
        <li>External Contact Name</li>
        <li>External Contact Email</li>
        <li>Internal Contact</li>
        <li>Ingest Contact</li>
        <li>Loading Contact</li>
        <li>Checking Contact</li>
        <li>In Master List Flag</li>
        <li>Has Documentation Flag</li>
        <li>Ingest Status</li>
        <li>Loading Status</li>
        <li>Checking Status</li>
        <li>Remote Link</li>
	<li>General Dataset Notes</li>
        <li>Ingest Notes</li>
        <li>Loading Notes</li>
        <li>Checking Notes</li>
        <li>Parent Dataset(s)</li>
        <li>Processing Contact</li>
        <li>Processing Status</li>
        <li>Exclude from Product Flag</li>
        <li>Processing Questions Flag</li>
        <li>Processing Notes</li>
	<li>Work Directory</li>
        <li>Final Data Directory</li>
        <li>Station List File Location</li>
        <li>Software Package(s) & Tag(s)</li>
        <li>How To File Location</li>
        <li>Data Plot Location</li>
        <li>Platform Type</li>
        <li>Source Dataset(s)</li>
        <li>Source Dataset(s) Documentation</li>
        <li>UTC Offset</li>
        <li>DST Flag</li>
        <li>Collection Time Notes</li>
        <li>Source Information Notes</li>
        <li>Dataset Documetation Link</li>
        <li><b>Multiple Versions</b></li>
      </ul>
    </td>
  </tr>
</table>
<p>The ingest type has been added to define how the dataset's data was given to EOL.  This would include FTP, Email, CD, Manual Downloads, etc.</p>
<p>The Ingest Location and Data to Load Location have been split into two separate fields to allow both the true ingest location and the data to load location to be different.  This is an attempt to prevent the DTS from being abused as the DLN was.</p>
<p>The Notes have been split into types.  This is to allow the notes to be specialized and may only appear in certain parts of the tools.</p>
<p>The raw data for a processed dataset has been replaced with a set of source datasets.  This is to allow products (composites) to also be datasets and to allow a dataset to be generated from multiple source datasets.</p>
<p>The software field has been replaced with a set of software packages and tags.  This is to allow multiple software packages for a single processed dataset.  Each package will also have a repository tag assigned to it to specify the "version" used for this particualar dataset.</p>
<p>The ID Type is no longer maintained in the EMDAC database so is no longer required in the tools.</p>
<p>The Best Software flag is no longer required because the software/repository field implies that the software has been updated within the repository.</p>
<p>The UTC offset, DST flag, Collection Time Notes, Source Information Notes, and Dataset Documention Link are special extensions of a dataset that is a source dataset for a processed dataset.  They will be used for new views to the user in the future and to make sure the user has looked into this information for each source dataset.</p>
<p>The dataset will contain details to handle multiple versions for a dataset.  This will allow multiple instances of a given task to maintain information if new data arrives, multiple processing of data, etc.</p>

<p><a href="#top">Top</a></p>
<hr />


<a name="user"></a>
<h2>User</h2>
<table align="center" border="1" width="90%">
  <tr>
    <th>In DLN & IVEN</th>
    <th>To be in DTS</th>
  </tr>
  <tr>
    <td valign="top">
      <ul>
        <li>User ID</li>
        <li>First Name</li>
        <li>Last Name</li>
        <li>Email</li>
        <li>Active Flag</li>
      </ul>
    </td>
    <td valign="top">
      <ul>
        <li>User ID</li>
        <li>Display Name</li>
        <li>Email</li>
        <li>Active Flag</li>
        <li>Password</li>
        <li>Roles</li>
      </ul>
    </td>
  </tr>
</table>
<p>The First and Last Name fields have been merged into a single display name field to better match the EMDAC database.</p>
<p>The password and roles are for logging the user into the database and allowing them to perform certain actions within the DTS.</p>
<p><a href="#top">Top</a></p>
<hr />


<a name="software"></a>
<h2>Software</h2>
<table align="center" border="1" width="90%">
  <tr>
    <th>In DLN & IVEN</th>
    <th>To be in DTS</th>
  </tr>
  <tr>
    <td valign="top">
      N/A
    </td>
    <td valign="top">
      <ul>
        <li>Software ID</li>
        <li>Name</li>
        <li>Language(s)</li>
        <li>Deployed Directory</li>
        <li>Description</li>
        <li>Repository Location</li>
        <li>Documentation Link(s)</li>
      </ul>
    </td>
  </tr>
</table>
<p>Software is new the DTS.  It is used to maintain the software packages used by the DMG.  These packages will be associated to datasets that used the packages as part of the processing.</p>
<p>The name field is a unique text identifier for the software package.  This would be something like DTS, NWS Sounding Converion, Surface Check Comp Tool, etc.  It is the text displayed to the users to associate software packages to datasets.</p>
<p><a href="#top">Top</a></p>
<hr />

<p class="foot">Last Updated: Oct 2, 2007 - jclawson</p>

</body>
</html>
