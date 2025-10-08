


<html>
<head>
   <title>DTS Workflow</title>

</head>
<body>

<h1>Data Tracking System Development Workflow</h1>
<p>This document provides an overview and outline for the development of the Data Tracking System.</p>


<h1>Phase 1</h1>
<p>This is the current state of the DTS following the upgrades to the ML, Arctic ML, and the ML Editor tool in the summer and fall of 2006.  Each of the tools (DLN, IVEN, and ML) are separate databases.  In this phase the Arctic and general ML have been combined into a single database.  There is no shared information between the tools.</p>

<p><a href="../images/layouts/config1.0.png">Configuration 1.0</a>- This was the state of the databases and tools before the ML merge.</p>
<p><a href="../images/layouts/config1.5.png">Configuration 1.5</a>- This is the current state (Spring 2007) of the database and tools.</p>

<ul>Current Tool Links
   <li><a href="/dln/?project=T-REX">DLN (T-REX)</a></li>
   <li><a href="/cgi-bin/iven/project_view?project=T-REX">IVEN (T-REX)</a></li>
   <li><a href="/master_list/">ML Editor (T-REX)</a></li>
</ul>

<hr />


<h1>Phase 2</h1>

<p>This phase will merge the DLN and IVEN tools to use a single database where the information will be shared between the two tools, without major changes to the tools themselves.  (<a href="../images/layouts/config2.0.png">Configuration 2.0</a>)</p>

<h2>2.1  Finalize the Requirements for the DTS</h2>
<ul>
   <li>Track datasets and associated metadata (by project) through their entire lifecycle.</li>
   <li>Track the software used to prepare a dataset (processing, checking, QA, etc.).</li>
   <li>Track and maintain dataset issues and concerns.</li>
</ul>
<p>Click <a href="../requirements/">here</a> for a rough draft of functionality and definitions.</p>


<h2>2.2  Finalize and Implement the DTS Database</h2>
<p>Finalize the database design and implement the phase 2 version in MySQL.</p>


<h2>2.3  Upgrade the Data Loading Notes (DLN) Interface</h2>
<p>Upgrade the DLN tool to use the new DTS database and port the existing data in the DLN database to it.</p>
<p>The only new/updated functionality will be the functionality that is required due to the database change.  Other minor upgrades may be possible.</p>


<h2>2.4  Upgrade the Processing Inventory Tool (IVEN) Interface</h2>
<p>Upgrade the IVEN tool to use the new DTS database and port the existing data in the IVEN database to it.</p>
<p>The only new/updated functionality will be the functionality that is required due to the database change.  Other minor upgrades may be possible.</p>

<hr />


<h1>Phase 3</h1>
<p>Phase 3 is making a new DTS interface to fully utilize the new database.  (<a href="../images/layouts/config3.0.png">Configuration 3.0</a>).</p>

<h2>3.1  Create the new DTS Interface</h2>
<p>Design and implement a new web interface to simplify the management of a dataset through its entire lifecycle.  New functionality will be added/upgraded to make full use of the DTS database.</p>
<p>Merge in the functionality of the Master List editor with the rest of the DTS to provide a single DTS dataset managment tool for a dataset/project.</p>

<hr />


<h1>Phase 4</h1>
<p>This phase joins the DTS and ML databases with ZEDI to reduce redundancy and share the most information between tools.</p>
<p><a href="../images/layouts/config4.0b.png">Configuration 4.0b</a>- This configuration merges the entire ML database into ZEDI.</p>
<p><a href="../images/layouts/config4.0a.png">Configuration 4.0a</a>- This configuration keeps the ML specific information within its own database that is connected to ZEDI.</p>

<h2>4.1  Connect the DTS Database with ZEDI</h2>
<p>Issues (i.e. categories/platforms) must be resolved before this can be accomplished.</p>
<p>Merge the duplicated information between the DTS and Master List databases into ZEDI and remove it from the Phase 3 database(s).</p>
<p>Add foreign key constraints across the databases into zedi.</p>
<p>Update the DTS and public ML view interfaces to use the Phase 4 database design.</p>


<hr />

</body>
</html>
