<%@ page import="grails.plugins.springsecurity.SpringSecurityService" %>
<%@ page import="meta.*" %>
<%@ page import="meta.auth.*" %>
<% def springSecurityService %>
<!doctype html>
<html>
	<head>
		<meta name="layout" content="main"/>
		<title>EOL CDS Metadata Tool - Data Submission Procedures</title>
		<style type="text/css" media="screen">
			#status {
				background-color: #eee;
				border: .2em solid #fff;
				margin: 2em 2em 1em;
				padding: 1em;
				width: 12em;
				float: left;
				-moz-box-shadow: 0px 0px 1.25em #ccc;
				-webkit-box-shadow: 0px 0px 1.25em #ccc;
				box-shadow: 0px 0px 1.25em #ccc;
				-moz-border-radius: 0.6em;
				-webkit-border-radius: 0.6em;
				border-radius: 0.6em;
			}

			.ie6 #status {
				display: inline; /* float double margin fix http://www.positioniseverything.net/explorer/doubled-margin.html */
			}

			#status ul {
				font-size: 0.9em;
				list-style-type: none;
				margin-bottom: 0.6em;
				padding: 0;
			}

			#status h1 {
				text-transform: uppercase;
				font-size: 1.1em;
				margin: 0 0 0.3em;
			}

			#page-body {
				margin: 2em 1em 1.25em 18em;
			}
			
			.full_box { padding: 0 1.0em; }
			.full_box h1 {
				margin-left: 0;
				margin-right: 0;
			}
			.full_box h2, .full_box h3, .full_box h4 {
				color: #704b43;
				font-weight: normal;
				border-bottom: 1px solid #f2f2f2;
			}
			.full_box h4 { font-weight: bold; border-bottom: none; }
			.full_box table { border-top-color: #000; }
			.full_box table tr:hover { background: none; }

			h2 {
				margin-top: 1em;
				margin-bottom: 0.3em;
				font-size: 1em;
			}

			p {
				margin: 1.0em 0;
			}

			#controller-list ul {
				list-style-position: inside;
			}

			#controller-list li {
				list-style-position: inside;
				margin: 0.25em 0;
			}

			@media screen and (max-width: 480px) {
				#status {
					display: none;
				}

				#page-body {
					margin: 0 1em 1em;
				}

				#page-body h1 {
					margin-top: 0;
				}
			}
		</style>
	</head>
	<body>
		<g:set var="per" value="${User.get(sec.loggedInUserInfo(field:"id") as Long)}" />
		<div id="spinner" style="display:none">
			<img src="${createLinkTo(dir:'images',file:'spinner.gif')}" alt="Spinner" />
		</div>
		<a href="#page-body" class="skip"><g:message code="default.link.skip.label" default="Skip to content&hellip;"/></a>
		
		<!-- Nav Bar -->
		<div class="nav" role="navigation">
			<ul>
				<li><g:link controller="user" class="show" action="show" id="${per.id}">My Profile</g:link></li>
				<li><g:link controller="dataset" class="create" action="create">Create a Data Set</g:link></li>
				
				<sec:ifLoggedIn> 
					<li class="login-li">
						<span>
						Welcome back, <strong><g:user /></strong>!
						</span>
					</li>
					<li class="login-li">
						<g:link action="index" controller="logout"> <i>Logout</i> </g:link>
					</li>
				</sec:ifLoggedIn>
				<sec:ifNotLoggedIn>
					<li class="login-li">
						<g:link action="index" controller="login"> <i>Login</i> </g:link>
					</li>
<%--					<li class="login-li">--%>
<%--						<span>--%>
<%--						Not a user? <g:link action="index" controller="register"> <i>Register Here</i> </g:link>--%>
<%--						</span>--%>
<%--					</li>--%>
				</sec:ifNotLoggedIn>
			</ul>
		</div>
		
		<sec:ifAnyGranted roles="ROLE_ADMIN,ROLE_DEVELOP">
		<div class="admin-nav nav" role="navigation">
			<ul>
				<li>
					<a href="${createLink(uri: '/admin')}">Admin Page</a>
				</li>
				<li>
					<g:link controller="project" class="create" action="create">Create a Project</g:link>
				</li>
				<li>
					<g:link class="list" action="list" controller="user">Users</g:link>
				</li>
				<li>
					<g:link class="list" action="list" controller="project">Projects</g:link>
				</li>
				<li>
					<g:link class="list" action="list" controller="dataset">Data Sets</g:link>
				</li>
			</ul>
		</div>
		</sec:ifAnyGranted>
		<!-- End Nav Bar -->
		
		<div class="main-body">	
				<div class="full content">
				  
				  <div class="full_box">
				
				
				<div class="full_box_text">
				
				  <h1> Data Submission Procedures </h1>
				  <p>
				    The NCAR/Earth Observing Laboratory is prepared to accept your
				    data sets and documentation.
				<!--
				    Please note that we have
				    provided <a id='doc_form_link' target="_self" href="#" onclick="fill_r_content(this, 'doc_format_guide.html', 'doc_form');">
				    guidelines for the data formatting and documentation</a> that can be found
				    as one of the data links listed above.
				-->
				<!--
				    There is specific information below on how to get the data to EOL and also 
				    notifying us that you have sent the submission.
				-->
				  </p>
				
				  <h4> Data Submission Instructions </h4>
				  <p>
				    Data sets can be submitted using the MetaArch submission tool.
				    <!-- To access the MetaArch submission tool, <a target="_blank" href="https://data.eol.ucar.edu/metaarch">click here</a>. -->
				  </p>
				  <p>
					Please note that your login information for the MetaArch submission tool is <em>not the same as</em> your login for the mapserver.  If you are uncertain as to your login information or if you do not have a login for MetaArch, please contact us at <span class="doc_email"><strong><u>metaarch at eol dot ucar dot edu</u></strong></span>.
				  </p>
				  <!-- Add section that describes what a data set is and how it relates to files -->
				  <p>
				    A data set is a collection of related data and documentation files.  For example, underway seawater sampling data collected aboard a cruise as compiled into one or more data files with the appropriate documentation would be considered a data set.
				  </p>
				  <p>
				    To submit a data set to the archive, fill out the "Create a Data Set" form for the project.
				    <!-- To log into MetaArch and immediately begin filling out this form, <a href="https://data.eol.ucar.edu/metaarch/dataset/create?project=PacMARS">click here</a>. -->
				  </p>
				  <p>
					Once your data set has been created, you can add files to the data set file list by clicking on the "Edit Data / Documentation File List" link at the bottom of the data set display page, as shown below:
				  </p>
				  <p style="padding: 0px 30px;">
					<img src="${createLinkTo(dir:'images',file:'show_dataset_to_files.png')}" class="colorbox" alt="Navigating to the Data Set File List" title="Navigating to the Data Set File List" width="800px" style="float: left;">
				  </p>
				  <p style="clear: both;">
				    <br>
					Previously uploaded files for this data set will already appear in the list.  Files must be identified with their purpose (i.e. data file, event log, or metadata/documentation) and format type (e.g. ASCII text, shape file, Excel spreadsheet, etc.) when being added to the list.
				  </p>
				  <p>
				    It is very important that you include documentation with your data. Identify this file as "Metadata/Documentation" when adding it to the list.
				    If you need assistance creating documentation for your data set, please see the <a href="${createLink(uri: '/docFormatGuide')}">documentation format and guidelines</a>.
				  </p>
				  
<%--				  <p>--%>
<%--				    If you have one or more files which are over 100 MB in size (per file), or have a larger quantity of files to submit, it is recommended that you use our FTP space to submit your files.  For detailed instructions on how to use FTP, click <a href="unix_ftp_inst.html">here</a>.--%>
<%--				  </p>--%>
				  
				  <p>
				    We are also prepared to receive other media my means of shipping.
				  </p>
				<div align="left">
				  <table table="" border="1">
				    <tbody><tr>
				      <td style="padding: 3.75pt;">
				        <p><b>The EOL mailing address is:</b></p>
				      </td>
				      <td style="padding: 3.75pt;">
				        <p><b>The Fedex address is:</b></p>
				      </td>
				    </tr>
				    <tr>
				      <td style="padding: 10px;">
				        <p>
				          NCAR/EOL <br>
				          Attention: Don Stott <br>
				          PO Box 3000 <br>
				          Boulder, CO 80307
				        </p>
				      </td>
				      <td style="padding: 10px;">
				        <p>
				          NCAR/EOL <br>
				          Attention: Don Stott <br>
				          3090 Center Green Drive <br>
				          Boulder, CO 80301
				        </p>
				      </td>
				    </tr>
				  </tbody></table>
				</div>
				
				  <h4>Notification of Submission</h4>
				  <p>Please send an e-mail to the project data management support team 
				  <!-- (<u><span class="doc_email"><strong>pacmars at eol dot ucar dot edu</strong></span></u>) -->
				   to confirm that you have submitted data and documentation to our submission site or have shipped other media to us. Data submissions will be checked over for completeness of metadata and then added to the archive.
				  <!--The NPRB program manager will be notified when data have been received and are in the process of archival.--></p>
				  
				  <h3>Further Information</h3>
				  <p>Please e-mail the project data management support team 
				  <!-- (<u><span class="doc_email"><strong>metaarch at eol dot ucar dot edu</strong></span></u>) -->
				   if there are any other questions. Thanks in advance for your help.</p>
				
				</div> <!-- end full_box_text -->
				
				
				  </div> <!-- end full_box -->
				</div>
		</div> <!-- End of main-body -->
	</body>
</html>