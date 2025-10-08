<%@ page import="grails.plugins.springsecurity.SpringSecurityService" %>
<%@ page import="meta.*" %>
<%@ page import="meta.auth.*" %>
<% def springSecurityService %>
<!doctype html>
<html>
	<head>
		<meta name="layout" content="main"/>
		<title>EOL CDS Metadata Tool - Documentation Guidelines</title>
		<link rel="stylesheet" type="text/css" href="${createLinkTo(dir:'css',file:'doc_guide.css')}">
		<script type="text/javascript" src="${createLinkTo(dir:'js',file:'dg_accordian.js')}"></script>
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
			<div class="nav-pills">
				<a class="nav-pill active" href="${createLink(uri: '/docFormatGuide')}">
					Documentation and Format Guidelines
				</a>
				<a class="nav-pill" href="${createLink(uri: '/dataSubmissions')}">
					Data Submission Instructions
				</a>
			</div>
			<div class="full content">
				<div class="full_box">
					<div class="full_box_text">
					
					
					
						<div id="intro">
							<p>
								Please adhere to the following documentation guidelines to ensure complete and 
								consistent documentation files for your data sets. Provide a documentation file 
								in this form for each file type included in the data set, using file names 
								unique to the combination of the data set and file type (e.g., 
								borehole_sitename_2013_doc.txt). Each updated version of the data set should be 
								accompanied by updated documentation. Please provide additional detail within 
								your documentation beyond what is outlined in these guidelines as you see fit.
								<!--
								These guidelines are not intended to be restrictive; please provide additional 
								detail within your documentation as you see fit.
								-->
							</p>
							<p>
								Contact <span class="email">metaarch (at) eol (dot) ucar (dot) edu</span> with 
								any questions or comments regarding these guidelines.
							</p>
						</div>
						
						<!-- 
						<hr class="divider">
					
						<h3>Template</h3>
						
						<div id="download">
							<p>
								A documentation template file can be downloaded and modified to be specific to 
								your dataset. For your convenience, the template file is provided in Microsoft 
								Word document and ASCII text formats.
							</p>
							<p>
								Download the template file here:&nbsp;&nbsp;&nbsp;&nbsp;
								<a class="download" href="http://arctic.eol.ucar.edu/doc_guide/ACADIS_doc_template.docx">[ DOC ]</a>
								&nbsp;&nbsp;&nbsp;&nbsp;
								<a class="download" target="_blank" href="http://arctic.eol.ucar.edu/doc_guide/ACADIS_doc_template.txt">[ TXT ]</a>
							</p>
						</div>
						-->
						
						<hr class="divider">
						
						<h3>Guidelines</h3>
							<div id="template">
							<div id="note">
								Click on the section titles below to expand/collapse each individual section.
							</div>
							
							<div class="section-title">Title<span class="arrow-down"></span></div>
							<div class="section-body">
								<ul>
									<li>This should match the dataset title</li>
									<li>Sufficiently descriptive (i.e. general content of dataset can be determined from title)</li>
									<li>Special characters allowed:  $ - _ . ! * ' ( ) ,</li>
									<li>No more than 255 characters</li>
								</ul>
								<div id="title-checker">
									<p>
										Paste your dataset title into the text box below to check the length of your title and mark any invalid characters:
									</p>
									<input id="title" name="title" type="text" value="">
									<span id="title-length"></span>
									<br>
									<span id="title-errors">
								</span></div>
							</div>
							
							<div class="section-title">Author(s)<span class="arrow-right"></span></div>
							<div class="section-body hide">
								<ul>
									<li>
										Full name(s) of authors of the dataset, listed in the order, for use 
										in a citation
									</li>
									<li>Full name(s) of any relevant associated personnel, if applicable</li>
									<li>For each author and associated person:
										<ul>
											<li>
												Include complete contact information: e-mail address, telephone 
												number, mailing address and web page
											</li>
											<li>
												Indicate his/her role (i.e. PI, Co-PI, Contributor, Metadata 
												Contact, etc.)
											</li>
											<li>
												Indicate his/her title, if available (e.g. Associate Scientist, 
												Research Associate, Professor)
											</li>
										</ul>
									</li>
									<li>Indicate corresponding author for data questions</li>
								</ul>
							</div>
							
							<div class="section-title">Funding Source and Grant Number<span class="arrow-right"></span></div>
							<div class="section-body hide">
								<ul>
									<li>List all funding agencies and associated award numbers (e.g. NSF xxxxxx)</li>
								</ul>
							</div>
							
							<div class="section-title">Data Set Overview<span class="arrow-right"></span></div>
							<div class="section-body hide">
								<ul>
									<li>
										Introduction or abstract - summarize the "who, where, what, when, and 
										why" of the data set
									</li>
									<li>Time period covered by the data</li>
									<li>Physical location of the measurements or platform, including:
										<ul>
											<li>Latitude (-90, 90)</li>
											<li>Longitude (-180, 180)</li>
											<li>Elevation/Depth</li>
										</ul>
									</li>
									<li>Data source (e.g. for operational data include agency)</li>
									<li>
										Background information (e.g. is this data set part of a larger 
										experiment or collection? If so, please provide specific details)
									</li>
									<li>
										Any web address references (i.e. additional documentation such as 
										Project web site, alternative data archive site)
									</li>
								</ul>
							</div>
							
							<div class="section-title">Platform(s)<span class="arrow-right"></span></div>
							<div class="section-body hide">
								<ul>
									<li>Describe the platform(s) or site(s) from which the data were collected</li>
									<li>Include maps, images and links</li>
								</ul>
							</div>
							
							<div class="section-title">Instrument(s)<span class="arrow-right"></span></div>
							<div class="section-body hide">
								<ul>
									<li>Describe each instrument with references, including:
										<ul>
											<li>
												Make, year, model, vendor, calibration and maintenance history, 
												deployment information, geo-location
											</li>
											<li>Table of specifications (i.e. accuracy, precision, frequency, etc.)</li>
											<li>
												Table of operational periods as well as "down" periods (see Data 
												Remarks below)
											</li>
										</ul>
									</li>
									<li>Include figures, diagrams and links</li>
								</ul>
							</div>
							
							<div class="section-title">Data Format<span class="arrow-right"></span></div>
							<div class="section-body hide">
								<div class="section-subtitle">File Information &amp; Organization</div>
								<ul>
									<li>Data file type (e.g. column delimited ASCII, NetCDF, GIF, JPEG, etc.)</li>
									<li>
										File naming conventions (e.g. 
										<em>cruise_year_instrument_data-type_version.txt</em>, 
										<em>instrument_data-type_begindate_enddate.nc</em>)
									</li>
									<li>
										Data format and layout (e.g. description of header/data records, sample 
										records)
									</li>
								</ul>
								<div class="section-subtitle">Parameters, Flags &amp; Codes</div>
								<ul>
									<li>List of parameters with units, sampling intervals, frequency, range
										<ul>
											<li>e.g. <a target="_blank" href="http://cf-pcmdi.llnl.gov/documents/cf-standard-names/standard-name-table/25/cf-standard-name-table.html">Climate and Forecast (CF) Metadata Convention standard names</a> for measurement types</li>
										</ul>
									</li>
									<li>
										Description of flags, codes used in the data, and definitions (i.e. good, 
										questionable, missing, estimated, etc.)
									</li>
								</ul>
								<div class="section-subtitle">Version</div>
								<ul>
									<li>Data version number and date</li>
								</ul>
							</div>
							
							<div class="section-title">Data Collection, Processing &amp; Methodology<span class="arrow-right"></span></div>
							<div class="section-body hide">
								<ul>
									<li>Description of:
										<ul>
											<li>Data collection and processing techniques and software used</li>
											<li>Derived parameters and methods</li>
											<li>Quality assurance and quality control procedures</li>
										</ul>
									</li>
								</ul>
							</div>
							
							<div class="section-title">Data Remarks<span class="arrow-right"></span></div>
							<div class="section-body hide">
								<ul>
									<li>
										PI's assessment of the data (i.e. disclaimers, instrument problems, uncertainty information, 
										data limitations, quality issues, etc.)
									</li>
									<li>Missing data periods</li>
									<li>
										Software compatibility (i.e. list of existing software (include version number if applicable) 
										to view/manipulate the data)
										<ul>
											<li>
												Include web page, copy of code or reference for less common and/or home-grown 
												software
											</li>
										</ul>
									</li>
								</ul>
							</div>
							
							<div class="section-title">References<span class="arrow-right"></span></div>
							<div class="section-body hide">
								<ul>
									<li>List references for materials cited in this dataset documentation</li>
									<li>List publication references that make use of these data in scientific study</li>
									<li>Include any DOIs or links to online resources</li>
								</ul>
							</div>
						</div>
						
						
						
					</div> <!-- end full_box_text -->
				</div> <!-- end full_box -->
			</div> <!-- end full content -->
		</div> <!-- End of main-body -->
	</body>
</html>