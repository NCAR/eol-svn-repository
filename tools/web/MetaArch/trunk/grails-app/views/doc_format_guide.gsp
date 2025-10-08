<%@ page import="grails.plugins.springsecurity.SpringSecurityService" %>
<%@ page import="meta.*" %>
<%@ page import="meta.auth.*" %>
<% def springSecurityService %>
<!doctype html>
<html>
	<head>
		<meta name="layout" content="main"/>
		<title>EOL CDS Metadata Tool - Documentation Guidelines</title>
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
			
			#readme_template {
				font-size: 15px;
				padding: 0 1.5em;
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
				  <!-- DOCUMENTATION GUIDELINES -->
				  <h1>Documentation Guidelines</h1>
				  <p>The documentation
				  (i.e. the "Readme" file) that accompanies each project data set is as
				  important as the data itself. This
				  information permits collaborators and other analysts to become aware of the
				  data and to understand any limitations or special characteristics of data that
				  may impact its use elsewhere. The data
				  set documentation should accompany all data set submissions and contain the
				  information listed in the outline below. While it will not be appropriate for 
				  each and every data set to have
				  information in each documentation category, the following outline (and content)
				  should be adhered to as closely as possible to make the documentation
				  consistent across all data sets. It is
				  also recommended that a documentation file submission accompany for each
				  preliminary and final data set.</p>
				  
				  <div id="readme_template">
				  <p>---TITLE: This should
				  match the data set name</p>
				  
				  <p>---AUTHOR(S):
				  <br>
				  -Name(s)
				  of PI and all co-PIs 
				  <br>
				  -Complete
				  mailing address, telephone/facsimile Nos., web pages and E-mail address of PI
				  <br>
				  -Similar
				  contact information for data questions (if different than above)</p>
				  
				  <p>---FUNDING SOURCE AND
				  GRANT NUMBER:</p>
				  
				  <p>---DATA SET OVERVIEW:
				  <br>
				  -Introduction
				  or abstract
				  <br>
				  -Time
				  period covered by the data
				  <br>
				  -Physical
				  location of the measurement or platform (latitude/longitude/elevation)
				  <br>
				  -Data
				  source, if applicable (e.g. for operational data include agency)
				  <br>
				  -Any
				  World Wide Web address references (i.e. additional documentation such as
				  Project WWW site)</p>
				  
				  <p>---INSTRUMENT
				  DESCRIPTION:
				  <br>
				  -Brief
				  text (i.e. 1-2 paragraphs) describing the instrument with references
				  <br>
				  -Figures
				  (or links), if applicable
				  <br>
				  -Table
				  of specifications (i.e. accuracy, precision, frequency, etc.)</p>
				  
				  <p>---DATA COLLECTION
				  and PROCESSING: 
				  <br>
				  -Description
				  of data collection
				  <br>
				  -Description
				  of derived parameters and processing techniques used
				  <br>
				  -Description
				  of quality control procedures
				  <br>
				  -Data
				  intercomparisons, if applicable</p>
				  
				  <p>---DATA FORMAT:
				  <br>
				  -Data
				  file structure, format and file naming conventions (e.g. column delimited
				  ASCII, NetCDF, GIF, JPEG, etc.)
				  <br>
				  -Data
				  format and layout (i.e. description of header/data records, sample records)
				  <br>
				  -List
				  of parameters with units, sampling intervals, frequency, range 
				  <br>
				  -Description
				  of flags, codes used in the data, and definitions (i.e. good, questionable,
				  missing, estimated, etc.)
				  <br>
				  -Data
				  version number and date</p>
				  
				  <p>---DATA REMARKS:
				  <br>
				  -PI's
				  assessment of the data (i.e. disclaimers, instrument problems, quality issues,
				  etc.) Missing data periods
				  <br>
				  -Software
				  compatibility (i.e. list of existing software to view/manipulate the data)</p>
				  
				  <p>---REFERENCES:
				  <br>
				  -List
				  of documents cited in this data set description</p>
				  
				  </div>
				  
				  
				  <!-- DATA FORMAT GUIDELINES -->
				  <h3>Data Format Guidelines</h3> 
				  <p> 
				    An inherent flexibility of the EOL data management system permits data in all different
				    formats to be submitted, stored and retrieved from CODIAC. EOL is prepared to work with the
				    participants to bring their data to the archive and make sure it is presented,
				    with proper documentation, for exchange with other project participants. In
				    anticipation of receiving many data sets from the field sites in ASCII format
				    we are providing guidelines below that will aid in the submission, integration
				    and retrieval of these data. EOL will work with any participants submitting other
				    formats including NetCDF, AREA, HDF and GRIB to assure access and retrieval 
				    capabilities within CODIAC.
				  </p>  
				  <p> 
				    The following ASCII data format guidelines are intended to help standardize the information
				    provided with any data archived for the project. These guidelines are based 
				    on EOL experience in handling thousands of different data files of differing formats. 
				    Specific suggestions are provided for naming a data file as well as information 
				    and layout of the header records and data records contained in each file. This
				    information is important when data are shared with other project participants
				    to minimize confusion and aid in the analysis. An example of the layout of an 
				    ASCII file using the guidelines is provided below.Keep in mind that it is
				    not mandatory that the data be received in this format. However, if the project
				    participants are willing to implement the data format guidelines described
				    below, there are some improved capabilities for integration, extraction,
				    compositing and display via CODIAC that are available.
				  </p> 
				
				  <h4> Naming Convention </h4> 
				  <p> 
				    A) All data files should be uniquely named. For example, it is very helpful
				    if date can be included in any image file name so that the file can be easily
				    time registered. Also include an extension indicating the type of file:
				  </p>
				  <p style="text-indent:0.7in">i.e.</p>
				  <p style="text-indent:1.0in">
				    <span style="text-indent:1">       </span>.gif = GIF image format file
				  </p>
				  <p style="text-indent:1.0in"> 
				    <span style="text-indent:1">            </span>.jpg = jpg image format file
				  </p> 
				  <p style="text-indent:1.0in"> 
				    <span style="text-indent:1">            </span>.txt = Text or ASCII format file
				  </p> 
				  <p style="text-indent:1.0in"> 
				    <span style="text-indent:1">            </span>.cdf = NetCDF format file
				  </p> 
				  <p style="text-indent:1.0in"> 
				    <span style="text-indent:1">            </span>.tar = archival format
				  </p> 
				  <p style="text-indent:1.3in"> 
				    <span style="text-indent:1">            </span> 
				    If compressed, the file name should have an additional extension indicating the type of
				    compression (i.e. .gz, .z, etc.).</p> 
				  <p></p>
				  <!--
				  <p> 
				    B) For Text (ASCII) files, the records should consist of both header 
				    records and data records. The header records at a minimum should consist 
				    of:
				  </p> 
				 
				  <div style="margin: 0px 20px 0px 20px;">
				    <h4>ASCII Data Format Header Records Specifications</h4> 
				    <p>  
				      Standard header records should precede the data records within the file 
				      itself. The header records should contain the following information:
				    </p> 
				    <p> 
				      </p><table width="100%" border="1" cellpadding="10"> 
				        <tbody><tr> 
				          <td width="15%"><span style="font-size:10px">PI/DATA CONTACT = </span></td> 
				          <td width="85%"><span style="font-size:10px">Text [PI and data contact name(s) and affiliation(s)]</span></td> 
				        </tr> 
				        <tr> 
				          <td><span style="font-size:10px">DATA COVERAGE =</span></td> 
				          <td><span style="font-size:10px">Start/Stop time of continuous data or sampling interval 
				          (Use data/time format described below)</span></td> 
				        </tr> 
				        <tr> 
				          <td><span style="font-size:10px">PLATFORM ID =</span></td> 
				          <td><span style="font-size:10px">Text [e.g. Cruise #, Station(s) #, Mooring ID, etc.] </span></td> 
				        </tr> 
				        <tr> 
				          <td><span style="font-size:10px">INSTRUMENT =</span></td> 
				          <td><span style="font-size:10px">Text [Mooring, CTD, VPR, etc.]</span></td> 
				        </tr> 
				        <tr> 
				          <td><span style="font-size:10px">LOCATION = </span></td> 
				          <td><span style="font-size:10px">Text [Range of Lat, Long coordinates or Sea name] </span></td> 
				        </tr> 
				        <tr> 
				          <td><span style="font-size:10px">DATA VERSION =</span></td> 
				          <td><span style="font-size:10px">Alphanumeric [unique ID (i.e. revision date, PRELIMNARY or FINAL]</span></td> 
				        </tr> 
				        <tr> 
				          <td><span style="font-size:10px">REMARKS =</span></td> 
				          <td><span style="font-size:10px">Text [PI remarks that aid in understanding data file structure 
				          and contents. Items such as file type, how missing and/or bad data 
				          are denoted or any other information helpful to users of this data]</span></td> 
				        </tr> 
				      </tbody></table> 
				    <p></p>
				    <p>
				      Missing Value
				      indicator - Text or integer
				      [value used for data for missing information] (e.g. -99 or 999.99, etc)
				    </p> 
				    <p>
				      Below Measurement
				      Threshold - Text or Integer
				      [Value used to signify reading below instrument detection threshold] (e.g.
				      &lt;0.00005)
				    </p> 
				    <p> 
				      Above Measurement
				      Threshold - Text or Integer
				      [Value used to signify reading at or above instrument saturation]
				    </p> 
				    <p> 
				      **NOTE** This type of
				      header information cannot be contained within GIF and Postscript files. They
				      will need to be submitted with attached files or separate documentation
				      containing this information. 
				    </p> 
				    
				  </div>
				  -->
				
				  <!--
				  <h4>Data Records Specifications:</h4> 
				  <ol id="olData">
				    <li style="background: none; list-style-type: decimal; font-size: 12px;">
				      First data record consists of parameters identifying each
				      column. Multiple parameter names should
				      be shown as one word (e.g. thaw_depth or Leaf_area_index)
				    </li> 
				    <li style="background: none; list-style-type: decimal; font-size: 12px;">
				      Second data record consists of respective parameter
				      units. Multiple unit names should be
				      shown as one word (e.g. crystals_per_liter) 
				    </li>
				    <li style="background: none; list-style-type: decimal; font-size: 12px;"> 
				      Third data record begins actual data and consists of a
				      standard variable set (see item 4) and subsequent observations at that time and
				      position. 
				    </li>
				    <li style="background: none; list-style-type: decimal; font-size: 12px;">
				      Data records need to
				      include a common set of standard variables so that data is placed within the
				      proper context of where it was collected. The appropriate standard variables
				      vary somewhat depending upon what instrument was used to collect the data. In
				      general the standard variables that should be included in every data set 
				      submitted for BSIERP should include: Cruise ID, Station #/Mooring #,
				      Event(Cast) #, Bottle #, Date, Time, Latitude, Longitude, Depth, in that order
				      followed by other PI data.
				    </li>
				    <li style="background: none; list-style-type: decimal; font-size: 12px;"> 
				      Date/time must be in UTC and recommended format is: YYYYMMDDHHmmss.ss 
				      where:
				      <p style="text-indent:1"> YYYY= Year </p> 
				      <p style="text-indent:1.0in"> MM = Month (00-12) </p> 
				      <p style="text-indent:1.0in"> DD = Day (01-31) </p> 
				      <p style="text-indent:1.0in"> HH = Hour (00-23) </p> 
				      <p style="text-indent:1.0in"> mm = Minute (00-59) </p> 
				      <p style="text-indent:1.0in"> ss = Second (00-59) </p> 
				      <p style="margin-left:1.0in"> .ss = Decimal Second (unlimited resolution based on sampling frequency) </p> 
				    </li>
				    <li style="background: none; list-style-type: decimal; font-size: 12px;"> 
				      For every data set, position coordinates (i.e. latitude,
				      longitude) should be expressed in decimal degrees for each data point. 
				      Water Depth should be reported in appropriate metric units. This may 
				      be done by: (a) providing date/time of collection with position 
				      coordinates in each data record; or (b) providing date/time of collection
				      for each data point in the submitted file, with an associated file 
				      containing date/time and location either from the platform navigation database or GPS file.
				      <p style="margin-left:2.0in;text-indent:-1.0in"> 
				        Latitude - <span style="text-indent:1">       </span> 
				        Northern hemisphere expressed as positive or "N" and
				      </p> 
				      <p style="text-indent:1.6in"> 
				        Southern hemisphere expressed as negative or "S".
				      </p> 
				      <p style="margin-left:2.0in;text-indent:-1.0in;"> 
				        Longitude -<span style="text-indent:1">     </span> 
				        0-360<sup>o</sup> moving east from Greenwich; west longitude goes from
				        180<sup>o</sup> to 360<sup>o</sup>; or
				      </p> 
				      <p style="text-indent:1.6in"> 
				        Eastern hemisphere expressed as positive or "E" and
				      </p>
				      <p style="text-indent:1.6in"> 
				        Western	hemisphere expressed as negative or "W".
				      </p> 
				      <p>
				        NOTE - Position information
				        in other grid conventions is acceptable but a conversion to latitude/longitude
				        should be provided where practical.
				      </p> 
				      <p>
				        NOTE - Having a
				        common date/time stamp and common position coordinates in each data record will
				        permit the ability to extract data and integrate multiple data records from
				        different data sets. If two times are provided (e.g. UTC and local), they should be put at the beginning of each
				        record.
				      </p> 
				      <p> 
				        Preferred format for ASCII data files is space, comma or tab
				        delimited columns, with a UTC date/time stamp at the beginning of each data
				        record. If the data in the file are comma delimited, decimal places must be
				        periods, not commas.
				      </p> 
				      <p></p><ul style="list-style-type: circle; background: none;"> 
				        <li style="list-style-type: circle; background: none;">
				          All data files must contain variable names and units of measurements as column headings (if applicable).
				        </li> 
				        <li style="list-style-type: circle; background: none;">
				          If, for some reason, the PI cannot provide the date/time in
				          the format shown above, it is important that the time be given in UTC. If local
				          time is also supplied, a conversionto UTC must be provided. In addition to UTC
				          and/or local time, other date/time formats (e.g. decimal days) can be 
				          used but must be fully documented.
				        </li> 
				        <li style="list-style-type: circle; background: none;"> 
				          The internal format structure of the file should remain
				          constant after the first data record to ensure continuity and permit plotting
				          and graphing.
				        </li> 
				        <li style="list-style-type: circle; background: none;">
				          Only COMPLETE replacement or updated data/metadata files can be accepted.
				        </li> 
				      </ul><p></p>
				    </li></ol>
				  -->
				
				  <!--
				  <div style="margin: 0px 20px 0px 20px;">
				  <h4>Sample Data Set (ASCII Format):</h4> 
				  <p> 
				    The following is an
				    example of an ASCII format data set in which the header precedes the reported
				    data, and the data is organized in columns separated by spaces. Each column is
				    identified by parameter and each parameter's units of measure are listed in the
				    respective column. Also each row has a date/time of observation reported in
				    Universal Time Coordinated (UTC) along with position coordinates. This data set
				    organization is ideal for plotting and integration of various data sets. This
				    data set format should be used whenever possible and could be easily produced
				    automatically from a spread sheet computer program.
				  </p>
				  <div style="margin: 0px 20px 0px 20px;">
				    <hr style="border-top: 1px dashed #000; margin-right: 40px;"> 
				    <p> PI/DATA CONTACT= Doe, John (Old Dominion)/ Doe, Jane (WHOI) </p>  
				    <p> DATA COVERAGE = START: 19990821133500; STOP: 19990821135500 UTC </p>
				    <p> PLATFORM ID = HLY-02-01 </p> 
				    <p> INSTRUMENT = CTD Water Sampling </p>
				    <p> LOCATION = Barrow Canyon Station 12 (BC 4) </p>
				    <p> DATA VERSION = 1.0 (10 March 2003), PRELIMINARY </p> 
				    <p> REMARKS = Old Dominion University </p> 
				    <p> REMARKS = ppm values are mole fraction </p> 
				    <p> REMARKS= nM/m3 at 25c and 101.3 kPa; DMS and NH4 in Parts per million (PPM) </p> 
				    <p> REMARKS = Missing data = 99.9; Bad data = 88.8 </p> 
				    <p> REMARKS = Data point Date/Time provided in UTC </p> 
				    <p></p><table width="100%" style="font-size: 12px;">
				     <tbody><tr>
				      <td width="11%">Cruise ID</td>
				      <td width="9%">Stn</td>
				      <td width="9%">Evnt</td>
				      <td width="9%">Bottle</td>
				      <td width="18%">Date/Time</td>
				      <td width="11%">Lat</td>
				      <td width="11%">Lon</td>
				      <td width="11%">Depth</td>
				      <td width="11%">O2</td> 
				     </tr>
				     <tr>
				      <td style="padding-bottom: 10px;">Int</td>
				      <td style="padding-bottom: 10px;">Int</td>
				      <td style="padding-bottom: 10px;">Int</td>
				      <td style="padding-bottom: 10px;">Int</td>
				      <td style="padding-bottom: 10px;">UTC</td>
				      <td style="padding-bottom: 10px;">Deg</td>
				      <td style="padding-bottom: 10px;">Deg</td>
				      <td style="padding-bottom: 10px;">M</td>
				      <td style="padding-bottom: 10px;">UMOL/KG</td>
				     </tr>
				     <tr>
				      <td>HLY-02-01</td><td>12</td><td>3</td><td>4</td><td>20020602124326</td><td>68.2378</td><td>-155.6294</td><td>120</td><td>0.13</td> 
				     </tr>
				     <tr>
				      <td>HLY-02-01</td><td>12</td><td>3</td><td>5</td><td>20020602124718</td><td>68.2378</td><td>-155.6294</td><td>120</td><td>0.18</td> 
				     </tr>
				     <tr>
				      <td colspan="9" style="text-indent: 30px;"><b>.</b></td> 
				     </tr>
				     <tr>
				      <td colspan="9" style="text-indent: 30px;"><b>.</b></td> 
				      </tr>
				     <tr>
				      <td colspan="9" style="text-indent: 30px;"><b>.</b></td>
				     </tr>
				     <tr>
				      <td colspan="9" style="text-indent: 25px;">etc.</td>
				     </tr>
				  </tbody></table><p></p>
				  <hr style="border-top: 1px dashed #000; margin-right: 40px;">
				  </div>
				  </div>
				  <br>
				  -->
				  
				  <h4>Spreadsheet and Columnar ASCII Data</h4>
				  <p> For spreadsheet and columnar ASCII data to be converted into shape files, please use the following template: </p>
				  <table id="xls_template" width="100%">
				    <tbody><tr>
				      <td>
						CruiseID 
					  </td>
					  <td>	
						text field
					  </td>
					  <td>	
						blank if not applicable (examples: HLY0601, PSEA0902)
					  </td>
					</tr>
					<tr>
					  <td>
						StationNum
					  </td>
					  <td>	
						text field
					  </td>
					  <td>	
						blank if not applicable
					  </td>
					</tr>
					<tr>
					  <td>
						StationNme
					  </td>
					  <td>	
						text field
					  </td>
					  <td>	
						blank if not applicable
					  </td>
					</tr>
					<tr>
					  <td>
						DataDate
					  </td>
					  <td>	
						text field, YYYY-MM-DD
					  </td>
					  <td>	
						other formats acceptable, e.g. YYYYMMDD
					  </td>
					</tr>
					<tr>
					  <td>
						DataYear
					  </td>
					  <td>	
						text field
					  </td>
					  <td>	
						blank if not applicable
					  </td>
					</tr>
					<tr>
					  <td>
						DataTime
					  </td>
					  <td>	
						text field, HH:mm:SS
					  </td>
					  <td>	
						other formats acceptable, e.g. HHmm or HHmmSS
					  </td>
					</tr>
					<tr>
					  <td>
						TimeZone
					  </td>
					  <td>	
						text field, UTC, MST, AKST, etc.
					  </td>
					  <td>	
						blank if not applicable
					</td></tr>
					<tr>
					  <td>
						UTCOffset
					  </td>
					  <td>
						text field, number of hours from UTC, + or - 
					  </td>
					  <td>
						used if TimeZone is not UTC - blank if not applicable
					  </td>
					</tr>
					<tr>
					  <td>
						Latitude
					  </td>
					  <td>
						decimal degrees
					  </td>
					  <td>
						use negative values for south latitude
					  </td>
					</tr>
					<tr>
					  <td>
						Longitude
					  </td>
					  <td>
						decimal degrees
					  </td>
					  <td>
						use negative values for west longitude
					  </td>
					</tr>
					<tr>
					  <td>
						Depth
					  </td>
					  <td>
						meters
					  </td>
					  <td>
						if not applicable, should be set to -999.99
					  </td>
					</tr>
					<tr>
					  <td>
						Data...
					  </td>
					  <td colspan="2">
						as many columns as needed
					  </td>
					</tr>
				  </tbody></table>
				  <p>
				  The order of the fields should be as they appear in the list.  Any other variables can be included after <span class="tmpcode">Depth</span> in any order.  The number <span class="tmpcode">-999.99</span> should be used for missing numeric values and a blank field for missing text values, or "unknown" if applicable.
				  </p>
				  <p>
				    <b>Note:</b>&nbsp;
				    Shape files limit names of fields to 10 characters. Certain reserved words for shape files can not be used for field names in the data files, such as Date, Year and Time.
				  </p>
				  
				  
				
				  <h3>Further Information</h3>
				  <p>
				    Please direct any questions regarding data format(s) and documentation guidelines to the project data management support team 
				    <!-- (<strong><u>pacmars at eol dot ucar dot edu</u></strong>) -->
				    .
				  </p>
				</div> <!-- end full_box_text -->
				
				
				  </div> <!-- end full_box -->
				</div>
		</div> <!-- End of main-body -->
	</body>
</html>