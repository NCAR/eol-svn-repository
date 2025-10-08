<%@ page import="grails.plugins.springsecurity.SpringSecurityService" %>
<%@ page import="meta.*" %>
<%@ page import="meta.auth.*" %>
<% def springSecurityService %>
<!doctype html>
<html>
	<head>
		<meta name="layout" content="main"/>
		<title>EOL CDS Metadata Tool</title>
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

			h2 {
				margin-top: 1em;
				margin-bottom: 0.3em;
				font-size: 1em;
			}

			p {
				margin: 0.25em 0;
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
			
			ul#ds-list { display: table; }
			ul#ds-list li { display: table-row; }
			ul#ds-list li span { display: table-cell; }
			ul#ds-list li span.ds-prj { width: 100px; font-weight: bold; font-style: normal; }
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
			<!-- Quick Welcome Blurb -->
			<h1>Welcome to the Metadata Submission Tool!</h1>
			<p>
				Principal Investigators and other researchers associated with a project can login and 
				submit metadata for data sets.  Click on a specific project link to see 
				additional details associated with that project. Click on a specific data set to see 
				additional details associated with that data set, or click on "View all Data Sets" to
				see a list of data sets within your own project(s).
			</p>
			<!-- End Quick Welcome Blurb -->
			
			
			<sec:ifAnyGranted roles="ROLE_ADMIN,ROLE_DEVELOP">
			<g:listApprovalRequests />
			</sec:ifAnyGranted>
			
			<br />
			
			<!-- Display Associated Projects -->
			<div class="project-permitted" style="float: left;">
				<h4>List of Recent Projects</h4>
				<p style="clear: both;"></p>
				
				<script type="text/javascript">
					$(document).ready(function() {
						$('.prj').click(function() {
							var link = $(this).find('.prjtop > .name > a').attr('href');
							window.location = link;
						});
					});
				</script>
				
				<g:set var="projects" value="${per.projects()}" />
				<sec:ifAnyGranted roles="ROLE_ADMIN,ROLE_DEVELOP,ROLE_DMG">
				<g:set var="projects" value="${meta.Project.list()}" />
				</sec:ifAnyGranted>
				
				<g:if test="${projects.size() > 15}">
				<g:link controller="project" class="list" action="list">View all Projects</g:link>
				</g:if>
				
				<%i = 0%>
			    <g:each in="${projects.sort{a,b -> (a.endDate > b.endDate) ? -1 : 1}}" var="projectInstance">
			    	<%i++%>
			    	<g:if test="${i < 15}">
			        	<g:projectBox project="${projectInstance}" forDataset="false" />
			        </g:if>
			    </g:each>
			</div>
			<!-- End Display Associated Projects -->
			
			
			<p style="clear: both;"></p>
			<br />
			
			<!-- Display Associated Datasets -->
			<div class="project-permitted" style="float: left;">
				<h4>List of Most Recent Data Sets</h4>
				<g:set var="datasets" value="${meta.Dataset.findAllByAuthorOrPointOfContact(per, per)}" />
				<sec:ifAnyGranted roles="ROLE_ADMIN,ROLE_DEVELOP,ROLE_DMG">
				<g:set var="datasets" value="${meta.Dataset.list()}" />
				</sec:ifAnyGranted>
				
				<g:if test="${datasets.size() > 15}">
				<g:link controller="dataset" class="list" action="list">View all Data Sets</g:link>
				</g:if>
				
				<%i = 0%>
				<ul id="ds-list">
					<g:each in="${datasets.sort{a,b -> (a.lastUpdated > b.lastUpdated) ? -1 : 1}}" var="datasetInstance">
						<%i++%>
				    	<g:if test="${i < 15}">
							<li class="dataset-link"><g:link controller="dataset" class="show" action="show" id="${datasetInstance.id}">
								<sec:ifAnyGranted roles="ROLE_ADMIN,ROLE_DEVELOP,ROLE_DMG">
									<span class="ds-prj">${datasetInstance.project}</span>
								</sec:ifAnyGranted>
								<span><g:fieldValue bean="${datasetInstance}" field="title"/></span>
							</g:link></li>
						</g:if>
					</g:each>
				</ul>
			</div>
			<!-- End Display Associated Datasets -->
			
			
			
			<!-- Quick Welcome Blurb -->
			<p style="clear: both;"></p>
			<br />
			<p>
				This is the metadata submission form to be used by project investigators.
				Any data or documentation files larger than 100 MB (per individual file) 
				 will have to be transferred separately via FTP. Please see
				<a target="_blank" href="http://pacmars.eol.ucar.edu/data_submission.html"> 
<%--				<a target="_blank" href="${createLink(uri: '/dataSubmissions')}">--%>
				data submission instructions</a> for details.
			</p>
			<br />
			<!-- End Quick Welcome Blurb -->
		</div> <!-- End of main-body -->
	</body>
</html>
