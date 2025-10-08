<%@ page contentType="text/html;charset=ISO-8859-1" %>
<html>
	<head>
		<meta name="layout" content="main">
		<g:set var="entityName" value="${message(code: 'Dataset.label', default: 'Dataset')}" />
		<title><g:message code="default.create.label" args="[entityName]" /> - Select a Project</title>
	</head>
<body>

	<a href="#getProject-Dataset" class="skip" tabindex="-1"><g:message code="default.link.skip.label" default="Skip to content&hellip;"/></a>
	<div class="nav" role="navigation">
		<g:render template="nav" />
	</div>

	<div class="body" style="overflow-y: hidden;">
	  	<h1><g:message code="default.create.label" args="[entityName]" /> - Select a Project</h1>
	
		<g:if test="${flash.message}">
			<div class="message">${flash.message}</div>
		</g:if>		
		
		<p class="tooltip">
			Do you have an update for an existing data set?
			&nbsp;
			If so, you do not need to create a new data set.
			&nbsp;
			<g:link controller="dataset" class="list" action="list">Click here</g:link> to view your data sets.
		</p>
		
		
		<script type="text/javascript">
			$(document).ready(function() {
				$('.prj').click(function() {
					var link = $(this).find('.prjtop > .name > a').attr('href');
					window.location = link;
				});
			});
		</script>
		
		<div id="project-list-div" class="list">
		    <g:each in="${projects.sort{a,b -> (a.endDate > b.endDate) ? -1 : 1}}" var="projectInstance">
		        <g:projectBox project="${projectInstance}" forDataset="true" />
		    </g:each>
		</div>
	</div>
	
</body>
</html>