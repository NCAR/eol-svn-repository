


<%@ page import="meta.Dataset" %>
<!doctype html>
<html>
	<head>
		<meta name="layout" content="main">
		<g:set var="entityName" value="${message(code: 'dataset.label', default: 'Dataset')}" />
		<title><g:message code="default.list.label" args="[entityName]" /></title>
	</head>
	<body>		
		<a href="#list-dataset" class="skip" tabindex="-1"><g:message code="default.link.skip.label" default="Skip to content&hellip;"/></a>
		<div class="nav" role="navigation">
			<g:render template="nav" />
		</div>
		<div id="list-dataset" class="content scaffold-list" role="main">
			<h1><g:message code="default.list.label" args="[entityName]" /></h1>
			<g:if test="${flash.message}">
			<div class="message" role="status">${flash.message}</div>
			</g:if>
			<table>
				<thead>
					<tr>
					
						<g:sortableColumn property="title" title="${message(code: 'dataset.title.label', default: 'Title')}" />
					
<%--						<g:sortableColumn property="datasetVersion" title="${message(code: 'dataset.datasetVersion.label', default: 'Dataset Version')}" />--%>
					
						<th><g:message code="dataset.project.label" default="Project" /></th>
					
						<th><g:message code="dataset.author.label" default="Author" /></th>
					
						<th><g:message code="dataset.fundingAgency.label" default="Funding" /></th>
					
<%--						<g:sortableColumn property="awardNumber" title="${message(code: 'dataset.awardNumber.label', default: 'Award Number')}" />--%>

						<th><g:message code="dataset.progress.label" default="Progress" /></th>
					
					</tr>
				</thead>
				<tbody>
				<g:each in="${datasetInstanceList}" status="i" var="datasetInstance">
					<tr class="${(i % 2) == 0 ? 'even' : 'odd'}">
					
						<td><g:link action="show" id="${datasetInstance.id}">${fieldValue(bean: datasetInstance, field: "title")}</g:link></td>
					
<%--						<td>${fieldValue(bean: datasetInstance, field: "datasetVersion")}</td>--%>
					
						<td>${fieldValue(bean: datasetInstance, field: "project")}</td>
						
						<td>${fieldValue(bean: datasetInstance, field: "author")}</td>
					
						<td>${fieldValue(bean: datasetInstance, field: "fundingAgency")} ${fieldValue(bean: datasetInstance, field: "awardNumber")}</td>
					
<%--						<td>${fieldValue(bean: datasetInstance, field: "awardNumber")}</td>--%>

						<td>${fieldValue(bean: datasetInstance, field: "progress")}</td>
					
					</tr>
				</g:each>
				</tbody>
			</table>
			<div class="pagination">
				<g:paginate total="${datasetInstanceTotal}" />
			</div>
		</div>
	</body>
</html>
