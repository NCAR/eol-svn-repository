


<%@ page import="meta.Project" %>
<!doctype html>
<html>
	<head>
		<meta name="layout" content="main">
		<g:set var="entityName" value="${message(code: 'project.label', default: 'Project')}" />
		<title><g:message code="default.list.label" args="[entityName]" /></title>
	</head>
	<body>		
		<a href="#list-project" class="skip" tabindex="-1"><g:message code="default.link.skip.label" default="Skip to content&hellip;"/></a>
		<div class="nav" role="navigation">
			<g:render template="nav"/>
		</div>
		<div id="list-project" class="content scaffold-list" role="main">
			<h1><g:message code="default.list.label" args="[entityName]" /></h1>
			<g:if test="${flash.message}">
			<div class="message" role="status">${flash.message}</div>
			</g:if>
			<table>
				<thead>
					<tr>
					
						<g:sortableColumn property="name" title="${message(code: 'project.name.label', default: 'Name')}" />
					
						<g:sortableColumn property="fullName" title="${message(code: 'project.fullName.label', default: 'Full Name')}" />
					
						<th><g:message code="project.fundingAgency.label" default="Funding" /></th>
					
<%--						<g:sortableColumn property="awardNumber" title="${message(code: 'project.awardNumber.label', default: 'Award Number')}" />--%>
					
						<th><g:message code="project.additionalMetadata.label" default="Additional Metadata?" /></th>
					
						<sec:ifAnyGranted roles="ROLE_ADMIN,ROLE_DEVELOP">
						<g:sortableColumn property="dateCreated" title="${message(code: 'project.dateCreated.label', default: 'Date Created')}" />
						</sec:ifAnyGranted>
						
						<th></th>
					
					</tr>
				</thead>
				<tbody>
				<g:each in="${projectInstanceList}" status="i" var="projectInstance">
					<tr class="${(i % 2) == 0 ? 'even' : 'odd'}">
					
						<td><g:link action="show" id="${projectInstance.id}">${fieldValue(bean: projectInstance, field: "name")}</g:link></td>
					
						<td>${fieldValue(bean: projectInstance, field: "fullName")}</td>
					
						<td>${fieldValue(bean: projectInstance, field: "fundingAgency")} ${fieldValue(bean: projectInstance, field: "awardNumber")}</td>
					
<%--						<td>${fieldValue(bean: projectInstance, field: "awardNumber")}</td>--%>
					
						<td><g:if test="${projectInstance?.additionalMetadata != null}">Yes</g:if><g:else>No</g:else></td>
					
						<sec:ifAnyGranted roles="ROLE_ADMIN,ROLE_DEVELOP">
						<td><g:formatDate date="${projectInstance.dateCreated}" /></td>
						</sec:ifAnyGranted>
						
						<td>
							<g:link controller="dataset" action="create" params="[project: projectInstance.name]">Add Dataset</g:link>
						</td>
					
					</tr>
				</g:each>
				</tbody>
			</table>
			<div class="pagination">
				<g:paginate total="${projectInstanceTotal}" />
			</div>
		</div>
	</body>
</html>
