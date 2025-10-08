


<%@ page import="meta.ProjectMember" %>
<!doctype html>
<html>
	<head>
		<meta name="layout" content="main">
		<g:set var="entityName" value="${message(code: 'projectMember.label', default: 'ProjectMember')}" />
		<title><g:message code="default.list.label" args="[entityName]" /></title>
	</head>
	<body>		
		<a href="#list-projectMember" class="skip" tabindex="-1"><g:message code="default.link.skip.label" default="Skip to content&hellip;"/></a>
		<div class="nav" role="navigation">
			<g:render template="nav"/>
		</div>
		<div id="list-projectMember" class="content scaffold-list" role="main">
			<h1><g:message code="default.list.label" args="[entityName]" /></h1>
			<g:if test="${flash.message}">
			<div class="message" role="status">${flash.message}</div>
			</g:if>
			<table>
				<thead>
					<tr>
					
						<th><g:message code="projectMember.member.label" default="Member" /></th>
					
						<g:sortableColumn property="memberType" title="${message(code: 'projectMember.memberType.label', default: 'Member Type')}" />
					
						<th><g:message code="projectMember.project.label" default="Project" /></th>
					
					</tr>
				</thead>
				<tbody>
				<g:each in="${projectMemberInstanceList}" status="i" var="projectMemberInstance">
					<tr class="${(i % 2) == 0 ? 'even' : 'odd'}">
					
						<td><g:link action="show" id="${projectMemberInstance.id}">${fieldValue(bean: projectMemberInstance, field: "member")}</g:link></td>
					
						<td>${fieldValue(bean: projectMemberInstance, field: "memberType")}</td>
					
						<td>${fieldValue(bean: projectMemberInstance, field: "project")}</td>
					
					</tr>
				</g:each>
				</tbody>
			</table>
			<div class="pagination">
				<g:paginate total="${projectMemberInstanceTotal}" />
			</div>
		</div>
	</body>
</html>
