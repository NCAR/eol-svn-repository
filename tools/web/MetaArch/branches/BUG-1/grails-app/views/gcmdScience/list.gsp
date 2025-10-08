


<%@ page import="meta.GcmdScience" %>
<!doctype html>
<html>
	<head>
		<meta name="layout" content="main">
		<g:set var="entityName" value="${message(code: 'gcmdScience.label', default: 'GcmdScience')}" />
		<title><g:message code="default.list.label" args="[entityName]" /></title>
	</head>
	<body>		
		<a href="#list-gcmdScience" class="skip" tabindex="-1"><g:message code="default.link.skip.label" default="Skip to content&hellip;"/></a>
		<div class="nav" role="navigation">
			<g:render template="nav"/>
		</div>
		<div id="list-gcmdScience" class="content scaffold-list" role="main">
			<h1><g:message code="default.list.label" args="[entityName]" /></h1>
			<g:if test="${flash.message}">
			<div class="message" role="status">${flash.message}</div>
			</g:if>
			<table>
				<thead>
					<tr>
					
						<g:sortableColumn property="keyword" title="${message(code: 'gcmdScience.keyword.label', default: 'Keyword')}" />
					
						<th><g:message code="gcmdScience.parent.label" default="Parent" /></th>
					
					</tr>
				</thead>
				<tbody>
				<g:each in="${gcmdScienceInstanceList}" status="i" var="gcmdScienceInstance">
					<tr class="${(i % 2) == 0 ? 'even' : 'odd'}">
					
						<td><g:link action="show" id="${gcmdScienceInstance.id}">${fieldValue(bean: gcmdScienceInstance, field: "keyword")}</g:link></td>
					
						<td>${fieldValue(bean: gcmdScienceInstance, field: "parent")}</td>
					
					</tr>
				</g:each>
				</tbody>
			</table>
			<div class="pagination">
				<g:paginate total="${gcmdScienceInstanceTotal}" />
			</div>
		</div>
	</body>
</html>
