


<%@ page import="meta.GcmdLocation" %>
<!doctype html>
<html>
	<head>
		<meta name="layout" content="main">
		<g:set var="entityName" value="${message(code: 'gcmdLocation.label', default: 'GcmdLocation')}" />
		<title><g:message code="default.list.label" args="[entityName]" /></title>
	</head>
	<body>		
		<a href="#list-gcmdLocation" class="skip" tabindex="-1"><g:message code="default.link.skip.label" default="Skip to content&hellip;"/></a>
		<div class="nav" role="navigation">
			<g:render template="nav"/>
		</div>
		<div id="list-gcmdLocation" class="content scaffold-list" role="main">
			<h1><g:message code="default.list.label" args="[entityName]" /></h1>
			<g:if test="${flash.message}">
			<div class="message" role="status">${flash.message}</div>
			</g:if>
			<table>
				<thead>
					<tr>
					
						<g:sortableColumn property="keyword" title="${message(code: 'gcmdLocation.keyword.label', default: 'Keyword')}" />
					
						<th><g:message code="gcmdLocation.parent.label" default="Parent" /></th>
					
					</tr>
				</thead>
				<tbody>
				<g:each in="${gcmdLocationInstanceList}" status="i" var="gcmdLocationInstance">
					<tr class="${(i % 2) == 0 ? 'even' : 'odd'}">
					
						<td><g:link action="show" id="${gcmdLocationInstance.id}">${fieldValue(bean: gcmdLocationInstance, field: "keyword")}</g:link></td>
					
						<td>${fieldValue(bean: gcmdLocationInstance, field: "parent")}</td>
					
					</tr>
				</g:each>
				</tbody>
			</table>
			<div class="pagination">
				<g:paginate total="${gcmdLocationInstanceTotal}" />
			</div>
		</div>
	</body>
</html>
