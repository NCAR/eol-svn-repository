


<%@ page import="meta.Frequency" %>
<!doctype html>
<html>
	<head>
		<meta name="layout" content="main">
		<g:set var="entityName" value="${message(code: 'frequency.label', default: 'Frequency')}" />
		<title><g:message code="default.list.label" args="[entityName]" /></title>
	</head>
	<body>		
		<a href="#list-frequency" class="skip" tabindex="-1"><g:message code="default.link.skip.label" default="Skip to content&hellip;"/></a>
		<div class="nav" role="navigation">
			<g:render template="nav"/>
		</div>
		<div id="list-frequency" class="content scaffold-list" role="main">
			<h1><g:message code="default.list.label" args="[entityName]" /></h1>
			<g:if test="${flash.message}">
			<div class="message" role="status">${flash.message}</div>
			</g:if>
			<table>
				<thead>
					<tr>
					
						<g:sortableColumn property="name" title="${message(code: 'frequency.name.label', default: 'Name')}" />
					
						<g:sortableColumn property="sortKey" title="${message(code: 'frequency.sortKey.label', default: 'Sort Key')}" />
					
					</tr>
				</thead>
				<tbody>
				<g:each in="${frequencyInstanceList}" status="i" var="frequencyInstance">
					<tr class="${(i % 2) == 0 ? 'even' : 'odd'}">
					
						<td><g:link action="show" id="${frequencyInstance.id}">${fieldValue(bean: frequencyInstance, field: "name")}</g:link></td>
					
						<td>${fieldValue(bean: frequencyInstance, field: "sortKey")}</td>
					
					</tr>
				</g:each>
				</tbody>
			</table>
			<div class="pagination">
				<g:paginate total="${frequencyInstanceTotal}" />
			</div>
		</div>
	</body>
</html>
