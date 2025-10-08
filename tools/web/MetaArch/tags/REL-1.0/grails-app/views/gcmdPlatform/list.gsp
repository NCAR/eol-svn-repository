


<%@ page import="meta.GcmdPlatform" %>
<!doctype html>
<html>
	<head>
		<meta name="layout" content="main">
		<g:set var="entityName" value="${message(code: 'gcmdPlatform.label', default: 'GcmdPlatform')}" />
		<title><g:message code="default.list.label" args="[entityName]" /></title>
	</head>
	<body>		
		<a href="#list-gcmdPlatform" class="skip" tabindex="-1"><g:message code="default.link.skip.label" default="Skip to content&hellip;"/></a>
		<div class="nav" role="navigation">
			<g:render template="nav"/>
		</div>
		<div id="list-gcmdPlatform" class="content scaffold-list" role="main">
			<h1><g:message code="default.list.label" args="[entityName]" /></h1>
			<g:if test="${flash.message}">
			<div class="message" role="status">${flash.message}</div>
			</g:if>
			<table>
				<thead>
					<tr>
					
						<g:sortableColumn property="keyword" title="${message(code: 'gcmdPlatform.keyword.label', default: 'Keyword')}" />
					
					</tr>
				</thead>
				<tbody>
				<g:each in="${gcmdPlatformInstanceList}" status="i" var="gcmdPlatformInstance">
					<tr class="${(i % 2) == 0 ? 'even' : 'odd'}">
					
						<td><g:link action="show" id="${gcmdPlatformInstance.id}">${fieldValue(bean: gcmdPlatformInstance, field: "keyword")}</g:link></td>
					
					</tr>
				</g:each>
				</tbody>
			</table>
			<div class="pagination">
				<g:paginate total="${gcmdPlatformInstanceTotal}" />
			</div>
		</div>
	</body>
</html>
