


<%@ page import="meta.GcmdInstrument" %>
<!doctype html>
<html>
	<head>
		<meta name="layout" content="main">
		<g:set var="entityName" value="${message(code: 'gcmdInstrument.label', default: 'GcmdInstrument')}" />
		<title><g:message code="default.list.label" args="[entityName]" /></title>
	</head>
	<body>		
		<a href="#list-gcmdInstrument" class="skip" tabindex="-1"><g:message code="default.link.skip.label" default="Skip to content&hellip;"/></a>
		<div class="nav" role="navigation">
			<g:render template="nav"/>
		</div>
		<div id="list-gcmdInstrument" class="content scaffold-list" role="main">
			<h1><g:message code="default.list.label" args="[entityName]" /></h1>
			<g:if test="${flash.message}">
			<div class="message" role="status">${flash.message}</div>
			</g:if>
			<table>
				<thead>
					<tr>
					
						<g:sortableColumn property="keyword" title="${message(code: 'gcmdInstrument.keyword.label', default: 'Keyword')}" />
					
					</tr>
				</thead>
				<tbody>
				<g:each in="${gcmdInstrumentInstanceList}" status="i" var="gcmdInstrumentInstance">
					<tr class="${(i % 2) == 0 ? 'even' : 'odd'}">
					
						<td><g:link action="show" id="${gcmdInstrumentInstance.id}">${fieldValue(bean: gcmdInstrumentInstance, field: "keyword")}</g:link></td>
					
					</tr>
				</g:each>
				</tbody>
			</table>
			<div class="pagination">
				<g:paginate total="${gcmdInstrumentInstanceTotal}" />
			</div>
		</div>
	</body>
</html>
