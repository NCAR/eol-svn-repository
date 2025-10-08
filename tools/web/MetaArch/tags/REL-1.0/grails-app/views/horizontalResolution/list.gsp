


<%@ page import="meta.HorizontalResolution" %>
<!doctype html>
<html>
	<head>
		<meta name="layout" content="main">
		<g:set var="entityName" value="${message(code: 'horizontalResolution.label', default: 'HorizontalResolution')}" />
		<title><g:message code="default.list.label" args="[entityName]" /></title>
	</head>
	<body>		
		<a href="#list-horizontalResolution" class="skip" tabindex="-1"><g:message code="default.link.skip.label" default="Skip to content&hellip;"/></a>
		<div class="nav" role="navigation">
			<g:render template="nav"/>
		</div>
		<div id="list-horizontalResolution" class="content scaffold-list" role="main">
			<h1><g:message code="default.list.label" args="[entityName]" /></h1>
			<g:if test="${flash.message}">
			<div class="message" role="status">${flash.message}</div>
			</g:if>
			<table>
				<thead>
					<tr>
					
						<g:sortableColumn property="scale" title="${message(code: 'horizontalResolution.scale.label', default: 'Scale')}" />
					
					</tr>
				</thead>
				<tbody>
				<g:each in="${horizontalResolutionInstanceList}" status="i" var="horizontalResolutionInstance">
					<tr class="${(i % 2) == 0 ? 'even' : 'odd'}">
					
						<td><g:link action="show" id="${horizontalResolutionInstance.id}">${fieldValue(bean: horizontalResolutionInstance, field: "scale")}</g:link></td>
					
					</tr>
				</g:each>
				</tbody>
			</table>
			<div class="pagination">
				<g:paginate total="${horizontalResolutionInstanceTotal}" />
			</div>
		</div>
	</body>
</html>
