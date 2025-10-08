


<%@ page import="meta.VerticalResolution" %>
<!doctype html>
<html>
	<head>
		<meta name="layout" content="main">
		<g:set var="entityName" value="${message(code: 'verticalResolution.label', default: 'VerticalResolution')}" />
		<title><g:message code="default.list.label" args="[entityName]" /></title>
	</head>
	<body>		
		<a href="#list-verticalResolution" class="skip" tabindex="-1"><g:message code="default.link.skip.label" default="Skip to content&hellip;"/></a>
		<div class="nav" role="navigation">
			<g:render template="nav"/>
		</div>
		<div id="list-verticalResolution" class="content scaffold-list" role="main">
			<h1><g:message code="default.list.label" args="[entityName]" /></h1>
			<g:if test="${flash.message}">
			<div class="message" role="status">${flash.message}</div>
			</g:if>
			<table>
				<thead>
					<tr>
					
						<g:sortableColumn property="scale" title="${message(code: 'verticalResolution.scale.label', default: 'Scale')}" />
					
					</tr>
				</thead>
				<tbody>
				<g:each in="${verticalResolutionInstanceList}" status="i" var="verticalResolutionInstance">
					<tr class="${(i % 2) == 0 ? 'even' : 'odd'}">
					
						<td><g:link action="show" id="${verticalResolutionInstance.id}">${fieldValue(bean: verticalResolutionInstance, field: "scale")}</g:link></td>
					
					</tr>
				</g:each>
				</tbody>
			</table>
			<div class="pagination">
				<g:paginate total="${verticalResolutionInstanceTotal}" />
			</div>
		</div>
	</body>
</html>
