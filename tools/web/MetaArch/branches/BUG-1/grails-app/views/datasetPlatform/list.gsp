


<%@ page import="meta.DatasetPlatform" %>
<!doctype html>
<html>
	<head>
		<meta name="layout" content="main">
		<g:set var="entityName" value="${message(code: 'datasetPlatform.label', default: 'DatasetPlatform')}" />
		<title><g:message code="default.list.label" args="[entityName]" /></title>
	</head>
	<body>		
		<a href="#list-datasetPlatform" class="skip" tabindex="-1"><g:message code="default.link.skip.label" default="Skip to content&hellip;"/></a>
		<div class="nav" role="navigation">
			<g:render template="nav"/>
		</div>
		<div id="list-datasetPlatform" class="content scaffold-list" role="main">
			<h1><g:message code="default.list.label" args="[entityName]" /></h1>
			<g:if test="${flash.message}">
			<div class="message" role="status">${flash.message}</div>
			</g:if>
			<table>
				<thead>
					<tr>
					
						<th><g:message code="datasetPlatform.dataset.label" default="Dataset" /></th>
					
						<g:sortableColumn property="platform" title="${message(code: 'datasetPlatform.platform.label', default: 'Platform')}" />
					
					</tr>
				</thead>
				<tbody>
				<g:each in="${datasetPlatformInstanceList}" status="i" var="datasetPlatformInstance">
					<tr class="${(i % 2) == 0 ? 'even' : 'odd'}">
					
						<td><g:link action="show" id="${datasetPlatformInstance.id}">${fieldValue(bean: datasetPlatformInstance, field: "dataset")}</g:link></td>
					
						<td>${fieldValue(bean: datasetPlatformInstance, field: "platform")}</td>
					
					</tr>
				</g:each>
				</tbody>
			</table>
			<div class="pagination">
				<g:paginate total="${datasetPlatformInstanceTotal}" />
			</div>
		</div>
	</body>
</html>
