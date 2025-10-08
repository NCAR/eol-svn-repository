


<%@ page import="meta.DatasetFrequency" %>
<!doctype html>
<html>
	<head>
		<meta name="layout" content="main">
		<g:set var="entityName" value="${message(code: 'datasetFrequency.label', default: 'DatasetFrequency')}" />
		<title><g:message code="default.list.label" args="[entityName]" /></title>
	</head>
	<body>		
		<a href="#list-datasetFrequency" class="skip" tabindex="-1"><g:message code="default.link.skip.label" default="Skip to content&hellip;"/></a>
		<div class="nav" role="navigation">
			<g:render template="nav"/>
		</div>
		<div id="list-datasetFrequency" class="content scaffold-list" role="main">
			<h1><g:message code="default.list.label" args="[entityName]" /></h1>
			<g:if test="${flash.message}">
			<div class="message" role="status">${flash.message}</div>
			</g:if>
			<table>
				<thead>
					<tr>
					
						<th><g:message code="datasetFrequency.dataset.label" default="Dataset" /></th>
					
						<g:sortableColumn property="frequency" title="${message(code: 'datasetFrequency.frequency.label', default: 'Frequency')}" />
					
					</tr>
				</thead>
				<tbody>
				<g:each in="${datasetFrequencyInstanceList}" status="i" var="datasetFrequencyInstance">
					<tr class="${(i % 2) == 0 ? 'even' : 'odd'}">
					
						<td><g:link action="show" id="${datasetFrequencyInstance.id}">${fieldValue(bean: datasetFrequencyInstance, field: "dataset")}</g:link></td>
					
						<td>${fieldValue(bean: datasetFrequencyInstance, field: "frequency")}</td>
					
					</tr>
				</g:each>
				</tbody>
			</table>
			<div class="pagination">
				<g:paginate total="${datasetFrequencyInstanceTotal}" />
			</div>
		</div>
	</body>
</html>
