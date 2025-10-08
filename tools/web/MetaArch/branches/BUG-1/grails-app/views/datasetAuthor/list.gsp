


<%@ page import="meta.DatasetAuthor" %>
<!doctype html>
<html>
	<head>
		<meta name="layout" content="main">
		<g:set var="entityName" value="${message(code: 'datasetAuthor.label', default: 'DatasetAuthor')}" />
		<title><g:message code="default.list.label" args="[entityName]" /></title>
	</head>
	<body>		
		<a href="#list-datasetAuthor" class="skip" tabindex="-1"><g:message code="default.link.skip.label" default="Skip to content&hellip;"/></a>
		<div class="nav" role="navigation">
			<g:render template="nav"/>
		</div>
		<div id="list-datasetAuthor" class="content scaffold-list" role="main">
			<h1><g:message code="default.list.label" args="[entityName]" /></h1>
			<g:if test="${flash.message}">
			<div class="message" role="status">${flash.message}</div>
			</g:if>
			<table>
				<thead>
					<tr>
					
						<th><g:message code="datasetAuthor.author.label" default="Author" /></th>
					
						<th><g:message code="datasetAuthor.dataset.label" default="Dataset" /></th>
					
						<g:sortableColumn property="sortKey" title="${message(code: 'datasetAuthor.sortKey.label', default: 'Sort Key')}" />
					
					</tr>
				</thead>
				<tbody>
				<g:each in="${datasetAuthorInstanceList}" status="i" var="datasetAuthorInstance">
					<tr class="${(i % 2) == 0 ? 'even' : 'odd'}">
					
						<td><g:link action="show" id="${datasetAuthorInstance.id}">${fieldValue(bean: datasetAuthorInstance, field: "author")}</g:link></td>
					
						<td>${fieldValue(bean: datasetAuthorInstance, field: "dataset")}</td>
					
						<td>${fieldValue(bean: datasetAuthorInstance, field: "sortKey")}</td>
					
					</tr>
				</g:each>
				</tbody>
			</table>
			<div class="pagination">
				<g:paginate total="${datasetAuthorInstanceTotal}" />
			</div>
		</div>
	</body>
</html>
