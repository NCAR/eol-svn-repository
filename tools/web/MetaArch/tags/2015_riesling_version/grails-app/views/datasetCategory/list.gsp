


<%@ page import="meta.DatasetCategory" %>
<!doctype html>
<html>
	<head>
		<meta name="layout" content="main">
		<g:set var="entityName" value="${message(code: 'datasetCategory.label', default: 'DatasetCategory')}" />
		<title><g:message code="default.list.label" args="[entityName]" /></title>
	</head>
	<body>		
		<a href="#list-datasetCategory" class="skip" tabindex="-1"><g:message code="default.link.skip.label" default="Skip to content&hellip;"/></a>
		<div class="nav" role="navigation">
			<g:render template="nav"/>
		</div>
		<div id="list-datasetCategory" class="content scaffold-list" role="main">
			<h1><g:message code="default.list.label" args="[entityName]" /></h1>
			<g:if test="${flash.message}">
			<div class="message" role="status">${flash.message}</div>
			</g:if>
			<table>
				<thead>
					<tr>
					
						<g:sortableColumn property="category" title="${message(code: 'datasetCategory.category.label', default: 'Category')}" />
					
						<th><g:message code="datasetCategory.dataset.label" default="Dataset" /></th>
					
					</tr>
				</thead>
				<tbody>
				<g:each in="${datasetCategoryInstanceList}" status="i" var="datasetCategoryInstance">
					<tr class="${(i % 2) == 0 ? 'even' : 'odd'}">
					
						<td><g:link action="show" id="${datasetCategoryInstance.id}">${fieldValue(bean: datasetCategoryInstance, field: "category")}</g:link></td>
					
						<td>${fieldValue(bean: datasetCategoryInstance, field: "dataset")}</td>
					
					</tr>
				</g:each>
				</tbody>
			</table>
			<div class="pagination">
				<g:paginate total="${datasetCategoryInstanceTotal}" />
			</div>
		</div>
	</body>
</html>
