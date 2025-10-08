


<%@ page import="meta.XmlTemplate" %>
<!doctype html>
<html>
	<head>
		<meta name="layout" content="main">
		<g:set var="entityName" value="${message(code: 'xmlTemplate.label', default: 'XmlTemplate')}" />
		<title><g:message code="default.list.label" args="[entityName]" /></title>
	</head>
	<body>		
		<a href="#list-xmlTemplate" class="skip" tabindex="-1"><g:message code="default.link.skip.label" default="Skip to content&hellip;"/></a>
		<div class="nav" role="navigation">
			<g:render template="nav"/>
		</div>
		<div id="list-xmlTemplate" class="content scaffold-list" role="main">
			<h1><g:message code="default.list.label" args="[entityName]" /></h1>
			<g:if test="${flash.message}">
			<div class="message" role="status">${flash.message}</div>
			</g:if>
			<table>
				<thead>
					<tr>
					
						<g:sortableColumn property="filename" title="${message(code: 'xmlTemplate.filename.label', default: 'Filename')}" />
					
						<g:sortableColumn property="body" title="${message(code: 'xmlTemplate.body.label', default: 'Body')}" />
					
						<g:sortableColumn property="size" title="${message(code: 'xmlTemplate.size.label', default: 'Size')}" />
					
						<g:sortableColumn property="dateCreated" title="${message(code: 'xmlTemplate.dateCreated.label', default: 'Date Created')}" />
					
						<g:sortableColumn property="lastUpdated" title="${message(code: 'xmlTemplate.lastUpdated.label', default: 'Last Updated')}" />
					
					</tr>
				</thead>
				<tbody>
				<g:each in="${xmlTemplateInstanceList}" status="i" var="xmlTemplateInstance">
					<tr class="${(i % 2) == 0 ? 'even' : 'odd'}">
					
						<td><g:link action="show" id="${xmlTemplateInstance.id}">${fieldValue(bean: xmlTemplateInstance, field: "filename")}</g:link></td>
					
						<td>${fieldValue(bean: xmlTemplateInstance, field: "body")}</td>
					
						<td>${fieldValue(bean: xmlTemplateInstance, field: "size")}</td>
					
						<td><g:formatDate date="${xmlTemplateInstance.dateCreated}" /></td>
					
						<td><g:formatDate date="${xmlTemplateInstance.lastUpdated}" /></td>
					
					</tr>
				</g:each>
				</tbody>
			</table>
			<div class="pagination">
				<g:paginate total="${xmlTemplateInstanceTotal}" />
			</div>
		</div>
	</body>
</html>
