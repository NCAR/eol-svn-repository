


<%@ page import="meta.File" %>
<!doctype html>
<html>
	<head>
		<meta name="layout" content="main">
		<g:set var="entityName" value="${message(code: 'file.label', default: 'File')}" />
		<title><g:message code="default.list.label" args="[entityName]" /></title>
	</head>
	<body>		
		<a href="#list-file" class="skip" tabindex="-1"><g:message code="default.link.skip.label" default="Skip to content&hellip;"/></a>
		<div class="nav" role="navigation">
			<g:render template="nav"/>
		</div>
		<div id="list-file" class="content scaffold-list" role="main">
			<h1><g:message code="default.list.label" args="[entityName]" /></h1>
			<g:if test="${flash.message}">
			<div class="message" role="status">${flash.message}</div>
			</g:if>
			<table>
				<thead>
					<tr>
					
						<g:sortableColumn property="index" title="${message(code: 'file.index.label', default: 'Index')}" />
					
						<g:sortableColumn property="name" title="${message(code: 'file.name.label', default: 'Name')}" />
					
						<g:sortableColumn property="directory" title="${message(code: 'file.directory.label', default: 'Directory')}" />
					
						<g:sortableColumn property="fileType" title="${message(code: 'file.fileType.label', default: 'File Type')}" />
					
						<th><g:message code="file.format.label" default="Format" /></th>
					
						<g:sortableColumn property="size" title="${message(code: 'file.size.label', default: 'Size')}" />
					
					</tr>
				</thead>
				<tbody>
				<g:each in="${fileInstanceList}" status="i" var="fileInstance">
					<tr class="${(i % 2) == 0 ? 'even' : 'odd'}">
					
						<td><g:link action="show" id="${fileInstance.id}">${fieldValue(bean: fileInstance, field: "index")}</g:link></td>
					
						<td>${fieldValue(bean: fileInstance, field: "name")}</td>
					
						<td>${fieldValue(bean: fileInstance, field: "directory")}</td>
					
						<td>${fieldValue(bean: fileInstance, field: "fileType")}</td>
					
						<td>${fieldValue(bean: fileInstance, field: "format")}</td>
					
						<td>${fieldValue(bean: fileInstance, field: "size")}</td>
					
					</tr>
				</g:each>
				</tbody>
			</table>
			<div class="pagination">
				<g:paginate total="${fileInstanceTotal}" />
			</div>
		</div>
	</body>
</html>
