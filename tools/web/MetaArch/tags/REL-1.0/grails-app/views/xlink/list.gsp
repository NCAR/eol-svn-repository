


<%@ page import="meta.Xlink" %>
<!doctype html>
<html>
	<head>
		<meta name="layout" content="main">
		<g:set var="entityName" value="${message(code: 'xlink.label', default: 'Xlink')}" />
		<title><g:message code="default.list.label" args="[entityName]" /></title>
	</head>
	<body>		
		<a href="#list-xlink" class="skip" tabindex="-1"><g:message code="default.link.skip.label" default="Skip to content&hellip;"/></a>
		<div class="nav" role="navigation">
			<g:render template="nav"/>
		</div>
		<div id="list-xlink" class="content scaffold-list" role="main">
			<h1><g:message code="default.list.label" args="[entityName]" /></h1>
			<g:if test="${flash.message}">
			<div class="message" role="status">${flash.message}</div>
			</g:if>
			<table>
				<thead>
					<tr>
					
						<g:sortableColumn property="index" title="${message(code: 'xlink.index.label', default: 'Index')}" />
					
						<g:sortableColumn property="title" title="${message(code: 'xlink.title.label', default: 'Title')}" />
					
						<g:sortableColumn property="type" title="${message(code: 'xlink.type.label', default: 'Type')}" />
					
						<g:sortableColumn property="href" title="${message(code: 'xlink.href.label', default: 'Href')}" />
					
						<th><g:message code="xlink.dataset.label" default="Dataset" /></th>
					
						<g:sortableColumn property="dateCreated" title="${message(code: 'xlink.dateCreated.label', default: 'Date Created')}" />
					
					</tr>
				</thead>
				<tbody>
				<g:each in="${xlinkInstanceList}" status="i" var="xlinkInstance">
					<tr class="${(i % 2) == 0 ? 'even' : 'odd'}">
					
						<td><g:link action="show" id="${xlinkInstance.id}">${fieldValue(bean: xlinkInstance, field: "index")}</g:link></td>
					
						<td>${fieldValue(bean: xlinkInstance, field: "title")}</td>
					
						<td>${fieldValue(bean: xlinkInstance, field: "type")}</td>
					
						<td>${fieldValue(bean: xlinkInstance, field: "href")}</td>
					
						<td>${fieldValue(bean: xlinkInstance, field: "dataset")}</td>
					
						<td><g:formatDate date="${xlinkInstance.dateCreated}" /></td>
					
					</tr>
				</g:each>
				</tbody>
			</table>
			<div class="pagination">
				<g:paginate total="${xlinkInstanceTotal}" />
			</div>
		</div>
	</body>
</html>
