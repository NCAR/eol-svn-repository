


<%@ page import="meta.IsoTopic" %>
<!doctype html>
<html>
	<head>
		<meta name="layout" content="main">
		<g:set var="entityName" value="${message(code: 'isoTopic.label', default: 'IsoTopic')}" />
		<title><g:message code="default.list.label" args="[entityName]" /></title>
	</head>
	<body>		
		<a href="#list-isoTopic" class="skip" tabindex="-1"><g:message code="default.link.skip.label" default="Skip to content&hellip;"/></a>
		<div class="nav" role="navigation">
			<g:render template="nav"/>
		</div>
		<div id="list-isoTopic" class="content scaffold-list" role="main">
			<h1><g:message code="default.list.label" args="[entityName]" /></h1>
			<g:if test="${flash.message}">
			<div class="message" role="status">${flash.message}</div>
			</g:if>
			<table>
				<thead>
					<tr>
					
						<g:sortableColumn property="keyword" title="${message(code: 'isoTopic.keyword.label', default: 'Keyword')}" />
					
					</tr>
				</thead>
				<tbody>
				<g:each in="${isoTopicInstanceList}" status="i" var="isoTopicInstance">
					<tr class="${(i % 2) == 0 ? 'even' : 'odd'}">
					
						<td><g:link action="show" id="${isoTopicInstance.id}">${fieldValue(bean: isoTopicInstance, field: "keyword")}</g:link></td>
					
					</tr>
				</g:each>
				</tbody>
			</table>
			<div class="pagination">
				<g:paginate total="${isoTopicInstanceTotal}" />
			</div>
		</div>
	</body>
</html>
