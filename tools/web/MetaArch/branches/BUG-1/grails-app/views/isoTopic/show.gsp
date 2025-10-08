



<%@ page import="meta.IsoTopic" %>
<!doctype html>
<html>
	<head>
		<meta name="layout" content="main">
		<g:set var="entityName" value="${message(code: 'isoTopic.label', default: 'IsoTopic')}" />
		<title><g:message code="default.show.label" args="[entityName]" /></title>
	</head>
	<body>
		<a href="#show-isoTopic" class="skip" tabindex="-1"><g:message code="default.link.skip.label" default="Skip to content&hellip;"/></a>
		<div class="nav" role="navigation">
			<g:render template="nav"/>
		</div>
		<div id="show-isoTopic" class="content scaffold-show" role="main">
			<h1><g:message code="default.show.label" args="[entityName]" /></h1>
			<g:if test="${flash.message}">
			<div class="message" role="status">${flash.message}</div>
			</g:if>
			<ol class="property-list isoTopic">
			
				<g:if test="${isoTopicInstance?.keyword}">
				<li class="fieldcontain">
					<span id="keyword-label" class="property-label"><g:message code="isoTopic.keyword.label" default="Keyword" /></span>
					
						<span class="property-value" aria-labelledby="keyword-label"><g:fieldValue bean="${isoTopicInstance}" field="keyword"/></span>
					
				</li>
				</g:if>
			
			</ol>
			<g:form>
				<fieldset class="buttons">
					<g:hiddenField name="id" value="${isoTopicInstance?.id}" />
					<g:link class="edit" action="edit" id="${isoTopicInstance?.id}"><g:message code="default.button.edit.label" default="Edit" /></g:link>
					<g:actionSubmit class="delete" action="delete" value="${message(code: 'default.button.delete.label', default: 'Delete')}" onclick="return confirm('${message(code: 'default.button.delete.confirm.message', default: 'Are you sure?')}');" />
				</fieldset>
			</g:form>
		</div>
	</body>
</html>
