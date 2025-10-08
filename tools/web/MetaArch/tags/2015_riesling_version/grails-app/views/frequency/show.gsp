



<%@ page import="meta.Frequency" %>
<!doctype html>
<html>
	<head>
		<meta name="layout" content="main">
		<g:set var="entityName" value="${message(code: 'frequency.label', default: 'Frequency')}" />
		<title><g:message code="default.show.label" args="[entityName]" /></title>
	</head>
	<body>
		<a href="#show-frequency" class="skip" tabindex="-1"><g:message code="default.link.skip.label" default="Skip to content&hellip;"/></a>
		<div class="nav" role="navigation">
			<g:render template="nav"/>
		</div>
		<div id="show-frequency" class="content scaffold-show" role="main">
			<h1><g:message code="default.show.label" args="[entityName]" /></h1>
			<g:if test="${flash.message}">
			<div class="message" role="status">${flash.message}</div>
			</g:if>
			<ol class="property-list frequency">
			
				<g:if test="${frequencyInstance?.name}">
				<li class="fieldcontain">
					<span id="name-label" class="property-label"><g:message code="frequency.name.label" default="Name" /></span>
					
						<span class="property-value" aria-labelledby="name-label"><g:fieldValue bean="${frequencyInstance}" field="name"/></span>
					
				</li>
				</g:if>
			
				<g:if test="${frequencyInstance?.sortKey}">
				<li class="fieldcontain">
					<span id="sortKey-label" class="property-label"><g:message code="frequency.sortKey.label" default="Sort Key" /></span>
					
						<span class="property-value" aria-labelledby="sortKey-label"><g:fieldValue bean="${frequencyInstance}" field="sortKey"/></span>
					
				</li>
				</g:if>
			
			</ol>
			<g:form>
				<fieldset class="buttons">
					<g:hiddenField name="id" value="${frequencyInstance?.id}" />
					<g:link class="edit" action="edit" id="${frequencyInstance?.id}"><g:message code="default.button.edit.label" default="Edit" /></g:link>
					<g:actionSubmit class="delete" action="delete" value="${message(code: 'default.button.delete.label', default: 'Delete')}" onclick="return confirm('${message(code: 'default.button.delete.confirm.message', default: 'Are you sure?')}');" />
				</fieldset>
			</g:form>
		</div>
	</body>
</html>
