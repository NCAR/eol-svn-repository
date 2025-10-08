



<%@ page import="meta.File" %>
<!doctype html>
<html>
	<head>
		<meta name="layout" content="main">
		<g:set var="entityName" value="${message(code: 'file.label', default: 'File')}" />
		<title><g:message code="default.show.label" args="[entityName]" /></title>
	</head>
	<body>
		<a href="#show-file" class="skip" tabindex="-1"><g:message code="default.link.skip.label" default="Skip to content&hellip;"/></a>
		<div class="nav" role="navigation">
			<g:render template="nav"/>
		</div>
		<div id="show-file" class="content scaffold-show" role="main">
			<h1><g:message code="default.show.label" args="[entityName]" /></h1>
			<g:if test="${flash.message}">
			<div class="message" role="status">${flash.message}</div>
			</g:if>
			<ol class="property-list file">
			
				<g:if test="${fileInstance?.name}">
				<li class="fieldcontain">
					<span id="name-label" class="property-label"><g:message code="file.name.label" default="Name" /></span>
					
						<span class="property-value" aria-labelledby="name-label"><g:fieldValue bean="${fileInstance}" field="name"/></span>
					
				</li>
				</g:if>
			
				<g:if test="${fileInstance?.fileType}">
				<li class="fieldcontain">
					<span id="fileType-label" class="property-label"><g:message code="file.fileType.label" default="File Type" /></span>
					
						<span class="property-value" aria-labelledby="fileType-label"><g:fieldValue bean="${fileInstance}" field="fileType"/></span>
					
				</li>
				</g:if>
			
				<g:if test="${fileInstance?.format}">
				<li class="fieldcontain">
					<span id="format-label" class="property-label"><g:message code="file.format.label" default="Format" /></span>
					
						<span class="property-value" aria-labelledby="format-label"><g:link controller="format" action="show" id="${fileInstance?.format?.id}">${fileInstance?.format?.encodeAsHTML()}</g:link></span>
					
				</li>
				</g:if>
			
				<g:if test="${fileInstance?.size}">
				<li class="fieldcontain">
					<span id="size-label" class="property-label"><g:message code="file.size.label" default="Size" /></span>
					
						<span class="property-value" aria-labelledby="size-label"><g:fieldValue bean="${fileInstance}" field="size"/> kB</span>
					
				</li>
				</g:if>
			
				<g:if test="${fileInstance?.dateCreated}">
				<li class="fieldcontain">
					<span id="dateCreated-label" class="property-label"><g:message code="file.dateCreated.label" default="Date Created" /></span>
					
						<span class="property-value" aria-labelledby="dateCreated-label"><g:formatDate date="${fileInstance?.dateCreated}" /></span>
					
				</li>
				</g:if>
			
				<g:if test="${fileInstance?.lastUpdated}">
				<li class="fieldcontain">
					<span id="lastUpdated-label" class="property-label"><g:message code="file.lastUpdated.label" default="Last Updated" /></span>
					
						<span class="property-value" aria-labelledby="lastUpdated-label"><g:formatDate date="${fileInstance?.lastUpdated}" /></span>
					
				</li>
				</g:if>
			
			</ol>
			<g:form>
				<fieldset class="buttons">
					<g:hiddenField name="id" value="${fileInstance?.id}" />
					<g:link class="edit" action="edit" id="${fileInstance?.id}"><g:message code="default.button.edit.label" default="Edit" /></g:link>
					<g:actionSubmit class="delete" action="delete" value="${message(code: 'default.button.delete.label', default: 'Delete')}" onclick="return confirm('${message(code: 'default.button.delete.confirm.message', default: 'Are you sure?')}');" />
				</fieldset>
			</g:form>
		</div>
	</body>
</html>
