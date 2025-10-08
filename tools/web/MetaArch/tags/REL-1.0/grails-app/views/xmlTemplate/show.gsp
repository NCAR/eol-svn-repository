<!doctype html>
<html>
	<head>
		<meta name="layout" content="main">
		<g:set var="entityName" value="${message(code: 'XmlTemplate.label', default: 'XmlTemplate')}" />
		<title><g:message code="default.show.label" args="[entityName]" /></title>
	</head>
	<body>
		<a href="#show-XmlTemplate" class="skip" tabindex="-1"><g:message code="default.link.skip.label" default="Skip to content&hellip;"/></a>
		<div class="nav" role="navigation">
			<g:render template="nav"/>
		</div>
		<div id="show-XmlTemplate" class="content scaffold-show" role="main">
			<h1><g:message code="default.show.label" args="[entityName]" /></h1>
			<g:if test="${flash.message}">
			<div class="message" role="status">${flash.message}</div>
			</g:if>
			<ol class="property-list XmlTemplate">
			
				<g:if test="${xmlTemplateInstance?.filename}">
				<li class="fieldcontain">
					<span id="filename-label" class="property-label"><g:message code="xmlTemplate.filename.label" default="Filename" /></span>
					
						<span class="property-value" aria-labelledby="filename-label"><g:fieldValue bean="${xmlTemplateInstance}" field="filename"/></span>
					
				</li>
				</g:if>
				
				<g:if test="${xmlTemplateInstance?.body}">
				<li class="fieldcontain">
					<span id="body-label" class="property-label"><g:message code="xmlTemplate.body.label" default="Body" /></span>
					
						<span class="property-value" aria-labelledby="body-label"><pre><g:fieldValue bean="${xmlTemplateInstance}" field="body"/></pre></span>
					
				</li>
				</g:if>
				
				<g:if test="${xmlTemplateInstance?.file && !xmlTemplateInstance?.body}">
					<li class="fieldcontain">
						<span id="file-label" class="property-label"><g:message code="xmlTemplate.file.label" default="File" /></span>
						
							<span class="property-value" aria-labelledby="file-label"><g:xmlFile id="${xmlTemplateInstance.id}" /></span>
						
					</li>
				</g:if>
				
				<g:if test="${xmlTemplateInstance?.size}">
				<li class="fieldcontain">
					<span id="size-label" class="property-label"><g:message code="xmlTemplate.size.label" default="Size" /></span>
					
						<span class="property-value" aria-labelledby="size-label"><g:fieldValue bean="${xmlTemplateInstance}" field="size"/> bytes</span>
					
				</li>
				</g:if>
				
			</ol>
			<g:form>
				<fieldset class="buttons">
					<g:hiddenField name="id" value="${xmlTemplateInstance?.id}" />
					<g:link class="edit" action="edit" id="${xmlTemplateInstance?.id}"><g:message code="default.button.edit.label" default="Edit" /></g:link>
					<g:actionSubmit class="delete" action="delete" value="${message(code: 'default.button.delete.label', default: 'Delete')}" onclick="return confirm('${message(code: 'default.button.delete.confirm.message', default: 'Are you sure?')}');" />
					<g:link class="download" action="download" id="${xmlTemplateInstance?.id}">
						<img id="download-arrow" src="${resource(dir: 'images', file: 'download_arrow.png')}" alt="" />
						Download XML
					</g:link>
				</fieldset>
			</g:form>
		</div>
	</body>
</html>