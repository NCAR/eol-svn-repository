



<%@ page import="meta.VerticalResolution" %>
<!doctype html>
<html>
	<head>
		<meta name="layout" content="main">
		<g:set var="entityName" value="${message(code: 'verticalResolution.label', default: 'VerticalResolution')}" />
		<title><g:message code="default.show.label" args="[entityName]" /></title>
	</head>
	<body>
		<a href="#show-verticalResolution" class="skip" tabindex="-1"><g:message code="default.link.skip.label" default="Skip to content&hellip;"/></a>
		<div class="nav" role="navigation">
			<g:render template="nav"/>
		</div>
		<div id="show-verticalResolution" class="content scaffold-show" role="main">
			<h1><g:message code="default.show.label" args="[entityName]" /></h1>
			<g:if test="${flash.message}">
			<div class="message" role="status">${flash.message}</div>
			</g:if>
			<ol class="property-list verticalResolution">
			
				<g:if test="${verticalResolutionInstance?.scale}">
				<li class="fieldcontain">
					<span id="scale-label" class="property-label"><g:message code="verticalResolution.scale.label" default="Scale" /></span>
					
						<span class="property-value" aria-labelledby="scale-label"><g:fieldValue bean="${verticalResolutionInstance}" field="scale"/></span>
					
				</li>
				</g:if>
			
			</ol>
			<g:form>
				<fieldset class="buttons">
					<g:hiddenField name="id" value="${verticalResolutionInstance?.id}" />
					<g:link class="edit" action="edit" id="${verticalResolutionInstance?.id}"><g:message code="default.button.edit.label" default="Edit" /></g:link>
					<g:actionSubmit class="delete" action="delete" value="${message(code: 'default.button.delete.label', default: 'Delete')}" onclick="return confirm('${message(code: 'default.button.delete.confirm.message', default: 'Are you sure?')}');" />
				</fieldset>
			</g:form>
		</div>
	</body>
</html>
