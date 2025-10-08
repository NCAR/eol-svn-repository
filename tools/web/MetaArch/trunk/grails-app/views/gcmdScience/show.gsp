



<%@ page import="meta.GcmdScience" %>
<!doctype html>
<html>
	<head>
		<meta name="layout" content="main">
		<g:set var="entityName" value="${message(code: 'gcmdScience.label', default: 'GcmdScience')}" />
		<title><g:message code="default.show.label" args="[entityName]" /></title>
	</head>
	<body>
		<a href="#show-gcmdScience" class="skip" tabindex="-1"><g:message code="default.link.skip.label" default="Skip to content&hellip;"/></a>
		<div class="nav" role="navigation">
			<g:render template="nav"/>
		</div>
		<div id="show-gcmdScience" class="content scaffold-show" role="main">
			<h1><g:message code="default.show.label" args="[entityName]" /></h1>
			<g:if test="${flash.message}">
			<div class="message" role="status">${flash.message}</div>
			</g:if>
			<ol class="property-list gcmdScience">
			
				<g:if test="${gcmdScienceInstance?.keyword}">
				<li class="fieldcontain">
					<span id="keyword-label" class="property-label"><g:message code="gcmdScience.keyword.label" default="Keyword" /></span>
					
						<span class="property-value" aria-labelledby="keyword-label"><g:fieldValue bean="${gcmdScienceInstance}" field="keyword"/></span>
					
				</li>
				</g:if>
			
				<g:if test="${gcmdScienceInstance?.parent}">
				<li class="fieldcontain">
					<span id="parent-label" class="property-label"><g:message code="gcmdScience.parent.label" default="Parent" /></span>
					
						<span class="property-value" aria-labelledby="parent-label"><g:link controller="gcmdScience" action="show" id="${gcmdScienceInstance?.parent?.id}">${gcmdScienceInstance?.parent?.encodeAsHTML()}</g:link></span>
					
				</li>
				</g:if>
			
			</ol>
			<g:form>
				<fieldset class="buttons">
					<g:hiddenField name="id" value="${gcmdScienceInstance?.id}" />
					<g:link class="edit" action="edit" id="${gcmdScienceInstance?.id}"><g:message code="default.button.edit.label" default="Edit" /></g:link>
					<g:actionSubmit class="delete" action="delete" value="${message(code: 'default.button.delete.label', default: 'Delete')}" onclick="return confirm('${message(code: 'default.button.delete.confirm.message', default: 'Are you sure?')}');" />
				</fieldset>
			</g:form>
		</div>
	</body>
</html>
