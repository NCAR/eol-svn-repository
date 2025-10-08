



<%@ page import="meta.Xlink" %>
<!doctype html>
<html>
	<head>
		<meta name="layout" content="main">
		<g:set var="entityName" value="${message(code: 'xlink.label', default: 'Xlink')}" />
		<title><g:message code="default.show.label" args="[entityName]" /></title>
	</head>
	<body>
		<a href="#show-xlink" class="skip" tabindex="-1"><g:message code="default.link.skip.label" default="Skip to content&hellip;"/></a>
		<div class="nav" role="navigation">
			<g:render template="nav"/>
		</div>
		<div id="show-xlink" class="content scaffold-show" role="main">
			<h1><g:message code="default.show.label" args="[entityName]" /></h1>
			<g:if test="${flash.message}">
			<div class="message" role="status">${flash.message}</div>
			</g:if>
			<ol class="property-list xlink">
			
				<g:if test="${xlinkInstance?.index}">
				<li class="fieldcontain">
					<span id="index-label" class="property-label"><g:message code="xlink.index.label" default="Index" /></span>
					
						<span class="property-value" aria-labelledby="index-label"><g:fieldValue bean="${xlinkInstance}" field="index"/></span>
					
				</li>
				</g:if>
			
				<g:if test="${xlinkInstance?.title}">
				<li class="fieldcontain">
					<span id="title-label" class="property-label"><g:message code="xlink.title.label" default="Title" /></span>
					
						<span class="property-value" aria-labelledby="title-label"><g:fieldValue bean="${xlinkInstance}" field="title"/></span>
					
				</li>
				</g:if>
			
				<g:if test="${xlinkInstance?.type}">
				<li class="fieldcontain">
					<span id="type-label" class="property-label"><g:message code="xlink.type.label" default="Type" /></span>
					
						<span class="property-value" aria-labelledby="type-label"><g:fieldValue bean="${xlinkInstance}" field="type"/></span>
					
				</li>
				</g:if>
			
				<g:if test="${xlinkInstance?.href}">
				<li class="fieldcontain">
					<span id="href-label" class="property-label"><g:message code="xlink.href.label" default="Href" /></span>
					
						<span class="property-value" aria-labelledby="href-label"><g:fieldValue bean="${xlinkInstance}" field="href"/></span>
					
				</li>
				</g:if>
			
				<g:if test="${xlinkInstance?.dataset}">
				<li class="fieldcontain">
					<span id="dataset-label" class="property-label"><g:message code="xlink.dataset.label" default="Dataset" /></span>
					
						<span class="property-value" aria-labelledby="dataset-label"><g:link controller="dataset" action="show" id="${xlinkInstance?.dataset?.id}">${xlinkInstance?.dataset?.encodeAsHTML()}</g:link></span>
					
				</li>
				</g:if>
			
				<g:if test="${xlinkInstance?.dateCreated}">
				<li class="fieldcontain">
					<span id="dateCreated-label" class="property-label"><g:message code="xlink.dateCreated.label" default="Date Created" /></span>
					
						<span class="property-value" aria-labelledby="dateCreated-label"><g:formatDate date="${xlinkInstance?.dateCreated}" /></span>
					
				</li>
				</g:if>
			
				<g:if test="${xlinkInstance?.lastUpdated}">
				<li class="fieldcontain">
					<span id="lastUpdated-label" class="property-label"><g:message code="xlink.lastUpdated.label" default="Last Updated" /></span>
					
						<span class="property-value" aria-labelledby="lastUpdated-label"><g:formatDate date="${xlinkInstance?.lastUpdated}" /></span>
					
				</li>
				</g:if>
			
			</ol>
			<g:form>
				<fieldset class="buttons">
					<g:hiddenField name="id" value="${xlinkInstance?.id}" />
					<g:link class="edit" action="edit" id="${xlinkInstance?.id}"><g:message code="default.button.edit.label" default="Edit" /></g:link>
					<g:actionSubmit class="delete" action="delete" value="${message(code: 'default.button.delete.label', default: 'Delete')}" onclick="return confirm('${message(code: 'default.button.delete.confirm.message', default: 'Are you sure?')}');" />
				</fieldset>
			</g:form>
		</div>
	</body>
</html>
