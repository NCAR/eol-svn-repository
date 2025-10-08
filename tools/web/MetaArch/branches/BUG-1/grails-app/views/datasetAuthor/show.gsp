



<%@ page import="meta.DatasetAuthor" %>
<!doctype html>
<html>
	<head>
		<meta name="layout" content="main">
		<g:set var="entityName" value="${message(code: 'datasetAuthor.label', default: 'DatasetAuthor')}" />
		<title><g:message code="default.show.label" args="[entityName]" /></title>
	</head>
	<body>
		<a href="#show-datasetAuthor" class="skip" tabindex="-1"><g:message code="default.link.skip.label" default="Skip to content&hellip;"/></a>
		<div class="nav" role="navigation">
			<g:render template="nav"/>
		</div>
		<div id="show-datasetAuthor" class="content scaffold-show" role="main">
			<h1><g:message code="default.show.label" args="[entityName]" /></h1>
			<g:if test="${flash.message}">
			<div class="message" role="status">${flash.message}</div>
			</g:if>
			<ol class="property-list datasetAuthor">
			
				<g:if test="${datasetAuthorInstance?.author}">
				<li class="fieldcontain">
					<span id="author-label" class="property-label"><g:message code="datasetAuthor.author.label" default="Author" /></span>
					
						<span class="property-value" aria-labelledby="author-label"><g:link controller="author" action="show" id="${datasetAuthorInstance?.author?.id}">${datasetAuthorInstance?.author?.encodeAsHTML()}</g:link></span>
					
				</li>
				</g:if>
			
				<g:if test="${datasetAuthorInstance?.dataset}">
				<li class="fieldcontain">
					<span id="dataset-label" class="property-label"><g:message code="datasetAuthor.dataset.label" default="Dataset" /></span>
					
						<span class="property-value" aria-labelledby="dataset-label"><g:link controller="dataset" action="show" id="${datasetAuthorInstance?.dataset?.id}">${datasetAuthorInstance?.dataset?.encodeAsHTML()}</g:link></span>
					
				</li>
				</g:if>
			
				<g:if test="${datasetAuthorInstance?.sortKey}">
				<li class="fieldcontain">
					<span id="sortKey-label" class="property-label"><g:message code="datasetAuthor.sortKey.label" default="Sort Key" /></span>
					
						<span class="property-value" aria-labelledby="sortKey-label"><g:fieldValue bean="${datasetAuthorInstance}" field="sortKey"/></span>
					
				</li>
				</g:if>
			
			</ol>
			<g:form>
				<fieldset class="buttons">
					<g:hiddenField name="id" value="${datasetAuthorInstance?.id}" />
					<g:link class="edit" action="edit" id="${datasetAuthorInstance?.id}"><g:message code="default.button.edit.label" default="Edit" /></g:link>
					<g:actionSubmit class="delete" action="delete" value="${message(code: 'default.button.delete.label', default: 'Delete')}" onclick="return confirm('${message(code: 'default.button.delete.confirm.message', default: 'Are you sure?')}');" />
				</fieldset>
			</g:form>
		</div>
	</body>
</html>
