



<%@ page import="meta.ProjectMember" %>
<!doctype html>
<html>
	<head>
		<meta name="layout" content="main">
		<g:set var="entityName" value="${message(code: 'projectMember.label', default: 'ProjectMember')}" />
		<title><g:message code="default.show.label" args="[entityName]" /></title>
	</head>
	<body>
		<a href="#show-projectMember" class="skip" tabindex="-1"><g:message code="default.link.skip.label" default="Skip to content&hellip;"/></a>
		<div class="nav" role="navigation">
			<g:render template="nav"/>
		</div>
		<div id="show-projectMember" class="content scaffold-show" role="main">
			<h1><g:message code="default.show.label" args="[entityName]" /></h1>
			<g:if test="${flash.message}">
			<div class="message" role="status">${flash.message}</div>
			</g:if>
			<ol class="property-list projectMember">
			
				<g:if test="${projectMemberInstance?.member}">
				<li class="fieldcontain">
					<span id="member-label" class="property-label"><g:message code="projectMember.member.label" default="Member" /></span>
					
						<span class="property-value" aria-labelledby="member-label"><g:link controller="user" action="show" id="${projectMemberInstance?.member?.id}">${projectMemberInstance?.member?.encodeAsHTML()}</g:link></span>
					
				</li>
				</g:if>
			
				<g:if test="${projectMemberInstance?.memberType}">
				<li class="fieldcontain">
					<span id="memberType-label" class="property-label"><g:message code="projectMember.memberType.label" default="Member Type" /></span>
					
						<span class="property-value" aria-labelledby="memberType-label"><g:fieldValue bean="${projectMemberInstance}" field="memberType"/></span>
					
				</li>
				</g:if>
			
				<g:if test="${projectMemberInstance?.project}">
				<li class="fieldcontain">
					<span id="project-label" class="property-label"><g:message code="projectMember.project.label" default="Project" /></span>
					
						<span class="property-value" aria-labelledby="project-label"><g:link controller="project" action="show" id="${projectMemberInstance?.project?.id}">${projectMemberInstance?.project?.encodeAsHTML()}</g:link></span>
					
				</li>
				</g:if>
			
			</ol>
			<g:form>
				<fieldset class="buttons">
					<g:hiddenField name="id" value="${projectMemberInstance?.id}" />
					<g:link class="edit" action="edit" id="${projectMemberInstance?.id}"><g:message code="default.button.edit.label" default="Edit" /></g:link>
					<g:actionSubmit class="delete" action="delete" value="${message(code: 'default.button.delete.label', default: 'Delete')}" onclick="return confirm('${message(code: 'default.button.delete.confirm.message', default: 'Are you sure?')}');" />
				</fieldset>
			</g:form>
		</div>
	</body>
</html>
