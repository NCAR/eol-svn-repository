



<%@ page import="meta.auth.User" %>
<!doctype html>
<html>
	<head>
		<meta name="layout" content="main">
		<g:set var="entityName" value="${message(code: 'user.label', default: 'User')}" />
		<title><g:message code="default.show.label" args="[entityName]" /></title>
	</head>
	<body>
		<a href="#show-user" class="skip" tabindex="-1"><g:message code="default.link.skip.label" default="Skip to content&hellip;"/></a>
		<div class="nav" role="navigation">
			<g:render template="nav"/>
		</div>
		<div id="show-user" class="content scaffold-show" role="main">
			<h1><g:message code="default.show.label" args="[entityName]" /></h1>
			<g:if test="${flash.message}">
			<div class="message" role="status">${flash.message}</div>
			</g:if>
			<ol class="property-list user">
			
				<g:if test="${userInstance?.username}">
				<li class="fieldcontain">
					<span id="username-label" class="property-label"><g:message code="user.username.label" default="Username" /></span>
					
						<span class="property-value" aria-labelledby="username-label"><g:fieldValue bean="${userInstance}" field="username"/></span>
					
				</li>
				</g:if>
			
				<g:if test="${userInstance?.realname}">
				<li class="fieldcontain">
					<span id="realname-label" class="property-label"><g:message code="user.realname.label" default="Full Name" /></span>
					
						<span class="property-value" aria-labelledby="realname-label"><g:fieldValue bean="${userInstance}" field="realname"/></span>
					
				</li>
				</g:if>
			
<%--				<g:if test="${userInstance?.password}">--%>
<%--				<li class="fieldcontain">--%>
<%--					<span id="password-label" class="property-label"><g:message code="user.password.label" default="Password" /></span>--%>
<%--					--%>
<%--						<span class="property-value" aria-labelledby="password-label"><g:fieldValue bean="${userInstance}" field="password"/></span>--%>
<%--					--%>
<%--				</li>--%>
<%--				</g:if>--%>
			
				<g:if test="${userInstance?.email && userInstance?.email_show == true}">
				<li class="fieldcontain">
					<span id="email-label" class="property-label"><g:message code="user.email.label" default="Email" /></span>
					
						<span class="property-value" aria-labelledby="email-label"><g:fieldValue bean="${userInstance}" field="email"/></span>
					
				</li>
				</g:if>
			
				<g:if test="${userInstance?.phoneNumber}">
				<li class="fieldcontain">
					<span id="phoneNumber-label" class="property-label"><g:message code="user.phoneNumber.label" default="Phone Number" /></span>
					
						<span class="property-value" aria-labelledby="phoneNumber-label"><g:fieldValue bean="${userInstance}" field="phoneNumber"/></span>
					
				</li>
				</g:if>
			
				<g:if test="${userInstance?.organization}">
				<li class="fieldcontain">
					<span id="organization-label" class="property-label"><g:message code="user.organization.label" default="Organization" /></span>
					
						<span class="property-value" aria-labelledby="organization-label"><g:fieldValue bean="${userInstance}" field="organization"/></span>
					
				</li>
				</g:if>
			
				<g:if test="${userInstance?.defaultProject != null && userInstance.projects() != null}">
				<li class="fieldcontain">
					<span id="defaultProject-label" class="property-label"><g:message code="user.defaultProject.label" default="Default Project" /></span>
					
						<span class="property-value" aria-labelledby="defaultProject-label"><g:link controller="project" action="show" id="${userInstance?.defaultProject?.id}">${userInstance?.defaultProject?.encodeAsHTML()}</g:link></span>
					
				</li>
				</g:if>
			
				<sec:ifAnyGranted roles="ROLE_ADMIN,ROLE_DEVELOP">
				<g:if test="${userInstance?.accountExpired}">
				<li class="fieldcontain">
					<span id="accountExpired-label" class="property-label"><g:message code="user.accountExpired.label" default="Account Expired" /></span>
					
						<span class="property-value" aria-labelledby="accountExpired-label"><g:formatBoolean boolean="${userInstance?.accountExpired}" /></span>
					
				</li>
				</g:if>
			
				<g:if test="${userInstance?.accountLocked}">
				<li class="fieldcontain">
					<span id="accountLocked-label" class="property-label"><g:message code="user.accountLocked.label" default="Account Locked" /></span>
					
						<span class="property-value" aria-labelledby="accountLocked-label"><g:formatBoolean boolean="${userInstance?.accountLocked}" /></span>
					
				</li>
				</g:if>
			
				<g:if test="${userInstance?.email_show}">
				<li class="fieldcontain">
					<span id="email_show-label" class="property-label"><g:message code="user.email_show.label" default="Emailshow" /></span>
					
						<span class="property-value" aria-labelledby="email_show-label"><g:formatBoolean boolean="${userInstance?.email_show}" /></span>
					
				</li>
				</g:if>
			
				<g:if test="${userInstance?.enabled}">
				<li class="fieldcontain">
					<span id="enabled-label" class="property-label"><g:message code="user.enabled.label" default="Enabled" /></span>
					
						<span class="property-value" aria-labelledby="enabled-label"><g:formatBoolean boolean="${userInstance?.enabled}" /></span>
					
				</li>
				</g:if>
			
				<g:if test="${userInstance?.isPasswordEncoded}">
				<li class="fieldcontain">
					<span id="isPasswordEncoded-label" class="property-label"><g:message code="user.isPasswordEncoded.label" default="Is Password Encoded" /></span>
					
						<span class="property-value" aria-labelledby="isPasswordEncoded-label"><g:formatBoolean boolean="${userInstance?.isPasswordEncoded}" /></span>
					
				</li>
				</g:if>
			
				<g:if test="${userInstance?.passwordExpired}">
				<li class="fieldcontain">
					<span id="passwordExpired-label" class="property-label"><g:message code="user.passwordExpired.label" default="Password Expired" /></span>
					
						<span class="property-value" aria-labelledby="passwordExpired-label"><g:formatBoolean boolean="${userInstance?.passwordExpired}" /></span>
					
				</li>
				</g:if>
				</sec:ifAnyGranted>
			
				<g:if test="${userInstance?.projectGroups != null && userInstance?.projects() != null}">
				<li class="fieldcontain">
					<span id="projectGroups-label" class="property-label"><g:message code="user.projectGroups.label" default="Project Groups" /></span>
					
						<g:each in="${userInstance?.projects()}" var="p">
						<span class="property-value" aria-labelledby="projectGroups-label"><g:link controller="project" action="show" id="${p.id}">${p?.encodeAsHTML()}</g:link></span>
						</g:each>
					
				</li>
				</g:if>
			
			</ol>
			<g:form>
				<fieldset class="buttons">
					<g:hiddenField name="id" value="${userInstance?.id}" />
					
					<sec:ifAnyGranted roles="ROLE_ADMIN,ROLE_DEVELOP">
					<g:if test="${(userInstance?.accountLocked == true) || (userInstance?.enabled == false)}">
						<g:link class="edit" action="approveUserReg" id="${userInstance?.id}">Approve Registration Request</g:link>
					</g:if>
					</sec:ifAnyGranted>
					
					<g:link class="edit" action="edit" id="${userInstance?.id}"><g:message code="default.button.edit.label" default="Edit" /></g:link>
					<g:actionSubmit class="delete" action="delete" value="${message(code: 'default.button.delete.label', default: 'Delete')}" onclick="return confirm('${message(code: 'default.button.delete.confirm.message', default: 'Are you sure?')}');" />
				</fieldset>
			</g:form>
		</div>
	</body>
</html>
