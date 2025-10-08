<%@ page import="meta.auth.User" %>


<sec:ifAnyGranted roles="ROLE_ADMIN,ROLE_DEVELOP">
<div class="fieldcontain ${hasErrors(bean: userInstance, field: 'username', 'error')} required">
	<label for="username">
		<g:message code="user.username.label" default="Username" />
		<span class="required-indicator">*</span>
	</label>
	<g:textField name="username" required="" value="${userInstance?.username}"/>
</div>
</sec:ifAnyGranted>

<div class="fieldcontain ${hasErrors(bean: userInstance, field: 'email', 'error')} ">
	<label for="email">
		<g:message code="user.email.label" default="Email" />
		
	</label>
	<g:field type="email" name="email" value="${userInstance?.email}"/>
</div>

<div class="fieldcontain ${hasErrors(bean: userInstance, field: 'realname', 'error')} ">
	<label for="realname">
		<g:message code="user.realname.label" default="Full Name" />
		
	</label>
	<g:textField name="realname" value="${userInstance?.realname}"/>
</div>

<g:if test="${actionName == 'create'}">
<div class="fieldcontain ${hasErrors(bean: userInstance, field: 'password', 'error')} ">
	<label for="password">
		<g:message code="user.password.label" default="Password" />
	</label>
    <g:passwordField name='password' class='text_' />
</div>
</g:if>

<div class="fieldcontain ${hasErrors(bean: userInstance, field: 'phoneNumber', 'error')} ">
	<label for="phoneNumber">
		<g:message code="user.phoneNumber.label" default="Phone Number" />
		
	</label>
	<g:textField name="phoneNumber" value="${userInstance?.phoneNumber}"/>
</div>

<div class="fieldcontain ${hasErrors(bean: userInstance, field: 'organization', 'error')} required">
	<label for="organization">
		<g:message code="user.organization.label" default="Organization" />
		<span class="required-indicator">*</span>
	</label>
	<g:textField name="organization" required="" value="${userInstance?.organization}"/>
</div>

<g:if test="${userInstance?.defaultProject != null && userInstance.projects() != null}">
<div class="fieldcontain ${hasErrors(bean: userInstance, field: 'defaultProject', 'error')} ">
	<label for="defaultProject">
		<g:message code="user.defaultProject.label" default="Default Project" />
		
	</label>
	<g:select id="defaultProject" name="defaultProject.id" from="${userInstance.projects()}" optionKey="id" value="${userInstance?.defaultProject?.id}" class="many-to-one" noSelection="['null': '']"/>
</div>
</g:if>

<%--<sec:ifAnyGranted roles="ROLE_ADMIN,ROLE_DEVELOP">--%>
<%--<div class="fieldcontain ${hasErrors(bean: userInstance, field: 'accountExpired', 'error')} ">--%>
<%--	<label for="accountExpired">--%>
<%--		<g:message code="user.accountExpired.label" default="Account Expired" />--%>
<%--		--%>
<%--	</label>--%>
<%--	<g:checkBox name="accountExpired" value="${userInstance?.accountExpired}" />--%>
<%--</div>--%>
<%----%>
<%--<div class="fieldcontain ${hasErrors(bean: userInstance, field: 'accountLocked', 'error')} ">--%>
<%--	<label for="accountLocked">--%>
<%--		<g:message code="user.accountLocked.label" default="Account Locked" />--%>
<%--		--%>
<%--	</label>--%>
<%--	<g:checkBox name="accountLocked" value="${userInstance?.accountLocked}" />--%>
<%--</div>--%>
<%--</sec:ifAnyGranted>--%>

<div class="fieldcontain ${hasErrors(bean: userInstance, field: 'email_show', 'error')} ">
	<label for="email_show">
		<g:message code="user.email_show.label" default="Display Email?" />
		
	</label>
	<g:checkBox name="email_show" value="${userInstance?.email_show}" />
</div>

<sec:ifAnyGranted roles="ROLE_ADMIN,ROLE_DEVELOP">
<div class="fieldcontain ${hasErrors(bean: userInstance, field: 'enabled', 'error')} ">
	<label for="enabled">
		<g:message code="user.enabled.label" default="Enabled" />
		
	</label>
	<g:checkBox name="enabled" value="${userInstance?.enabled}" />
</div>

<%--<div class="fieldcontain ${hasErrors(bean: userInstance, field: 'isPasswordEncoded', 'error')} ">--%>
<%--	<label for="isPasswordEncoded">--%>
<%--		<g:message code="user.isPasswordEncoded.label" default="Is Password Encoded" />--%>
<%--		--%>
<%--	</label>--%>
<%--	<g:checkBox name="isPasswordEncoded" value="${userInstance?.isPasswordEncoded}" />--%>
<%--</div>--%>

<%--<div class="fieldcontain ${hasErrors(bean: userInstance, field: 'passwordExpired', 'error')} ">--%>
<%--	<label for="passwordExpired">--%>
<%--		<g:message code="user.passwordExpired.label" default="Password Expired" />--%>
<%--		--%>
<%--	</label>--%>
<%--	<g:checkBox name="passwordExpired" value="${userInstance?.passwordExpired}" />--%>
<%--</div>--%>
</sec:ifAnyGranted>


<g:if test="${userInstance?.projectGroups != null && userInstance?.projects() != null}">	
<div class="fieldcontain ${hasErrors(bean: userInstance, field: 'projectGroups', 'error')} ">
	<label for="projectGroups">
		<g:message code="user.projectGroups.label" default="Project Groups" />
		
	</label>
<ul class="one-to-many">
<g:each in="${userInstance?.projects()}" var="p">
    <li><g:link controller="project" action="show" id="${p.id}">${p?.encodeAsHTML()}</g:link></li>
</g:each>
<sec:ifAnyGranted roles="ROLE_ADMIN,ROLE_DEVELOP">
<li class="add">
	<g:link controller="projectMember" action="create" params="['user.id': userInstance?.id]">${message(code: 'default.add.label', args: [message(code: 'projectMember.label', default: 'ProjectMember')])}</g:link>
</li>
</sec:ifAnyGranted>
</ul>

</div>
</g:if>

