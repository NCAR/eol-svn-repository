<%@ page import="meta.Dataset" %>

<g:set var="entityName" value="${message(code: 'dataset.label', default: 'Data Set')}" />
<ul>
	<li><a class="home" href="${createLink(uri: '/')}"><g:message code="default.home.label"/></a></li>
	<g:if test="${actionName != 'list'}">
		<li><g:link class="list" action="list"><g:message code="dataset.list.label" args="[entityName]" /></g:link></li>
	</g:if>
	<g:if test="${!actionName.equals('create') && !actionName.equals('getProject')}">
		<li><g:link class="create" action="create"><g:message code="dataset.new.label" args="[entityName]" /></g:link></li>
	</g:if>
	<g:if test="${actionName.equals('create')}">
		<g:ifMultiProject>
			<li><g:link action="getProject">Choose a Different Project</g:link></li>
		</g:ifMultiProject>
	</g:if>
	
<%--	<g:if test="${actionName.equals('show')}">--%>
<%--		<li><g:link class="clone" action="template" id="${datasetInstance?.id}">Clone Data Set</g:link></li>--%>
<%--	</g:if>--%>
	
	<sec:ifLoggedIn> 
		<li class="login-li">
			<span>
				Welcome back, <strong><g:user /></strong>!
			</span>
		</li>
		<li class="login-li">
			<g:link action="index" controller="logout"> <i>Logout</i> </g:link>
		</li>
	</sec:ifLoggedIn>
	<sec:ifNotLoggedIn>
		<li class="login-li">
			<g:link action="index" controller="login"> <i>Login</i> </g:link>
		</li>
		<li class="login-li">
			<span>
				Not a user?<g:link action="index" controller="register"> <i>Register Here</i> </g:link>
			</span>
		</li>
	</sec:ifNotLoggedIn>
</ul>