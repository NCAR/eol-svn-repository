<%@ page import="meta.GcmdLocation" %>


<ul>
	<li><a class="home" href="${createLink(uri: '/')}"><g:message code="default.home.label"/></a></li>
	<g:if test="${actionName != 'list'}">
		<li><g:link class="list" action="list"><g:message code="default.list.label" args="[entityName]" /></g:link></li>
	</g:if>
	<g:if test="${actionName!= 'create'}">
		<li><g:link class="create" action="create"><g:message code="default.new.label" args="[entityName]" /></g:link></li>
	</g:if>
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