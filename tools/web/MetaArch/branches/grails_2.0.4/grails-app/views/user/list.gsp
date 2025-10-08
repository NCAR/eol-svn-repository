


<%@ page import="meta.auth.User" %>
<!doctype html>
<html>
	<head>
		<meta name="layout" content="main">
		<g:set var="entityName" value="${message(code: 'user.label', default: 'User')}" />
		<title><g:message code="default.list.label" args="[entityName]" /></title>
	</head>
	<body>		
		<a href="#list-user" class="skip" tabindex="-1"><g:message code="default.link.skip.label" default="Skip to content&hellip;"/></a>
		<div class="nav" role="navigation">
			<g:render template="nav"/>
		</div>
		<div id="list-user" class="content scaffold-list" role="main">
			<h1><g:message code="default.list.label" args="[entityName]" /></h1>
			<g:if test="${flash.message}">
			<div class="message" role="status">${flash.message}</div>
			</g:if>
			<table>
				<thead>
					<tr>
						<sec:ifAnyGranted roles="ROLE_ADMIN,ROLE_DEVELOP">
						<g:sortableColumn property="username" title="${message(code: 'user.username.label', default: 'Username')}" />
						</sec:ifAnyGranted>
						
						<g:sortableColumn property="realname" title="${message(code: 'user.realname.label', default: 'Full Name')}" />
					
<%--						<g:sortableColumn property="password" title="${message(code: 'user.password.label', default: 'Password')}" />--%>
					
						<g:sortableColumn property="email" title="${message(code: 'user.email.label', default: 'Email')}" />
					
						<sec:ifAnyGranted roles="ROLE_ADMIN,ROLE_DEVELOP">
						<g:sortableColumn property="phoneNumber" title="${message(code: 'user.phoneNumber.label', default: 'Phone Number')}" />
						</sec:ifAnyGranted>
					
						<g:sortableColumn property="organization" title="${message(code: 'user.organization.label', default: 'Organization')}" />
					
						<sec:ifAnyGranted roles="ROLE_ADMIN,ROLE_DEVELOP">
						<th><g:message code="user.defaultProject.label" default="Default Project" /></th>
						</sec:ifAnyGranted>
					
					</tr>
				</thead>
				<tbody>
				<g:set var="dmgRole" value="${null}" />
				<sec:ifAnyGranted roles="ROLE_ADMIN,ROLE_DEVELOP">
					<g:set var="dmgRole" value="${meta.auth.Authority.findByAuthority('ROLE_DMG')}" />
				</sec:ifAnyGranted>
				<g:each in="${userInstanceList}" status="i" var="userInstance">
					<tr class="${(i % 2) == 0 ? 'even' : 'odd'}<g:if test="${(userInstance.getAuthorities()).contains(dmgRole)}"> tr-dmg</g:if>">
					
						<sec:ifAnyGranted roles="ROLE_ADMIN,ROLE_DEVELOP">
						<td><g:link action="show" id="${userInstance.id}">${fieldValue(bean: userInstance, field: "username")}</g:link></td>
						</sec:ifAnyGranted>
						
						<td>${fieldValue(bean: userInstance, field: "realname")}</td>
					
<%--						<td>${fieldValue(bean: userInstance, field: "password")}</td>--%>
					
						<td>
							<sec:ifNotGranted roles="ROLE_ADMIN,ROLE_DEVELOP">
							<g:if test="${userInstance?.email_show == true}">
							${fieldValue(bean: userInstance, field: "email")}
							</g:if>
							</sec:ifNotGranted>
							<sec:ifAnyGranted roles="ROLE_ADMIN,ROLE_DEVELOP">
							${fieldValue(bean: userInstance, field: "email")}
							</sec:ifAnyGranted>
						</td>
					
						<sec:ifAnyGranted roles="ROLE_ADMIN,ROLE_DEVELOP">
						<td>${fieldValue(bean: userInstance, field: "phoneNumber")}</td>
						</sec:ifAnyGranted>
					
						<td>${fieldValue(bean: userInstance, field: "organization")}</td>
					
						<sec:ifAnyGranted roles="ROLE_ADMIN,ROLE_DEVELOP">
						<td>${fieldValue(bean: userInstance, field: "defaultProject")}</td>
						</sec:ifAnyGranted>
					
					</tr>
				</g:each>
				</tbody>
			</table>
			<div class="pagination">
				<g:paginate total="${userInstanceTotal}" />
			</div>
		</div>
	</body>
</html>
