<html>

<head>
	<meta name='layout' content='register'/>
	<title><g:message code='spring.security.ui.register.title'/></title>
</head>

<body>

<br />

<s2ui:form width='650' height='375' elementId='loginFormContainer'
           titleCode='spring.security.ui.register.description' center='true'>

<g:form action='register' name='registerForm'>

	<g:if test='${emailSent}'>
	<br/>
	<g:message code='spring.security.ui.register.sent'/>
	</g:if>
	<g:else>

	<br/>

	<table>
	<tbody>

		<s2ui:textFieldRow name='username' labelCode='user.username.label' bean="${command}"
                         size='40' labelCodeDefault='Username *' value="${command.username}" required=""/>

		<s2ui:textFieldRow name='email' bean="${command}" value="${command.email}"
		                   size='40' labelCode='user.email.label' labelCodeDefault='E-mail *' required=""/>

		<s2ui:passwordFieldRow name='password' labelCode='user.password.label' bean="${command}"
                             size='40' labelCodeDefault='Password *' value="${command.password}" required=""/>

		<s2ui:passwordFieldRow name='password2' labelCode='user.password2.label' bean="${command}"
                             size='40' labelCodeDefault='Password (again)' value="${command.password2}"/>
                          
		
		<tr class="prop">
			<td class="name" valign="top">
			<label for="realname">
				<g:message code="user.realname.label" default="Full Name" />
					
			</label>
			</td>
			<td class="value" valign="top">
			<g:textField name="realname" value="${userInstance?.realname}" size='40'/>
			</td>
		</tr>
		
		<tr class="prop">
			<td class="name" valign="top">
			<label for="organization">
				<g:message code="user.organization.label" default="Organization" />
				<span class="required-indicator">*</span>
			</label>
			</td>
			<td class="value" valign="top">
			<g:textField name="organization" required="" value="${userInstance?.organization}" size='40'/>
			</td>
		</tr>
		
		<tr class="prop">
			<td class="name" valign="top">
			<label for="phoneNumber">
				<g:message code="user.phoneNumber.label" default="Phone Number" />
			</label>
			</td>
			<td class="value" valign="top">
			<g:textField name="phoneNumber" value="${userInstance?.phoneNumber}"/>
			</td>
		</tr>

	</tbody>
	</table>

	<s2ui:submitButton elementId='create' form='registerForm' messageCode='spring.security.ui.register.submit'/>

	</g:else>

</g:form>

</s2ui:form>

<script>
$(document).ready(function() {
	$('#username').focus();
});
</script>

</body>
</html>
