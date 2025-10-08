<html>

<head>
	<meta name='layout' content='register'/>
	<title><g:message code='spring.security.ui.register.title'/> Message</title>
</head>

<body>
<!-- Nav Bar -->
		<div class="nav" role="navigation">
			<ul>
				<sec:ifLoggedIn> 
					<li class="login-li">
						<span>
						Welcome back, <strong><sec:loggedInUserInfo field="username" /></strong>!
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
						Not a user? <g:link action="index" controller="register"> <i>Register Here</i> </g:link>
						</span>
					</li>
				</sec:ifNotLoggedIn>
			</ul>
		</div>
		<!-- End Nav Bar -->
<br />
<g:if test="${flash.message}">
	<div class="message" role="status">${flash.message}</div>
</g:if>


</body>
</html>
