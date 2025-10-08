<head>
<meta name='layout' content='main' />
<title><g:message code="springSecurity.denied.title" /></title>
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
<%--				<li class="login-li">--%>
<%--					<g:link action="index" controller="login"> <i>Login</i> </g:link>--%>
<%--				</li>--%>
<%--				<li class="login-li">--%>
<%--					<span>--%>
<%--					Not a user? <g:link action="index" controller="register"> <i>Register Here</i> </g:link>--%>
<%--					</span>--%>
<%--				</li>--%>
			</sec:ifNotLoggedIn>
		</ul>
	</div>
	<!-- End Nav Bar -->

	<div id="ucar_body" class="clearfix center_div">
		<div class='body'>
			<div class='errors'><g:message code="springSecurity.denied.message" /></div>
		</div>
	</div>
</body>
