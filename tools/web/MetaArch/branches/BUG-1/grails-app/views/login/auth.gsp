<html>

<head>
	<meta name='layout' content='main'/>
	<title><g:message code='spring.security.ui.login.title'/></title>
	<meta name='layout' content='register'/>
	
	<style type='text/css' media='screen'>
		#login {
			margin: 15px 0px;
			padding: 0px;
			text-align: center;
		}
		
		#login .inner {
			width: 340px;
			padding-bottom: 6px;
			margin: 60px auto;
			text-align: left;
			border: 1px solid #aab;
			background-color: #f0f0fa;
			-moz-box-shadow: 2px 2px 2px #eee;
			-webkit-box-shadow: 2px 2px 2px #eee;
			-khtml-box-shadow: 2px 2px 2px #eee;
			box-shadow: 2px 2px 2px #eee;
		}
	
		#login .inner .fheader {
			padding: 18px 26px 14px 26px;
			background-color: #f7f7ff;
			margin: 0px 0 14px 0;
			color: #2e3741;
			font-size: 18px;
			font-weight: bold;
		}
		
		#login .inner .cssform p {
			clear: left;
			margin: 0;
			padding: 4px 0 3px 0;
			padding-left: 105px;
			margin-bottom: 20px;
			height: 1%;
		}
	
	
		#login .inner .cssform input[type='text'] {
			width: 120px;
		}
		
		#login .inner .cssform label {
			font-weight: bold;
			float: left;
			text-align: right;
			margin-left: -105px;
			width: 110px;
			padding-top: 3px;
			padding-right: 10px;
		}
		
		#login #remember_me_holder {
			padding-left: 120px;
		}
	
		#login #submit {
			margin-left: 15px;
		}
	
		#login #remember_me_holder label {
			display: inline;
			float: none;
			margin-left: 0;
			text-align: left;
			/*width: 200px*/
		}
	
		#login .inner .login_message {
			padding: 6px 25px 20px 25px;
			color: #c33;
		}
	
		#login .inner .text_ {
			width: 120px;
		}
	
		#login .inner .chk {
			height: 12px;
		}
	</style>
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
				<li class="login-li">
					<span>
					Not a user? <g:link action="index" controller="register"> <i>Register Here</i> </g:link>
					</span>
				</li>
			</sec:ifNotLoggedIn>
		</ul>
	</div>
	<!-- End Nav Bar -->

	<div id="ucar_body" class="clearfix center_div">
		<br />
		<div class="login s2ui_center ui-corner-all" style='text-align:center;'>
			<div class="login-inner">
			<form action='${postUrl}' method='POST' id="loginForm" name="loginForm" autocomplete='off'>
			<div class="sign-in">
		
			<h1><g:message code='spring.security.ui.login.signin'/></h1>
		
			<table>
				<tr>
					<td><label for="username"><g:message code='spring.security.ui.login.username'/></label></td>
					<td><input name="j_username" id="username" size="20" /></td>
				</tr>
				<tr>
					<td><label for="password"><g:message code='spring.security.ui.login.password'/></label></td>
					<td><input type="password" name="j_password" id="password" size="20" /></td>
				</tr>
				<tr>
					<td colspan='2'>
						<input type="checkbox" class="checkbox" name="${rememberMeParameter}" id="remember_me" checked="checked" />
						<label for='remember_me'><g:message code='spring.security.ui.login.rememberme'/></label> |
						<span class="forgot-link">
							<g:link controller='register' action='forgotPassword'><g:message code='spring.security.ui.login.forgotPassword'/></g:link>
						</span>
					</td>
				</tr>
				<tr>
					<td colspan='2'>
		<%--				<s2ui:linkButton elementId='register' controller='register' messageCode='spring.security.ui.login.register'/>--%>
						<s2ui:submitButton elementId='loginButton' form='loginForm' messageCode='spring.security.ui.login.login'/>
					</td>
				</tr>
			</table>
		
			</div>
			</form>
			</div>
		</div>
		
		<script>
		$(document).ready(function() {
			$('#username').focus();
		});
		
		<s2ui:initCheckboxes/>
		
		</script>
	</div>
</body>
</html>
