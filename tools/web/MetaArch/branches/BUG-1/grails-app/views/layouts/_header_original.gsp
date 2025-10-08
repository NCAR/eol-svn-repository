<div id="grailsLogo" role="banner">
			<table>
				<tr>
					<td>
						<a style="text-decoration: none;" href="${createLink(uri: '/')}">
<%--				<img src="${resource(dir: 'images', file: 'grails_logo.png')}" alt="Grails"/>--%>
							<!-- 
							<img style="height: 100px; float:left; padding-right: 15px;" src="http://www.eol.ucar.edu/projects/logos_and_icons/EOL_logo_trans_bg.gif" alt="EOL" />
							<h1>NCAR-EOL/CDS Metadata Tool</h1>
							-->
							<img style="float:left; margin-left: -15px;" src="${resource(dir: 'images', file: 'eol_logo.png')}" alt="EOL" />
							<h1 style="float:left;">CDS Metadata Tool</h1>
						</a>
					</td>
					<td>
						<%--<div id="banner-user">
							<sec:ifLoggedIn> 
								Welcome back, <strong><sec:loggedInUserInfo field="username" /></strong>!
								<g:link action="index" controller="logout"> <i>Logout</i> </g:link>
								<br />
								<br />
								<br />
							</sec:ifLoggedIn>
							<sec:ifNotLoggedIn>
								<g:link action="index" controller="login"> <i>Login</i> </g:link>
								Not a user?<g:link action="index" controller="register"> <i>Register Here</i> </g:link>
								<br />
								<br />
								<br />
							</sec:ifNotLoggedIn>
						</div>
					--%></td>
				</tr>
			</table>
		</div>