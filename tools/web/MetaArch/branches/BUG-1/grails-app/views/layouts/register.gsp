<html>
	<head>
		<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
		
		<title><g:layoutTitle default='User Registration'/></title>
		
		<link rel="shortcut icon" href="${resource(dir:'images',file:'favicon.ico')}" type="image/x-icon"/>
	
		<s2ui:resources module='register' />
		<link rel="stylesheet" href="${resource(dir: 'css', file: 'tooltip.css')}" type="text/css">
		<%--
		
		The 'resources' tag in SecurityUiTagLib renders these tags if you're not using the resources plugin:
		
			<g:javascript library='jquery' plugin='jquery' />
			<link rel="stylesheet" media="screen" href="${resource(dir:'css',file:'reset.css',plugin:'spring-security-ui')}"/>
			<link rel="stylesheet" media="screen" href="${resource(dir:'css',file:'spring-security-ui.css',plugin:'spring-security-ui')}"/>
			<jqui:resources />
			<link rel="stylesheet" media="screen" href="${resource(dir:'css/smoothness',file:'jquery-ui-1.8.2.custom.css',plugin:'spring-security-ui')}"/>
			<link rel="stylesheet" media="screen" href="${resource(dir:'css',file:'jquery.jgrowl.css',plugin:'spring-security-ui')}"/>
			<link rel="stylesheet" media="screen" href="${resource(dir:'css',file:'jquery.safari-checkbox.css',plugin:'spring-security-ui')}"/>
			<link rel="stylesheet" media="screen" href="${resource(dir:'css',file:'auth.css',plugin:'spring-security-ui')}"/>
		
		or these if you are:
		
		   <r:require module="register"/>
		   <r:layoutResources/>
		
		If you need to customize the resources, replace the <s2ui:resources> tag with
		the explicit tags above and edit those, not the taglib code.
		--%>
		
		<g:layoutHead/>
		
		<script type="text/javascript">
			function adjustNavWidth() {
				var w = $('body').css('width');
				w = w.substring(0, w.length-2);
				var ubw = $('#ucar_body').css('width');
				ubw = ubw.substring(0, ubw.length-2);
				var x = (w-ubw)/2;
				$('#ucar_body > .nav').css('padding-right', x+'px');
				$('#ucar_body > .nav').css('margin-right', '-'+x+'px');
			}
	    
			function printCurrentYear() {
				var d = new Date();
				document.getElementById("curYear").innerHTML = " - " + d.getFullYear();
			} 
		</script>

		<link rel="stylesheet" href="${resource(dir: 'css', file: 'main.css')}" type="text/css">
		<link rel="stylesheet" href="${resource(dir: 'css', file: 'mobile.css')}" type="text/css">
		<link rel="stylesheet" href="${resource(dir: 'css', file: 'custom.css')}" type="text/css">
		<link rel="stylesheet" href="${resource(dir: 'css', file: 'eol.css')}" type="text/css">
		<link rel="stylesheet" href="${resource(dir: 'css', file: 'universal_orgnav.css')}" type="text/css">
<%--        <sec:ifLoggedIn>--%>
<%--        	  <g:linkTheme href="" />  --%>
<%--        </sec:ifLoggedIn>--%>
	</head>

	<body onload="printCurrentYear(); adjustNavWidth()">
		<g:render template="/layouts/nav_org_wrap"/>
		<g:render template="/layouts/header"/>
		
		<div id="ucar_body" class="clearfix center_div">
			<s2ui:layoutResources module='register' />
			<g:layoutBody/>
			<%--
			<g:javascript src='jquery/jquery.jgrowl.js' plugin='spring-security-ui'/>
			<g:javascript src='jquery/jquery.checkbox.js' plugin='spring-security-ui'/>
			<g:javascript src='spring-security-ui.js' plugin='spring-security-ui'/>
			--%>
			
			<s2ui:showFlash/>
			<br />
		</div>
		
		<g:render template="/layouts/footer"/>
		<div id="spinner" class="spinner" style="display:none;"><g:message code="spinner.alt" default="Loading&hellip;"/></div>
	</body>
</html>
