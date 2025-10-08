<!doctype html>
<!--[if lt IE 7 ]> <html lang="en" class="no-js ie6" id="htmltop"> <![endif]-->
<!--[if IE 7 ]>    <html lang="en" class="no-js ie7" id="htmltop"> <![endif]-->
<!--[if IE 8 ]>    <html lang="en" class="no-js ie8" id="htmltop"> <![endif]-->
<!--[if IE 9 ]>    <html lang="en" class="no-js ie9" id="htmltop"> <![endif]-->
<!--[if (gt IE 9)|!(IE)]><!--> <html lang="en" class="no-js" id="htmltop"><!--<![endif]-->
	<head>
		<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
		<meta http-equiv="X-UA-Compatible" content="IE=edge,chrome=1">
		<title><g:layoutTitle default="Grails"/></title>
		<meta name="viewport" content="width=device-width, initial-scale=1.0">
		<link rel="shortcut icon" href="${resource(dir: 'images', file: 'favicon.ico')}" type="image/x-icon">
		<link rel="apple-touch-icon" href="${resource(dir: 'images', file: 'apple-touch-icon.png')}">
		<link rel="apple-touch-icon" sizes="114x114" href="${resource(dir: 'images', file: 'apple-touch-icon-retina.png')}">
		<link rel="stylesheet" href="${resource(dir: 'css', file: 'main.css')}" type="text/css">
		<link rel="stylesheet" href="${resource(dir: 'css', file: 'mobile.css')}" type="text/css">
		
		<g:javascript library="jquery" />
		<g:layoutHead/>
        <r:layoutResources />
        
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
        
        <link rel="stylesheet" href="${resource(dir: 'css', file: 'custom.css')}" type="text/css">
		<link rel="stylesheet" href="${resource(dir: 'css', file: 'universal_orgnav.css')}" type="text/css">
<%--        <sec:ifLoggedIn>--%>
        	  <g:linkTheme href="" />  
<%--        </sec:ifLoggedIn>--%>
	</head>
	<body onload="printCurrentYear(); adjustNavWidth()">
		<g:render template="/layouts/nav_org_wrap"/>
		<g:render template="/layouts/header"/>
		
		<div id="ucar_body" class="clearfix center_div">
			<g:layoutBody/>
		</div>
		<g:render template="/layouts/footer"/>
		<div id="spinner" class="spinner" style="display:none;"><g:message code="spinner.alt" default="Loading&hellip;"/></div>
		<g:javascript library="application"/>
        <r:layoutResources />
	</body>
</html>