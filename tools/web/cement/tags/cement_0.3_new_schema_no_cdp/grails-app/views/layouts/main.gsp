<html>
	<head>
		<title><g:layoutTitle default="CADIS" /></title>
		<link rel="stylesheet" href="${createLinkTo(dir:'css',file:'main.css')}"></link>
		<g:layoutHead />
		<g:javascript library="application" />				
	</head>
	<body>
		<div id="spinner" class="spinner" style="display:none;">
			<img src="${createLinkTo(dir:'images',file:'spinner.gif')}" alt="Spinner" />
		</div>	
        <div class="logo" align="right" ><img src="${createLinkTo(dir:'images',file:'cadis_logo.gif')}" alt="CADIS" /></div>	
		<g:layoutBody />		
	</body>	
</html>