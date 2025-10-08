<html>
	<head>
		<title><g:layoutTitle default="CADIS" /></title>
		<link type="text/css" rel="stylesheet" href="${createLinkTo(dir:'css',file:'main.css')}" />
		<g:layoutHead />
		<g:javascript library="application" />				
	</head>
	<body>
		<div id="spinner" class="spinner" style="display:none;">
			<img src="${createLinkTo(dir:'images',file:'spinner.gif')}" alt="Spinner" />
		</div>	
        <div class="nav" align="center"> 
            <g:render template="/adminmenubar" />   
        </div>
		<g:layoutBody />		
	</body>	
</html>
