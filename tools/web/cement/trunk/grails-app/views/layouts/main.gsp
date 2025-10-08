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
        <div class="logo" align="right" ><img src="${createLinkTo(dir:'images',file:'cadis_logo.gif')}" alt="CADIS" /></div>	
<table width="100%" border="0" cellspacing="0" cellpadding="2" bordercolor="#CCCCCC" name="contact panel">
        <tr>
          <td>
		  	<g:layoutBody />		
		  </td>
        </tr>
  
</table>
	</body>	
</html>
