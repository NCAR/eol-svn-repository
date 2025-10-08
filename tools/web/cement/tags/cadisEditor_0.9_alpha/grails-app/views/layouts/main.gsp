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
<table width="100%" border="0" cellspacing="0" cellpadding="2" height="808" bordercolor="#CCCCCC" name="contact panel">
  <tr>
    <td height="860" width="78%">
      <table width="100%" border="0" cellspacing="2" cellpadding="2">
        <tr>
          <td height="35">
		  </td>
        </tr>
        <tr>
          <td height="825">
		  	<g:layoutBody />		
		  </td>
        </tr>
      </table>
    </td>
  </tr>
  
</table>
	</body>	
</html>
