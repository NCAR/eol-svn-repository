<html>
<head>
		<title><g:layoutTitle default="CADIS metadata" /></title>
        <link rel="stylesheet" href="${createLinkTo(dir:'css',file:'formpage.css')}"></link>
		<g:layoutHead />
		<g:javascript library="application" />				
<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
</head>

<body bgcolor="#E6FFF2" text="#000000">
<table width="100%" border="0" cellspacing="0" cellpadding="2" height="808" bordercolor="#CCCCCC" name="contact panel">
  <tr>
    <td height="860" width="78%">
      <table width="100%" border="0" cellspacing="2" cellpadding="2">
        <tr>
          <td height="35">
      		<div class="nav">
            	<g:render template="/adminmenubar" /> 
      		</div>
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
