<html>
<head>
<title>Metadata Entry</title>
<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
         <meta name="layout" content="mainpage" />
		    <g:javascript library="cement" /> 
</head>

<body bgcolor="#E6FFF2" text="#000000">
<div class="body">

<table width="95%" border="1" cellspacing="1" cellpadding="2" height="860" bordercolor="#CCCCCC" name="main panel">
  <tr>
    <td height="120">
      <div align="center"><img src="${createLinkTo(dir:'images',file:'CADIS_archive_logo.gif')}" align="absmiddle" alt="CADIS" />  </div>
    </td>
  </tr>
  <tr>
    <td height="599" valign="baseline">
	 <div class="greeting" align="left">
     <br> &nbsp;&nbsp;<font face="Arial, Helvetica, sans-serif"><b>Hello, ${contact?.personName}! Choose from your previously submitted datasets to edit, or to use as a template:</b></font> <br>
	 </div>
      <br>
      <table width="95%" border="0" align="center" name="datasets" bgcolor="white" >
		<thead>
        <tr>
          <th>PI name</th>
          <th>dataset title</th>
        </tr>
		</thead>
		<tbody>
          		<g:each var="ds" in="${datasets}">
                    <tr class="listRow" onmouseover="cursorOver(this)" onmouseout="cursorOut(this)" >
                       <td> ${ds.project?.piContact?.personName}</td>
                       <td><g:link style="display:block" controller="dataset" action="edit" id="${ds.id}">${ds.title}</g:link>
                       </td>
                    </tr>
               </g:each>
		</tbody>
      </table>
     <br>	
    </td>
  </tr>
    <tr>
    <td>&nbsp;</td>
  </tr>
</table>
</div>
</body>
</html>
