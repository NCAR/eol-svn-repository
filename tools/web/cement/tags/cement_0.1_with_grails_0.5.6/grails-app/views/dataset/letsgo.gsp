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
      <div align="center"><img src="${createLinkTo(dir:'images',file:'main_cadis_top_2.gif')}" align="absmiddle" alt="CADIS" />  </div>
    </td>
  </tr>
  <tr>
    <td height="133">
	 <div class="greeting" align="left">
     <br> &nbsp;&nbsp;<font face="Arial, Helvetica, sans-serif"><b>Hello, ${contact?.personName}! Your previously submitted datasets:</b></font> <br>
	 </div>
      <br>
      <table width="95%" border="0" align="center" name="datasets" bgcolor="white" >
		<thead>
        <tr>
          <g:sortableColumn property="${datasets[piContact]}" title="PI name" />
          <g:sortableColumn property="${datasets.title}" title="dataset title" />
        </tr>
		</thead>
		<tbody>
          		<g:each var="ds" in="${datasets}">
                    <tr class="listRow" onmouseover="cursorOver(this)" onmouseout="cursorOut(this)" >
                       <td> ${ds.piContact?.personName}</td>
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
    <td height="221">
      <div align="center"><img src="${createLinkTo(dir:'images',file:'boxes.gif')}" width="700" height="260" alt="boxes"></div>
    </td>
  </tr>
  <tr>
    <td height="291">
      <div align="center">
        <table width="95%" border="0" cellspacing="1" cellpadding="2" height="185" name="Buttons" bordercolor="#CCCCCC">
          <tr>
            <td width="90%" height="216">
              <div align="center">
                <p><b><font face="Arial, Helvetica, sans-serif">Submitting data to the CADIS data archive is a 3-step process.</font></b></p>
              </div>
			  <div align="left">	
              <ol>
                <li>
                  <p><font face="Arial, Helvetica, sans-serif">If you have submitted data your contact information should already be filled out; please review it.</font></p>
                </li>
                <li>
                  <p><font face="Arial, Helvetica, sans-serif">Describe the dataset and provide metadata for the dataset.</font></p>
                </li>
                <li>
                  <p><font face="Arial, Helvetica, sans-serif">Fill in some details about the data files, and then upload the files to the archive.</font></p>
                  <p>&nbsp;</p>
                  <p><font face="Arial, Helvetica, sans-serif">Choose a dataset from the list above to use as a template, or start with a new one.</font></p>
                </li>
              </ol>
			  </div>
            </td>
            <td width="10%" height="216">
			
			<a href="new_dataset.html"><img src="${createLinkTo(dir:'images',file:'new_dataset.gif')}"width="150" height="50" border="0" alt="New_dataset" /></a>
			<a href="edit_dataset.html"><img src="${createLinkTo(dir:'images',file:'edit_dataset.gif')}" width="150" height="50" border="0" alt="Edit_dataset"></a></td>
          </tr>
        </table>
        <p>&nbsp;</p>
      </div>
      </td>
  </tr>
  <tr>
    <td>&nbsp;</td>
  </tr>
</table>
</div>
</body>
</html>
