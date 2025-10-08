<html>
<head>
<title>Upload form</title>
         <meta name="layout" content="formpage" />
</head>

<body>
<!-- This is the form -->
<center><table cellpadding="20"><tr><td>
<div align="left"><center><h3>Upload - File Selection Page</h3></center>
<br/>dataset ID = ${dataset.id}<p/>

<g:form enctype="multipart/form-data">

	<input type="hidden" name="dataset.id" value="${dataset.id}" />
	Select files that you want to upload to this dataset<p/> 
	<table>
		<input type="hidden" name="f2" value="whatever..." /> 
		<tr><td> <input id="fileName1" type="file" name="file_1"/> </td></tr>
		<tr><td> <input id="fileName2" type="file" name="file_2"/> </td></tr>
		<tr><td> <input id="fileName3" type="file" name="file_3"/> </td></tr>
		<tr><td> <input id="fileName4" type="file" name="file_4"/> </td></tr>
		<tr><td> <input id="fileName5" type="file" name="file_5"/> </td></tr>
		<tr><td> </td></tr>
		<tr><td> </td></tr>
     		<tr><td> <g:actionSubmit value="Upload File(s)" action="UploadMultipleFiles"/></td></tr>

		
		<tr><td> Note: when you press the Submit button there is no progress indicated.</td></tr>
		<tr><td> If you are on a slow link and the file(s) are large the upload may take several minutes.</td></tr>
		<tr><td>Please only press the Submit button once!</td></tr>
	</table>
</g:form>
</div>
</td></tr></table></center>
    <td height="808" width="22%" valign="top">
      <table width="100%" border="0" cellspacing="2" cellpadding="2" height="638">
        <tr>
          <td height="74" valign="top" align="left"><img src="${createLinkTo(dir:'images',file:'cadis_logo.gif')}" alt="CADIS" width="217" height="84" align="top" /></td>
        </tr>
        <tr>
          <td height="388" valign="top" align="left">
            <p>&nbsp;</p> 
            <p>&nbsp;</p>
            <p>&nbsp;</p>
            <p><img src="${createLinkTo(dir:'images',file:'3_nav_panel.gif')}" alt="123" width="148" height="182" align="absmiddle" /></p>
          </td>
        </tr>
        <tr>
          <td height="119" valign="top" align="left"> <a href="../../contact/show/149" <img src="${createLinkTo(dir:'images/buttons',file:'back.gif')}"</a> 
																		<a href="../.." <img src="${createLinkTo(dir:'images/buttons',file:'next.gif',file:'next.gif')}"</a> 
        </tr>
      </table>
      <p>&nbsp;</p>
      <p>&nbsp;</p>
      </td>
</body>
</html>
