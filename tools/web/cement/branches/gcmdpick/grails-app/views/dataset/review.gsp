<html>
<head>
<title>Metadata Entry</title>
<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">

<g:if test="${'template' == what}">
<g:set var="theAction" value="template" />
<meta name="layout" content="formpage" />
</g:if>

<g:elseif test="${'upload' == what}">
<g:set var="theAction" value="uploadto" />
<%-- XXX change formpage --%>
<meta name="layout" content="formpage" />
</g:elseif>

<g:else>
<g:set var="theAction" value="edit" />
<meta name="layout" content="formpage" />
</g:else>

<g:javascript library="cement" /> 
</head>
<body>
<div class="body">

<table width="95%" border="0" cellspacing="1" cellpadding="2" height="860" name="main panel">
  <tr>
    <td height="120">
      <span align="center"><img src="${createLinkTo(dir:'images',file:'cadishead10.png')}" align="absmiddle" alt="CADIS" /></span>
    </td>
  </tr>
  <tr><td>
            <g:if test="${flash.message}">
            <div class="message">${flash.message}</div>
            </g:if>
  </td></tr>
  <tr>
    <td height="599" valign="baseline">
	 <div class="greeting" align="left">
     <br> &nbsp;&nbsp;<font face="Arial, Helvetica, sans-serif"><b>Hello, ${contact?.personName}! Choose
<g:if test="${'template' == theAction}">
     from these datasets to use as a template for a new dataset:
</g:if>
<g:elseif test="${'uploadto' == theAction}">
     a dataset to which to upload data:
</g:elseif>
<g:else>
     from your previously submitted datasets to edit:
</g:else>
     </b></font> <br>
	 </div>
      <br>
      <table width="95%" border="0" align="center" name="datasets" bgcolor="white" >
		<thead>
        <tr>
          <th width="15%">PI name</th>
          <th>dataset title</th>
        </tr>
		</thead>
		<tbody>
          		<g:each var="ds" in="${datasets}">
                    <tr class="listRow" onmouseover="cursorOver(this)" onmouseout="cursorOut(this)" >
                       <td width="15%">${ds.project?.piContact?.personName?.encodeAsHTML()}</td>
                       <td><g:link style="display:block" controller="dataset" action="${theAction}" id="${ds.id}">${ds.title?.encodeAsHTML()}</g:link></td>
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
