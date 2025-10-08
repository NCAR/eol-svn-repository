<html>
<head>
<title>Contacts Entry</title>
         <meta name="layout" content="formpage" />
</head>

<body>
        <div class="body">
           <h1>Show Contact</h1>
           <g:if test="${flash.message}">
                 <div class="message">${flash.message}</div>
           </g:if>
           <div class="dialog">
                 <table>
                   <tbody>
                        <tr class="prop">
                              <td valign="top" class="name">Id:</td>
                              <td valign="top" class="value">${contact.id}</td>
                        </tr>
                   
                        <tr class="prop">
                              <td valign="top" class="name">Person Name:</td>
                                    <td valign="top" class="value">${contact.personName}</td>
                        </tr>
                   
                        <tr class="prop">
                              <td valign="top" class="name">Organization Name:</td>
                                    <td valign="top" class="value">${contact.organizationName}</td>
                        </tr>
                   
                        <tr class="prop">
                              <td valign="top" class="name">Short Name:</td>
                                    <td valign="top" class="value">${contact.shortName}</td>
                        </tr>
                   
                        <tr class="prop">
                              <td valign="top" class="name">Address:</td>
                                    <td valign="top" class="value">${contact.address}</td>
                        </tr>
                   
                        <tr class="prop">
                              <td valign="top" class="name">City:</td>
                                    <td valign="top" class="value">${contact.city}</td>
                        </tr>
                   
                        <tr class="prop">
                              <td valign="top" class="name">State:</td>
                                    <td valign="top" class="value">${contact.state}</td>
                        </tr>
                   
                        <tr class="prop">
                              <td valign="top" class="name">Postal Code:</td>
                                    <td valign="top" class="value">${contact.postalCode}</td>
                        </tr>
                   
                        <tr class="prop">
                              <td valign="top" class="name">Email:</td>
                                    <td valign="top" class="value">${contact.email}</td>
                        </tr>
                   
                        <tr class="prop">
                              <td valign="top" class="name">Home Page:</td>
                                    <td valign="top" class="value">${contact.homePage}</td>
                        </tr>
                   
                        <tr class="prop">
                              <td valign="top" class="name">Fax Number:</td>
                                    <td valign="top" class="value">${contact.faxNumber}</td>
                        </tr>
                   
                        <tr class="prop">
                              <td valign="top" class="name">Gcmd Name:</td>
                                    <td valign="top" class="value">${contact.gcmdName}</td>
                        </tr>
                   
                        <tr class="prop">
                              <td valign="top" class="name">Metadata Datasets:</td>
                                     <td  valign="top" style="text-align:left;" class="value">
                                        <ul>
                                            <g:each var="m" in="${contact.metadataDatasets}">
                                                <li><g:link controller="dataset" action="show" id="${m.id}">${m}</g:link></li>
                                            </g:each>
                                        </ul>
                                     </td>
                              
                        </tr>
                   
                        <tr class="prop">
                              <td valign="top" class="name">Department:</td>
                                    <td valign="top" class="value">${contact.department}</td>
                        </tr>
                   
                        <tr class="prop">
                              <td valign="top" class="name">Country:</td>
                                    <td valign="top" class="value">${contact.country}</td>
                        </tr>
                   
                        <tr class="prop">
                              <td valign="top" class="name">Datacenter Datasets:</td>
                                     <td  valign="top" style="text-align:left;" class="value">
                                        <ul>
                                            <g:each var="d" in="${contact.datacenterDatasets}">
                                                <li><g:link controller="dataset" action="show" id="${d.id}">${d}</g:link></li>
                                            </g:each>
                                        </ul>
                                     </td>
                              
                        </tr>
                   
                        <tr class="prop">
                              <td valign="top" class="name">Phone Number:</td>
                                    <td valign="top" class="value">${contact.phoneNumber}</td>
                        </tr>
                   
                        <tr class="prop">
                              <td valign="top" class="name">Pi Datasets:</td>
                                     <td  valign="top" style="text-align:left;" class="value">
                                        <ul>
                                            <g:each var="p" in="${contact.piDatasets}">
                                                <li><g:link controller="dataset" action="show" id="${p.id}">${p}</g:link></li>
                                            </g:each>
                                        </ul>
                                     </td>
                        </tr>
                   </tbody>
                 </table>
           </div>
           <div class="buttons">
               <g:form controller="contact">
                 <input type="hidden" name="id" value="${contact?.id}" />
                 <span class="button"><g:actionSubmit value="Edit" /></span>
                 <span class="button"><g:actionSubmit value="Delete" onclick="return warnBeforeContactDelete();" /></span>
               </g:form>
           </div>
        </div>
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
            <p><img src="${createLinkTo(dir:'images',file:'2_nav_panel.gif')}" alt="123" width="148" height="182" align="absmiddle" /></p>
          </td>
        </tr>
        <tr>
          <td height="119" valign="top" align="left"> <a href="../../dataset/edit/482" <img src="${createLinkTo(dir:'images/buttons',file:'back.gif')}"</a> 
																		<a href="../../upload/upload/149" <img src="${createLinkTo(dir:'images/buttons',file:'next.gif',file:'next.gif')}"</a> 
        </tr>
      </table>
      <p>&nbsp;</p>
      <p>&nbsp;</p>
      </td>

</body>
</html
