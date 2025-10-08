<html>
    <head>
         <title>Show Dataset</title>
         <meta name="layout" content="formpage" />
    </head>

    <body>
        <div class="body">
           <h1>Show Dataset</h1>
           <g:if test="${flash.message}">
                 <div class="message">${flash.message}</div>
           </g:if>
           <div class="dialog">
                 <table>
                   <tbody>
                        <tr class="prop">
                              <td valign="top" class="name">Title:</td>
                              
                                    <td valign="top" class="value">${dataset.title}</td>
                              
                        </tr>
                   
                        <tr class="prop">
                              <td valign="top" class="name">Summary:</td>
                              
                                    <td valign="top" class="value">${dataset.summary}</td>
                              
                        </tr>
                   
                        <tr class="prop">
                              <td valign="top" class="name">Begin Date:</td>
                              
                                    <td valign="top" class="value"> <g:formatDate date="${dataset.beginDate}" format="yyyy-MMM-dd"/> </td>
                              
                        </tr>
                   
                        <tr class="prop">
                              <td valign="top" class="name">End Date:</td>
                              
                                    <td valign="top" class="value"> <g:formatDate date="${dataset.endDate}" format="yyyy-MMM-dd"/> </td>
                              
                        </tr>
                   
                        <tr class="prop">
                              <td valign="top" class="name">Minimum Latitude:</td>
                              
                                    <td valign="top" class="value">${dataset.minimumLatitude}</td>
                              
                        </tr>
                   
                        <tr class="prop">
                              <td valign="top" class="name">Maximum Latitude:</td>
                              
                                    <td valign="top" class="value">${dataset.maximumLatitude}</td>
                              
                        </tr>
                   
                        <tr class="prop">
                              <td valign="top" class="name">Minimum Longitude:</td>
                              
                                    <td valign="top" class="value">${dataset.minimumLongitude}</td>
                              
                        </tr>
                   
                        <tr class="prop">
                              <td valign="top" class="name">Maximum Longitude:</td>
                              
                                    <td valign="top" class="value">${dataset.maximumLongitude}</td>
                              
                        </tr>
                   
                        <tr class="prop">
                              <td valign="top" class="name">Pi Contact:</td>
                              
                                    <td valign="top" class="value"><g:link controller="contact" action="show" id="${dataset?.piContact?.id}">${dataset?.piContact}</g:link></td>
                              
                        </tr>
                   
                        <tr class="prop">
                              <td valign="top" class="name">Metadata Contact:</td>
                              
                                    <td valign="top" class="value"><g:link controller="contact" action="show" id="${dataset?.metadataContact?.id}">${dataset?.metadataContact}</g:link></td>
                              
                        </tr>
                   
                        <tr class="prop">
                              <td valign="top" class="name">Datacenter Contact:</td>
                              
                                    <td valign="top" class="value"><g:link controller="contact" action="show" id="${dataset?.datacenterContact?.id}">${dataset?.datacenterContact}</g:link></td>
                              
                        </tr>
                   
                        <tr class="prop">
                              <td valign="top" class="name">Progress:</td>
                              
                                    <td valign="top" class="value">${dataset.progress}</td>
                              
                        </tr>
                   
                        <tr class="prop">
                              <td valign="top" class="name">Spatial Type:</td>
                              
                                    <td valign="top" class="value">${dataset.spatialType}</td>
                              
                        </tr>
                   
                        <tr class="prop">
                              <td valign="top" class="name">Location Keyword:</td>
                              
                                    <td valign="top" class="value"><g:link controller="gcmdLocation" action="show" id="${dataset?.locationKeyword?.id}">${dataset?.locationKeyword}</g:link></td>
                              
                        </tr>
                   
                        <tr class="prop">
                              <td valign="top" class="name">Science Keyword:</td>
                              
                                    <td valign="top" class="value"><g:link controller="gcmdScience" action="show" id="${dataset?.scienceKeyword?.id}">${dataset?.scienceKeyword}</g:link></td>
                              
                        </tr>
                   
                        <tr class="prop">
                              <td valign="top" class="name">Discipline:</td>
                              
                                    <td valign="top" class="value">${dataset.discipline}</td>
                              
                        </tr>
                   
                        <tr class="prop">
                              <td valign="top" class="name">NSF Award Number:</td>
                              
                                    <td valign="top" class="value">${dataset.nsfAwardNumber}</td>
                              
                        </tr>
                   
                        <tr class="prop">
                              <td valign="top" class="name">Project Keyword:</td>
                              
                                    <td valign="top" class="value"><g:link controller="gcmdProject" action="show" id="${dataset?.projectKeyword?.id}">${dataset?.projectKeyword}</g:link></td>
                              
                        </tr>
                   
                   </tbody>
                 </table>
           </div>
           <div class="buttons">
               <g:form controller="dataset">
                 <input type="hidden" name="id" value="${dataset?.id}" />
                 <span class="button"><g:actionSubmit value="Edit" /></span>
                 <span class="button"><g:actionSubmit value="Delete" onclick="return warnBeforeDatasetDelete();" /></span>
               </g:form>
           </div>
        </div>
      </table>
      <p>&nbsp;</p>
      <p>&nbsp;</p>
      </td>

    </body>
</html>
