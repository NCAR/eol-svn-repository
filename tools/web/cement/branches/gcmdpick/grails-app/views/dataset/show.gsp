  
<html>
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
        <meta name="layout" content="ds_publish" />
        <title>Show Dataset</title>
    </head>
    <body>
        <div class="body">
  			<div class="altsubsubtitle" align="left">Review Dataset Metadata</div>
			<br>
            <g:if test="${flash.message}">
            <div class="message">${flash.message}</div>
            </g:if>
            <div class="dialog">
                <table>
                    <tbody>
                    
                         <tr class="prop">
                            <td valign="top" class="name">Project:</td>
                            
                            <td valign="top" class="value">${dataset?.project}</td>
                            
                        </tr>
                  
                       <tr class="prop">
                            <td valign="top" class="name">Title:</td>
                            
                            <td valign="top" class="value">${dataset.title}</td>
                            
                        </tr>
                     
                        <tr class="prop">
                            <td valign="top" class="name">Data Set Summary:</td>
                            
                            <td valign="top" class="value">${dataset.summary.encodeAsHTML()}</td>
                            
                        </tr>
                      
                        <tr class="prop">
                            <td valign="top" class="name">Begin Date:</td>
                            
                            <td valign="top" class="value">${dataset.beginDate}</td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">End Date:</td>
                            
                            <td valign="top" class="value">${dataset.endDate}</td>
                            
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
                            <td valign="top" class="name">Location Keyword:</td>
                            
                            <td valign="top" class="value">${dataset?.locationKeyword}</td>
                            
                        </tr>
                   
                        <tr class="prop">
                            <td valign="top" class="name">Discipline:</td>
                            
                            <td valign="top" class="value">${dataset.discipline}</td>
                            
                        </tr>
                     
                        <tr class="prop">
                            <td valign="top" class="name">Frequency:</td>
                            
                            <td valign="top" class="value">${dataset.frequency}</td>
                            
                        </tr>
                                       
                        <tr class="prop">
                            <td valign="top" class="name">Spatial Type:</td>
                            
                            <td valign="top" class="value">${dataset.spatialType}</td>
                            
                        </tr>
                     
                        <tr class="prop">
                            <td valign="top" class="name">Resolution:</td>
                            
                            <td valign="top" class="value">${dataset.resolution}</td>
                            
                        </tr>
                                       
                        <tr class="prop">
                            <td valign="top" class="name">Platform Name:</td>
                            
                            <td valign="top" class="value">${dataset?.platformKeyword}</td>
                            
                        </tr>
 
                        <tr class="prop">
                            <td valign="top" class="name">Instrument Keyword:</td>
                            
                            <td valign="top" class="value">${dataset?.instrumentKeyword}</td>
                            
                        </tr>
                  
                        <tr class="prop">
                            <td valign="top" class="name">Science Keyword:</td>
                            
                            <td valign="top" class="value">${dataset?.scienceKeyword}</td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">ISO Topic:</td>
                            
                            <td valign="top" class="value">${dataset?.topic}</td>
                            
                        </tr>

                        <tr class="prop">
                            <td valign="top" class="name">PI Contact:</td>

                            <td valign="top" class="value">${dataset?.project?.piContact?.personName?.encodeAsHTML()}</td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">Metadata Contact:</td>
                            
                            <td valign="top" class="value">${dataset?.metadataContact}</td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">Datacenter Contact:</td>
                            
                            <td valign="top" class="value">${dataset?.datacenterContact.encodeAsHTML()}</td>
                            
                        </tr>
 
                        <tr class="prop">
                            <td valign="top" class="name">Data Access:</td>
                            
                            <td valign="top" class="value">${dataset.dataAccess}</td>
                            
                        </tr>
                                        
                        <tr class="prop">
                            <td valign="top" class="name">Distribution Format:</td>
                            
                            <td valign="top" class="value">${dataset?.distributionFormat}</td>
                            
                        </tr>
                     
                        <tr class="prop">
                            <td valign="top" class="name">Progress:</td>
                            
                            <td valign="top" class="value">${dataset.progress}</td>
                            
                        </tr>
 
                        <tr class="prop">
                            <td valign="top" class="name">Data Tags:</td>
                            
                            <td valign="top" class="value">${dataset.dataTags}</td>
                            
                        </tr>

                        <tr class="prop">
                            <td valign="top" class="name">Dataset Language:</td>
                            
                            <td valign="top" class="value">${dataset.datasetLanguage}</td>
                            
                        </tr>
                     
                        <tr class="prop">
                            <td valign="top" class="name">Access Restrictions:</td>
                            
                            <td valign="top" class="value">${dataset.accessRestrictions}</td>
                            
                        </tr>
                   
                        <tr class="prop">
                            <td valign="top" class="name">Use Constraints:</td>
                            
                            <td valign="top" class="value">${dataset.useConstraints}</td>
                            
                        </tr>
                     
                         <tr class="prop">
                            <td valign="top" class="name">CDP Entry ID:</td>
                            <td valign="top" class="value">${dataset.entryID}</td>
                        </tr>
                    
                       <tr class="prop">
                            <td valign="top" class="name">Creation Date:</td>
                            
                            <td valign="top" class="value">${dataset.dateCreated}</td>
                            
                        </tr>
                   
                        <tr class="prop">
                            <td valign="top" class="name">Revision Date:</td>
                            
                            <td valign="top" class="value">${dataset.lastUpdated}</td>
                            
                        </tr>
                  
                    </tbody>
                </table>
            </div>
            <div class="buttons">
                <g:form controller="dataset">
                    <input type="hidden" name="id" value="${dataset?.id}" />
                    <span class="buttons"><g:actionSubmit class="edit" value="Edit" /></span>
		    		<span class="menuButton"><g:link controller="cdp" action="publish" id="${dataset.id}">Submit metadata</g:link></span>
                    <span class="buttons"><g:actionSubmit class="delete" onclick="return confirm('Are you sure?');" value="Delete" /></span>
                </g:form>
            </div>
        </div>
    </body>
</html>
