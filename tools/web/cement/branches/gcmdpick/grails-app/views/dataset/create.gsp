<g:set var="contacts" value="${Contact.list().sort()}" />
<html>
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
        <meta name="layout" content="ds_create" />
        <title>Create Dataset</title>         
    </head>
    <body>
        <div class="body">
  			<div class="altsubsubtitle" align="left">Provide Metadata for New Dataset</div>
			 <div class='req'>Required fields are marked with *</div>
			<br>
            <g:if test="${flash.message}">
            <div class="message">${flash.message}</div>
            </g:if>
            <g:hasErrors bean="${dataset}">
            <div class="errors">
                <g:renderErrors bean="${dataset}" as="list" />
            </div>
            </g:hasErrors>
            <g:form action="save" method="post">
                <div class="dialog">
                    <table>
                        <tbody>

                             <tr class='req'>
                                <td valign='top' class='name'>
                                    <label for='project'>1. *Project:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'project','errors')}'>
                                    <g:select optionKey="id" from="${Project.list().sort()}" name='project.id' value="${dataset?.project?.id}"  noSelection="['null':'-choose the project-']"></g:select>
                                </td>
                            </tr> 
                      
                           <tr class='req'>
                                <td valign='top' class='name'>
                                   <label for='title'>2. *Title:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'title','errors')}'>
									<input type='text' name='title' maxlength='255' size='80' value="${dataset?.title?.encodeAsHTML()}" />
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                   <label for='summary'><a href="/cadisEditor/help-files/help.html#summary">3.</a> Data Set Summary:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'summary','errors')}'>
									<textarea style='height:auto; width:auto;' rows='16' cols='79' name='summary'>${dataset?.summary?.encodeAsHTML()}</textarea>
                                </td>
                            </tr> 
                          
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                   <label for='beginDate'>4. Begin Date:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'beginDate','errors')}'>
                                    <g:datePicker name='beginDate' value="${dataset?.beginDate}" precision="minute" ></g:datePicker>
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='endDate'>5. End Date:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'endDate','errors')}'>
                                    <g:datePicker name='endDate' value="${dataset?.endDate}" precision="minute" ></g:datePicker>
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='minimumLatitude'>6. Minimum Latitude:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'minimumLatitude','errors')}'>
                                    <input type='text' id='minimumLatitude' name='minimumLatitude' value="${fieldValue(bean:dataset,field:'minimumLatitude')}" />
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='maximumLatitude'>7. Maximum Latitude:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'maximumLatitude','errors')}'>
                                    <input type='text' id='maximumLatitude' name='maximumLatitude' value="${fieldValue(bean:dataset,field:'maximumLatitude')}" />
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='minimumLongitude'>8. Minimum Longitude:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'minimumLongitude','errors')}'>
                                    <input type='text' id='minimumLongitude' name='minimumLongitude' value="${fieldValue(bean:dataset,field:'minimumLongitude')}" />
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='maximumLongitude'>9. Maximum Longitude:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'maximumLongitude','errors')}'>
                                    <input type='text' id='maximumLongitude' name='maximumLongitude' value="${fieldValue(bean:dataset,field:'maximumLongitude')}" />
                                </td>
                            </tr> 
                        
                            <tr class='req'>
                                <td valign='top' class='name'>
                                    <label for='locationKeyword'>10. *Location Keyword:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'locationKeyword','errors')}'>
                                    <g:select optionKey="id" from="${GcmdLocation.list()}" name='locationKeyword.id' value="${dataset?.locationKeyword?.id}"  noSelection="['null':'-choose the location-']"></g:select>
                                </td>
                            </tr> 

                            <tr class='req'>
                                <td valign='top' class='name'>
                                    <label for='discipline'>11. *Discipline:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'discipline','errors')}'>
                                    <g:select id='discipline' name='discipline' from='${dataset.constraints.discipline.inList.collect{it.encodeAsHTML()}}' value="${fieldValue(bean:dataset,field:'discipline')}"  noSelection="['null':'-choose a discipline-']" ></g:select>
                                </td>
                            </tr> 

                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='frequency'>12. Frequency:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'frequency','errors')}'>
                                    <g:select id='frequency' name='frequency' from='${dataset.constraints.frequency.inList.collect{it.encodeAsHTML()}}' value="${fieldValue(bean:dataset,field:'frequency')}" ></g:select>
                                </td>
                            </tr>                         

                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='spatialType'>13. Spatial Type:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'spatialType','errors')}'>
                                    <g:select id='spatialType' name='spatialType' from='${dataset.constraints.spatialType.inList.collect{it.encodeAsHTML()}}' value="${fieldValue(bean:dataset,field:'spatialType')}" ></g:select>
                                </td>
                            </tr> 
                         
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='resolution'>14. Resolution:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'resolution','errors')}'>
                                    <g:select id='resolution' name='resolution' from='${dataset.constraints.resolution.inList.collect{it.encodeAsHTML()}}' value="${fieldValue(bean:dataset,field:'resolution')}" ></g:select>
                                </td>
                            </tr> 
                                             
                            <tr class='req'>
                                <td valign='top' class='name'>
                                    <label for='platformKeyword'>15. *Platform Keyword:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'platformKeyword','errors')}'>
                                    <g:select optionKey="id" from="${GcmdPlatform.list()}" name='platformKeyword.id' value="${dataset?.platformKeyword?.id}"  noSelection="['null':'-choose a platform-']"></g:select>
                                </td>
                            </tr> 
     
                            <tr class='req'>
                                <td valign='top' class='name'>
                                    <label for='instrumentKeyword'>16. *Instrument Name:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'instrumentKeyword','errors')}'>
                                    <g:select optionKey="id" from="${GcmdInstrument.list()}" name='instrumentKeyword.id' value="${dataset?.instrumentKeyword?.id}"  noSelection="['null':'-choose the instrument-']"></g:select>
                                </td>
                            </tr> 
 
                            <tr class='req'>
                                <td valign='top' class='name'>
                                    <label for='scienceKeyword'>17. *Science Keyword:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'scienceKeyword','errors')}'>
                                    <g:select optionKey="id" from="${GcmdScience.list()}" name='scienceKeyword.id' value="${dataset?.scienceKeyword?.id}" noSelection="['null':'-choose from GCMD science keyword-']" ></g:select>
                                </td>
                            </tr> 
                        
                            <tr class='req'>
                                <td valign='top' class='name'>
                                    <label for='topic'>18. *ISO Topic:  &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; </label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'topic','errors')}'>
                                    <g:select optionKey="id" from="${IsoTopic.list()}" name='topic.id' value="${dataset?.topic?.id}"  noSelection="['null':'-choose from ISO topics-']"></g:select>
                                </td>
                            </tr> 

                            <tr class='req'>
                                <td valign='top' class='name'>
                                    <label for='metadataContact'>19. *Metadata Contact:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'metadataContact','errors')}'>
                                    <g:select optionKey="id" from="${contacts}" name='metadataContact.id' value="${dataset?.metadataContact?.id}" noSelection="['null':'-choose a contact-']"></g:select> <span class="menuButton"><g:link controller="contact" action="create">create new contact</g:link></span>
                                </td>
                            </tr> 
                        
                            <tr class='req'>
                                <td valign='top' class='name'>
                                    <label for='datacenterContact'>20. *Datacenter Contact:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'datacenterContact','errors')}'>
                                    <g:select optionKey="id" from="${contacts}" name='datacenterContact.id' value="${dataset?.datacenterContact?.id}" noSelection="['null':'-choose a contact-']" ></g:select> <span class="menuButton"><g:link controller="contact" action="create">create new contact</g:link></span>
                                </td>
                            </tr> 

                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='dataAccess'>21. Data Access:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'dataAccess','errors')}'>
                                  <input type="text" id='dataAccess' name='dataAccess' maxlength='255' size='60' value="${fieldValue(bean:dataset,field:'dataAccess')}"/>
                                </td>
                            </tr> 

                            <tr class='req'>
                                <td valign='top' class='name'>
                                    <label for='distributionFormat'>22. *Distribution Format:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'distributionFormat','errors')}'>
                                    <g:select optionKey="id" from="${Format.list()}" name='distributionFormat.id' value="${dataset?.distributionFormat?.id}" noSelection="['null':'-choose a format-']" ></g:select>
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='progress'>23. Progress:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'progress','errors')}'>
                                    <g:select id='progress' name='progress' from='${dataset.constraints.progress.inList.collect{it.encodeAsHTML()}}' value="${fieldValue(bean:dataset,field:'progress')}"  noSelection="['null':'-choose-']" ></g:select>
                                </td>
                            </tr>                        
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='dataTags'>24. Data Tags:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'dataTags','errors')}'>
				  <input type='text' name='dataTags' maxlength='255' size='60' value="${dataset?.dataTags?.encodeAsHTML()}" />
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='datasetLanguage'>25. Dataset Language:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'datasetLanguage','errors')}'>
                                    <input type="text" id='datasetLanguage' name='datasetLanguage' value="${fieldValue(bean:dataset,field:'datasetLanguage')}" />
                                </td>
                            </tr> 
                         
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='accessRestrictions'>26. Access Restrictions:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'accessRestrictions','errors')}'>
                                    <input type="text" id='accessRestrictions' name='accessRestrictions' value="${fieldValue(bean:dataset,field:'accessRestrictions')}" />
                                </td>
                            </tr> 
                       
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='useConstraints'>27. Use Constraints:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'useConstraints','errors')}'>
                                    <input type="text" id='useConstraints' name='useConstraints' value="${fieldValue(bean:dataset,field:'useConstraints')}" />
                                </td>
                            </tr> 
                        
                        </tbody>
                    </table>
                </div>
				<p>
				<hr>
                <div class="buttons">
                    <span class="button"><input class="save" type="submit" value="Save metadata for new dataset" /></span><br>
                </div>
				<p>
				<p>
				<hr>
            </g:form>
        </div>

    </body>
</html>
