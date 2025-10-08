<g:set var="contacts" value="${Contact.list().sort()}" />
<html>
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
        <meta name="layout" content="ds_edit" />
        <title>Edit Dataset</title>
    </head>
    <body>
        <div class="body">
  			<div class="altsubsubtitle" align="left">Edit Metadata for Dataset</div>
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
            <g:form>
		    <g:hiddenField name="id" value="${dataset?.id}" />
                <div class="dialog">
                    <table>
                        <tbody>
                        
                             <tr class='req'>
                                <td valign='top' class='name'>
                                    <label for='project'>1. *<a href="/cadisEditor/help-files/help.html#Project">Project</a>:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'project','errors')}'>
                                    <g:select tabindex="1" optionKey="id" from="${myList}" name='project.id' value="${dataset?.project?.id}"  noSelection="['null':'-choose the project-']" ></g:select>
                                </td>
                            </tr>                        

                           <tr class='req'>
                                <td valign='top' class='name'>
                                   <label for='title'>2. *<a href="/cadisEditor/help-files/help.html#Title">Title</a>:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'title','errors')}'>
									<input tabindex="2" type='text' name='title' maxlength='255' size='80' value="${dataset?.title?.encodeAsHTML()}" />
                                </td>
                            </tr> 
                        
                            <tr class='req'>
                                <td valign='top' class='name'>
                                   <label for='summary'>3. *<a href="/cadisEditor/help-files/help.html#Summary">Data Set Summary</a>:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'summary','errors')}'>
									<textarea tabindex="3" style='height:auto; width:auto;' rows='16' cols='79' name='summary'>${dataset?.summary?.encodeAsHTML()}</textarea>
                                </td>
                            </tr> 
                           
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                   <label for='beginDate'>4. <a href="/cadisEditor/help-files/help.html#Date">Begin Date</a>:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'beginDate','errors')}'>
                                    <g:datePicker name='beginDate' value="${dataset?.beginDate}" precision="minute" ></g:datePicker>
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='endDate'>5. <a href="/cadisEditor/help-files/help.html#Date">End Date</a>:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'endDate','errors')}'>
                                    <g:datePicker name='endDate' value="${dataset?.endDate}" precision="minute" ></g:datePicker>
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='minimumLatitude'>6. <a href="/cadisEditor/help-files/help.html#LatLong">Minimum Latitude</a>:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'minimumLatitude','errors')}'>
                                    <input tabindex="4" type='text' id='minimumLatitude' name='minimumLatitude' value="${fieldValue(bean:dataset,field:'minimumLatitude')}" />
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='maximumLatitude'>7. <a href="/cadisEditor/help-files/help.html#LatLong">Maximum Latitude</a>:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'maximumLatitude','errors')}'>
                                    <input tabindex="5" type='text' id='maximumLatitude' name='maximumLatitude' value="${fieldValue(bean:dataset,field:'maximumLatitude')}" />
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='minimumLongitude'>8. <a href="/cadisEditor/help-files/help.html#LatLong">Minimum Longitude</a>:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'minimumLongitude','errors')}'>
                                    <input tabindex="6" type='text' id='minimumLongitude' name='minimumLongitude' value="${fieldValue(bean:dataset,field:'minimumLongitude')}" />
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='maximumLongitude'>9. <a href="/cadisEditor/help-files/help.html#LatLong">Maximum Longitude</a>:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'maximumLongitude','errors')}'>
                                    <input tabindex="7" type='text' id='maximumLongitude' name='maximumLongitude' value="${fieldValue(bean:dataset,field:'maximumLongitude')}" />
                                </td>
                            </tr> 
                        
                            <tr class='req'>
                                <td valign='top' class='name'>
                                    <label for='locationKeyword'>10. *<a href="/cadisEditor/help-files/help.html#Location">Location Keyword</a>:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'locationKeyword','errors')}'>
                                    <g:select tabindex="8" optionKey="id" from="${GcmdLocation.list()}" name='locationKeyword.id' value="${dataset?.locationKeyword?.id}"  noSelection="['null':'-choose the location-']" ></g:select>
                                </td>
                            </tr> 

                            <tr class='req'>
                                <td valign='top' class='name'>
                                    <label for='discipline'>11. *<a href="/cadisEditor/help-files/help.html#Discipline">Discipline</a>:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'discipline','errors')}'>
                                    <g:select tabindex="9" id='discipline' name='discipline' from='${dataset.constraints.discipline.inList.collect{it.encodeAsHTML()}}' value="${fieldValue(bean:dataset,field:'discipline')}"  noSelection="['null':'-choose a discipline-']" ></g:select>
                                </td>
                            </tr> 

                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='frequency'>12. <a href="/cadisEditor/help-files/help.html#Frequency">Frequency</a>:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'frequency','errors')}'>
                                    <g:select tabindex="10" id='frequency' name='frequency' from='${dataset.constraints.frequency.inList.collect{it.encodeAsHTML()}}' value="${fieldValue(bean:dataset,field:'frequency')}" ></g:select>
                                </td>
                            </tr>                         

                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='spatialType'>13. <a href="/cadisEditor/help-files/help.html#Spatial">Spatial Type</a>:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'spatialType','errors')}'>
                                    <g:select tabindex="11" id='spatialType' name='spatialType' from='${dataset.constraints.spatialType.inList.collect{it.encodeAsHTML()}}' value="${fieldValue(bean:dataset,field:'spatialType')}" ></g:select>
                                </td>
                            </tr> 
                         
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='resolution'>14. <a href="/cadisEditor/help-files/help.html#Resolution">Resolution</a>:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'resolution','errors')}'>
                                    <g:select tabindex="12" id='resolution' name='resolution' from='${dataset.constraints.resolution.inList.collect{it.encodeAsHTML()}}' value="${fieldValue(bean:dataset,field:'resolution')}" ></g:select>
                                </td>
                            </tr> 
                      
                            <tr class='req'>
                                <td valign='top' class='name'>
                                    <label for='platformKeyword'>15. *<a href="/cadisEditor/help-files/help.html#Platform">Platform Keyword</a>:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'platformKeyword','errors')}'>
                                    <g:select tabindex="13" optionKey="id" from="${GcmdPlatform.list()}" name='platformKeyword.id' value="${dataset?.platformKeyword?.id}"  noSelection="['null':'-choose a platform-']" ></g:select>
                                </td>
                            </tr> 
 
                            <tr class='req'>
                                <td valign='top' class='name'>
                                    <label for='instrumentKeyword'>16. *<a href="/cadisEditor/help-files/help.html#Instrument">Instrument Name</a>:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'instrumentKeyword','errors')}'>
                                    <g:select tabindex="14" optionKey="id" from="${GcmdInstrument.list()}" name='instrumentKeyword.id' value="${dataset?.instrumentKeyword?.id}"  noSelection="['null':'-choose the instrument-']" ></g:select>
                                </td>
                            </tr> 
 
                            <tr class='req'>
                                <td valign='top' class='name'>
                                    <label for='scienceKeyword'>17. *<a href="/cadisEditor/help-files/help.html#Science">Science Keyword</a>:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'scienceKeyword','errors')}'>
                                    <g:select tabindex="15" optionKey="id" from="${GcmdScience.list()}" name='scienceKeyword.id' value="${dataset?.scienceKeyword?.id}" noSelection="['null':'-choose from GCMD science keyword-']" ></g:select>
                                </td>
                            </tr> 
                        
                            <tr class='req'>
                                <td valign='top' class='name'>
                                    <label for='topic'>18. *<a href="/cadisEditor/help-files/help.html#ISO">ISO Topic</a>:  &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; </label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'topic','errors')}'>
                                    <g:select tabindex="16" optionKey="id" from="${IsoTopic.list()}" name='topic.id' value="${dataset?.topic?.id}"  noSelection="['null':'-choose from ISO topics-']" ></g:select>
                                </td>
                            </tr> 

                            <tr class='req'>
                                <td valign='top' class='name'>
                                    <label for='metadataContact'>19. *<a href="/cadisEditor/help-files/help.html#MetaContact">Metadata Contact</a>:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'metadataContact','errors')}'>
                                    <g:select tabindex="17" optionKey="id" from="${contacts}" name='metadataContact.id' value="${dataset?.metadataContact?.id}" noSelection="['null':'-choose a contact-']" ></g:select> 
                                </td>
                            </tr> 
                        
                            <tr class='req'>
                                <td valign='top' class='name' nowrap>
                                    <label for='datacenterContact'>20. *<a href="/cadisEditor/help-files/help.html#DataContact">Datacenter Contact</a>:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'datacenterContact','errors')}'>
                                    <g:select tabindex="18" optionKey="id" from="${contacts}" name='datacenterContact.id' value="${dataset?.datacenterContact?.id}" noSelection="['null':'-choose a contact-']" ></g:select>
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='dataAccess'>21. <a href="/cadisEditor/help-files/help.html#DataAccess">Data Access</a>:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'dataAccess','errors')}'>
                                  <input tabindex="19" type="text" id='dataAccess' name='dataAccess' maxlength='255' size='60 'value="${fieldValue(bean:dataset,field:'dataAccess')}"/>&nbsp;<i style="font-size:85%">(URL to data not at CADIS archive)</i>
                                </td>
                            </tr> 

                            <tr class='req'>
                                <td valign='top' class='name'>
                                    <label for='distributionFormat'>22. *<a href="/cadisEditor/help-files/help.html#Format">Distribution Format</a>:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'distributionFormat','errors')}'>
                                    <g:select tabindex="20" optionKey="id" from="${Format.list()}" name='distributionFormat.id' value="${dataset?.distributionFormat?.id}" noSelection="['null':'-choose a format-']" ></g:select>
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='progress'>23. <a href="/cadisEditor/help-files/help.html#Progress">Progress</a>:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'progress','errors')}'>
                                    <g:select tabindex="21" id='progress' name='progress' from='${dataset.constraints.progress.inList.collect{it.encodeAsHTML()}}' value="${fieldValue(bean:dataset,field:'progress')}"  noSelection="['null':'-choose-']" ></g:select>
                                </td>
                            </tr>                        
 
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='dataTags'>24.<a href="/cadisEditor/help-files/help.html#DataTags"> Data Tags</a>:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'dataTags','errors')}'>
				    				<input tabindex="22" type='text' name='dataTags' maxlength='255' size='60' value="${dataset?.dataTags?.encodeAsHTML()}" />
                                </td>
                            </tr> 
                         
                         </tbody>
                    </table>

                    <table>
                        <tbody>
                                              
                            <tr class="prop">
                                <td valign="top" class="name">
                                    <label for="xlink1">25. <a href="/cadisEditor/help-files/help.html#Xlink">Related Resource</a>:</label>
                                </td>
                                <td valign="top" class="value ${hasErrors(bean:dataset,field:'xlink1','errors')}">
                                    <input tabindex="23" type="text" id="xlink1" name="xlink1" size='47' value="${fieldValue(bean:dataset,field:'xlink1')}"/>
                                <td valign="top" class="value ${hasErrors(bean:dataset,field:'xlink1Purpose','errors')}">
                                    <g:select optionKey="id" from="${XlinkType.list()}" name="xlink1Purpose.id" value="${dataset?.xlink1Purpose?.id}" noSelection="['null':'-purpose-']" ></g:select>
                                </td>
                            </tr> 
                        
                            <tr class="prop">
                                <td valign="top" class="name">
                                    <label for="xlink2">26. <a href="/cadisEditor/help-files/help.html#Xlink">Related Resource</a>:</label>
                                </td>
                                <td valign="top" class="value ${hasErrors(bean:dataset,field:'xlink2','errors')}">
                                    <input tabindex="24" type="text" id="xlink2" name="xlink2" size='47' value="${fieldValue(bean:dataset,field:'xlink2')}"/>
                                <td valign="top" class="value ${hasErrors(bean:dataset,field:'xlink2Purpose','errors')}">
                                    <g:select optionKey="id" from="${XlinkType.list()}" name="xlink2Purpose.id" value="${dataset?.xlink2Purpose?.id}" noSelection="['null':'-purpose-']" ></g:select>
                                </td>
                            </tr> 
 
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='datasetLanguage'>27. <a href="/cadisEditor/help-files/help.html#Language">Dataset Language</a>:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'datasetLanguage','errors')}'>
                                    <input tabindex="25" type="text" id='datasetLanguage' name='datasetLanguage' value="${fieldValue(bean:dataset,field:'datasetLanguage')}"/>
                                </td>
                            </tr> 
                         
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='accessRestrictions'>28. <a href="/cadisEditor/help-files/help.html#Restrictions">Access Restrictions</a>:&nbsp;&nbsp;</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'accessRestrictions','errors')}'>
                                    <input tabindex="26" type="text" id='accessRestrictions' name='accessRestrictions' value="${fieldValue(bean:dataset,field:'accessRestrictions')}"/>
                                </td>
                            </tr> 
                       
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='useConstraints'>29. <a href="/cadisEditor/help-files/help.html#Constraints">Use Constraints</a>:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'useConstraints','errors')}'>
                                    <input tabindex="27" type="text" id='useConstraints' name='useConstraints' value="${fieldValue(bean:dataset,field:'useConstraints')}"/>
                                </td>
                            </tr> 
                        </tbody>
                    </table>
                </div>
                <div class="buttons" align="center">
                    <span class="button"><g:actionSubmit class="save" action="Update" value="         Update this dataset's metadata          " /></span><br>
                    <span class="button"><g:actionSubmit class="save" action="Save" value="Create metadata for a new dataset using this one as a template" /></span><br>
                    <span class="button"><g:actionSubmit class="delete" action="Delete" value="          Delete this metadata entry          " onclick="return confirm('Are you sure?')" /></span><br>
                    <span class="button"><g:actionSubmit action="Cancel" value="     Cancel editing     " /></span><br>
                </div>
            </g:form>
        </div>
    </body>
</html>
