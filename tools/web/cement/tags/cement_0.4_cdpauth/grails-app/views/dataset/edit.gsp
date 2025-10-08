  
<html>
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
        <meta name="layout" content="main" />
        <title>Edit Dataset</title>
    </head>
    <body>
        <div class="nav">
            <span class="menuButton"><a class="home" href="${createLinkTo(dir:'')}">Home</a></span>
            <span class="menuButton"><g:link class="list" action="list">Dataset List</g:link></span>
            <span class="menuButton"><g:link class="create" action="create">New Dataset</g:link></span>
        </div>
        <div class="body">
            <h1>Edit Dataset</h1>
            <g:if test="${flash.message}">
            <div class="message">${flash.message}</div>
            </g:if>
            <g:hasErrors bean="${dataset}">
            <div class="errors">
                <g:renderErrors bean="${dataset}" as="list" />
            </div>
            </g:hasErrors>
            <g:form controller="dataset" method="post" >
                <input type="hidden" name="id" value="${dataset?.id}" />
                <div class="dialog">
                    <table>
                        <tbody>
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='title'>Title:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'title','errors')}'>
                                    <textarea rows='5' cols='40' name='title'>${dataset?.title?.encodeAsHTML()}</textarea>
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='summary'>Summary:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'summary','errors')}'>
                                    <textarea rows='5' cols='40' name='summary'>${dataset?.summary?.encodeAsHTML()}</textarea>
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='beginDate'>Begin Date:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'beginDate','errors')}'>
                                    <g:datePicker name='beginDate' value="${dataset?.beginDate}" ></g:datePicker>
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='endDate'>End Date:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'endDate','errors')}'>
                                    <g:datePicker name='endDate' value="${dataset?.endDate}" ></g:datePicker>
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='minimumLatitude'>Minimum Latitude:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'minimumLatitude','errors')}'>
                                    <input type='text' id='minimumLatitude' name='minimumLatitude' value="${fieldValue(bean:dataset,field:'minimumLatitude')}" />
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='maximumLatitude'>Maximum Latitude:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'maximumLatitude','errors')}'>
                                    <input type='text' id='maximumLatitude' name='maximumLatitude' value="${fieldValue(bean:dataset,field:'maximumLatitude')}" />
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='minimumLongitude'>Minimum Longitude:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'minimumLongitude','errors')}'>
                                    <input type='text' id='minimumLongitude' name='minimumLongitude' value="${fieldValue(bean:dataset,field:'minimumLongitude')}" />
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='maximumLongitude'>Maximum Longitude:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'maximumLongitude','errors')}'>
                                    <input type='text' id='maximumLongitude' name='maximumLongitude' value="${fieldValue(bean:dataset,field:'maximumLongitude')}" />
                                </td>
                            </tr> 

                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='metadataContact'>Metadata Contact:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'metadataContact','errors')}'>
                                    <g:select optionKey="id" from="${Contact.list()}" name='metadataContact.id' value="${dataset?.metadataContact?.id}" ></g:select>
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='datacenterContact'>Datacenter Contact:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'datacenterContact','errors')}'>
                                    <g:select optionKey="id" from="${Contact.list()}" name='datacenterContact.id' value="${dataset?.datacenterContact?.id}" ></g:select>
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='progress'>Progress:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'progress','errors')}'>
                                    <g:select id='progress' name='progress' from='${dataset.constraints.progress.inList.collect{it.encodeAsHTML()}}' value="${fieldValue(bean:dataset,field:'progress')}" ></g:select>
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='spatialType'>Spatial Type:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'spatialType','errors')}'>
                                    <g:select id='spatialType' name='spatialType' from='${dataset.constraints.spatialType.inList.collect{it.encodeAsHTML()}}' value="${fieldValue(bean:dataset,field:'spatialType')}" ></g:select>
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='accessRestrictions'>Access Restrictions:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'accessRestrictions','errors')}'>
                                    <input type="text" id='accessRestrictions' name='accessRestrictions' value="${fieldValue(bean:dataset,field:'accessRestrictions')}"/>
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='creationDate'>Creation Date:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'creationDate','errors')}'>
                                    <g:datePicker name='creationDate' value="${dataset?.creationDate}" ></g:datePicker>
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='dataAccess'>Data Access:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'dataAccess','errors')}'>
                                    <input type="text" id='dataAccess' name='dataAccess' value="${fieldValue(bean:dataset,field:'dataAccess')}"/>
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='datasetLanguage'>Dataset Language:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'datasetLanguage','errors')}'>
                                    <input type="text" id='datasetLanguage' name='datasetLanguage' value="${fieldValue(bean:dataset,field:'datasetLanguage')}"/>
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='discipline'>Discipline:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'discipline','errors')}'>
                                    <g:select id='discipline' name='discipline' from='${dataset.constraints.discipline.inList.collect{it.encodeAsHTML()}}' value="${fieldValue(bean:dataset,field:'discipline')}" ></g:select>
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='distributionFormat'>Distribution Format:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'distributionFormat','errors')}'>
                                    <g:select optionKey="id" from="${Format.list()}" name='distributionFormat.id' value="${dataset?.distributionFormat?.id}" ></g:select>
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='entryID'>Entry ID:</label>
                                </td>
                                <td valign='top' class='value'>
				    ${dataset.entryID}
				</td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='instrumentKeyword'>Instrument Keyword:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'instrumentKeyword','errors')}'>
                                    <g:select optionKey="id" from="${GcmdInstrument.list()}" name='instrumentKeyword.id' value="${dataset?.instrumentKeyword?.id}" ></g:select>
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='locationKeyword'>Location Keyword:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'locationKeyword','errors')}'>
                                    <g:select optionKey="id" from="${GcmdLocation.list()}" name='locationKeyword.id' value="${dataset?.locationKeyword?.id}" ></g:select>
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='metadataName'>Metadata Name:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'metadataName','errors')}'>
                                    <input type="text" id='metadataName' name='metadataName' value="${fieldValue(bean:dataset,field:'metadataName')}"/>
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='metadataVersion'>Metadata Version:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'metadataVersion','errors')}'>
                                    <input type="text" id='metadataVersion' name='metadataVersion' value="${fieldValue(bean:dataset,field:'metadataVersion')}"/>
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='platformKeyword'>Platform Keyword:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'platformKeyword','errors')}'>
                                    <g:select optionKey="id" from="${GcmdPlatform.list()}" name='platformKeyword.id' value="${dataset?.platformKeyword?.id}" ></g:select>
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='project'>Project:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'project','errors')}'>
                                    <g:select optionKey="id" from="${Project.list()}" name='project.id' value="${dataset?.project?.id}" ></g:select>
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='revisionDate'>Revision Date:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'revisionDate','errors')}'>
                                    <g:datePicker name='revisionDate' value="${dataset?.revisionDate}" ></g:datePicker>
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='scienceKeyword'>Science Keyword:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'scienceKeyword','errors')}'>
                                    <g:select optionKey="id" from="${GcmdScience.list()}" name='scienceKeyword.id' value="${dataset?.scienceKeyword?.id}" ></g:select>
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='topic'>Topic:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'topic','errors')}'>
                                    <g:select optionKey="id" from="${IsoTopic.list()}" name='topic.id' value="${dataset?.topic?.id}" ></g:select>
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='useConstraints'>Use Constraints:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:dataset,field:'useConstraints','errors')}'>
                                    <input type="text" id='useConstraints' name='useConstraints' value="${fieldValue(bean:dataset,field:'useConstraints')}"/>
                                </td>
                            </tr> 
                        
                        </tbody>
                    </table>
                </div>
                <div class="buttons">
                    <span class="button"><g:actionSubmit class="save" value="Update" /></span>
                    <span class="button"><g:actionSubmit class="delete" onclick="return confirm('Are you sure?');" value="Delete" /></span>
                </div>
            </g:form>
        </div>
    </body>
</html>
