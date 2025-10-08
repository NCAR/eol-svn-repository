<%@ defaultCodec="HTML" %>
<html>
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
        <meta name="layout" content="main" />
        <title>Create Dataset</title>         
    </head>
    <body>
        <div class="nav">
            <span class="menuButton"><a class="home" href="${createLinkTo(dir:'')}">Home</a></span>
            <span class="menuButton"><g:link class="list" action="list">Dataset List</g:link></span>
        </div>
        <div class="body">
            <h1>Provide Metadata for New Data Set</h1>
            <g:if test="${flash.message}">
            <div class="message">${flash.message}</div>
            </g:if>
            <g:hasErrors bean="${dataset}">
            <div class="errors">
                <g:renderErrors bean="${dataset}" as="list" />
            </div>
            </g:hasErrors>
            <g:form action="save" method="post" >
                <div class="dialog">
                    <table>
                        <tbody>
                        
                            <tr class="prop">
                                <td valign="top" class="name">
                                    <label for="project">Project:</label>
                                </td>
                                <td valign="top" class="value ${hasErrors(bean:dataset,field:'project','errors')}">
                                    <g:select optionKey="id" from="${myList}" name="project.id" value="${dataset?.project?.id}" noSelection="['null':'-choose the project-']" ></g:select>
                                </td>
                            </tr> 
                        
                            <tr class="prop">
                                <td valign="top" class="name">
                                    <label for="title">Title:</label>
                                </td>
                                <td valign="top" class="value ${hasErrors(bean:dataset,field:'title','errors')}">
                                    <input type="text" maxlength="200" size='80' id="title" name="title" value="${fieldValue(bean:dataset,field:'title')}"/>
                                </td>
                            </tr> 
                        
                            <tr class="prop">
                                <td valign="top" class="name">
                                    <label for="cruise">Cruise:</label>
                                </td>
                                <td valign="top" class="value ${hasErrors(bean:dataset,field:'cruise','errors')}">
                                    <input type="text" id="cruise" name="cruise" value="${fieldValue(bean:dataset,field:'cruise')}"/>
                                </td>
                            </tr> 
                        
                            <tr class="prop">
                                <td valign="top" class="name">
                                    <label for="summary">Summary:</label>
                                </td>
                                <td valign="top" class="value ${hasErrors(bean:dataset,field:'summary','errors')}">
                                    <textarea rows="5" cols="79" name="summary">${fieldValue(bean:dataset, field:'summary')}</textarea>
                                </td>
                            </tr> 
                        
                            <tr class="prop">
                                <td valign="top" class="name">
                                    <label for="beginDate">Begin Date:</label>
                                </td>
                                <td valign="top" class="value ${hasErrors(bean:dataset,field:'beginDate','errors')}">
                                    <g:datePicker name="beginDate" value="${dataset?.beginDate}" precision="minute" ></g:datePicker>
                                </td>
                            </tr> 
                        
                            <tr class="prop">
                                <td valign="top" class="name">
                                    <label for="endDate">End Date:</label>
                                </td>
                                <td valign="top" class="value ${hasErrors(bean:dataset,field:'endDate','errors')}">
                                    <g:datePicker name="endDate" value="${dataset?.endDate}" precision="minute" ></g:datePicker>
                                </td>
                            </tr> 
                        
                            <tr class="prop">
                                <td valign="top" class="name">
                                    <label for="minimumLatitude">Minimum Latitude:</label>
                                </td>
                                <td valign="top" class="value ${hasErrors(bean:dataset,field:'minimumLatitude','errors')}">
                                    <input type="text" id="minimumLatitude" name="minimumLatitude" value="${fieldValue(bean:dataset,field:'minimumLatitude')}" />
                                </td>
                            </tr> 
                        
                            <tr class="prop">
                                <td valign="top" class="name">
                                    <label for="maximumLatitude">Maximum Latitude:</label>
                                </td>
                                <td valign="top" class="value ${hasErrors(bean:dataset,field:'maximumLatitude','errors')}">
                                    <input type="text" id="maximumLatitude" name="maximumLatitude" value="${fieldValue(bean:dataset,field:'maximumLatitude')}" />
                                </td>
                            </tr> 
                        
                            <tr class="prop">
                                <td valign="top" class="name">
                                    <label for="minimumLongitude">Minimum Longitude:</label>
                                </td>
                                <td valign="top" class="value ${hasErrors(bean:dataset,field:'minimumLongitude','errors')}">
                                    <input type="text" id="minimumLongitude" name="minimumLongitude" value="${fieldValue(bean:dataset,field:'minimumLongitude')}" />
                                </td>
                            </tr> 
                        
                            <tr class="prop">
                                <td valign="top" class="name">
                                    <label for="maximumLongitude">Maximum Longitude:</label>
                                </td>
                                <td valign="top" class="value ${hasErrors(bean:dataset,field:'maximumLongitude','errors')}">
                                    <input type="text" id="maximumLongitude" name="maximumLongitude" value="${fieldValue(bean:dataset,field:'maximumLongitude')}" />
                                </td>
                            </tr> 
                        
                            <tr class="prop">
                                <td valign="top" class="name">
                                    <label for="discipline">Discipline:</label>
                                </td>
                                <td valign="top" class="value ${hasErrors(bean:dataset,field:'discipline','errors')}">
                                    <g:select optionKey="id" from="${Discipline.list()}" name="discipline.id" value="${dataset?.discipline?.id}" noSelection="['null':'-choose a discipline-']" ></g:select>
                                </td>
                            </tr> 
                        
                            <tr class="prop">
                                <td valign="top" class="name">
                                    <label for="locationKeyword">Location Keyword:</label>
                                </td>
                                <td valign="top" class="value ${hasErrors(bean:dataset,field:'locationKeyword','errors')}">
                                    <g:select optionKey="id" from="${GcmdLocation.list()}" name="locationKeyword.id" value="${dataset?.locationKeyword?.id}" noSelection="['null':'-choose the location-']" ></g:select>
                                </td>
                            </tr> 
                        
                            <tr class="prop">
                                <td valign="top" class="name">
                                    <label for="frequency">Frequency:</label>
                                </td>
                                <td valign="top" class="value ${hasErrors(bean:dataset,field:'frequency','errors')}">
                                    <g:select optionKey="id" from="${Frequency.list()}" name="frequency.id" value="${dataset?.frequency?.id}" ></g:select>
                                </td>
                            </tr> 
                        
                            <tr class="prop">
                                <td valign="top" class="name">
                                    <label for="spatialType">Spatial Type:</label>
                                </td>
                                <td valign="top" class="value ${hasErrors(bean:dataset,field:'spatialType','errors')}">
                                    <g:select id="spatialType" name="spatialType" from="${dataset.constraints.spatialType.inList}" value="${dataset.spatialType}" ></g:select>
                                </td>
                            </tr> 
                        
                            <tr class="prop">
                                <td valign="top" class="name">
                                    <label for="resolution">Resolution:</label>
                                </td>
                                <td valign="top" class="value ${hasErrors(bean:dataset,field:'resolution','errors')}">
                                    <g:select optionKey="id" from="${Resolution.list()}" name="resolution.id" value="${dataset?.resolution?.id}" ></g:select>
                                </td>
                            </tr> 
                        
                            <tr class="prop">
                                <td valign="top" class="name">
                                    <label for="platformKeyword">Platform Keyword:</label>
                                </td>
                                <td valign="top" class="value ${hasErrors(bean:dataset,field:'platformKeyword','errors')}">
                                    <g:select optionKey="id" from="${GcmdPlatform.list()}" name="platformKeyword.id" value="${dataset?.platformKeyword?.id}" noSelection="['null':'-choose a platform-']" ></g:select>
                                </td>
                            </tr> 
                        
                            <tr class="prop">
                                <td valign="top" class="name">
                                    <label for="instrumentKeyword">Instrument Keyword:</label>
                                </td>
                                <td valign="top" class="value ${hasErrors(bean:dataset,field:'instrumentKeyword','errors')}">
                                    <g:select optionKey="id" from="${GcmdInstrument.list()}" name="instrumentKeyword.id" value="${dataset?.instrumentKeyword?.id}" noSelection="['null':'-choose the instrument-']" ></g:select>
                                </td>
                            </tr> 
                        
                            <tr class="prop">
                                <td valign="top" class="name">
                                    <label for="scienceKeyword">Science Keyword:</label>
                                </td>
                                <td valign="top" class="value ${hasErrors(bean:dataset,field:'scienceKeyword','errors')}">
                                    <g:select optionKey="id" from="${GcmdScience.list()}" name="scienceKeyword.id" value="${dataset?.scienceKeyword?.id}" noSelection="['null':'-choose from GCMD science keyword-']" ></g:select>
                                </td>
                            </tr> 
                        
                            <tr class="prop">
                                <td valign="top" class="name">
                                    <label for="topic">ISO Topic:</label>
                                </td>
                                <td valign="top" class="value ${hasErrors(bean:dataset,field:'topic','errors')}">
                                    <g:select optionKey="id" from="${IsoTopic.list()}" name="topic.id" value="${dataset?.topic?.id}" noSelection="['null':'-choose an ISO topic-']" ></g:select>
                                </td>
                            </tr> 
                        
                            <tr class="prop">
                                <td valign="top" class="name">
                                    <label for="metadataContact">Metadata Contact:</label>
                                </td>
                                <td valign="top" class="value ${hasErrors(bean:dataset,field:'metadataContact','errors')}">
                                    <g:select optionKey="id" from="${Researcher.list()}" name="metadataContact.id" value="${dataset?.metadataContact?.id}" ></g:select>
                                </td>
                            </tr> 
                        
                            <tr class="prop">
                                <td valign="top" class="name">
                                    <label for="dataAccess">Data Access:</label>
                                </td>
                                <td valign="top" class="value ${hasErrors(bean:dataset,field:'dataAccess','errors')}">
                                    <input type="text" id="dataAccess" name="dataAccess" value="${fieldValue(bean:dataset,field:'dataAccess').encodeAsURL()}"/> &nbsp; <i style="font-size:85%"> (URL to data not at BEST/BSIERP archives) </i>
                                </td>
                            </tr> 
                        
                            <tr class="prop">
                                <td valign="top" class="name">
                                    <label for="dataTags">Data Tags:</label>
                                </td>
                                <td valign="top" class="value ${hasErrors(bean:dataset,field:'dataTags','errors')}">
                                    <input type="text" maxlength="200" id="dataTags" name="dataTags" value="${fieldValue(bean:dataset,field:'dataTags')}"/>
                                </td>
                            </tr> 
                        
                            <tr class="prop">
                                <td valign="top" class="name">
                                    <label for="versionNumber">Data Set Version:</label>
                                </td>
                                <td valign="top" class="value ${hasErrors(bean:dataset,field:'versionNumber','errors')}">
                                    <input type="text" id="versionNumber" name="versionNumber" value="${fieldValue(bean:dataset,field:'versionNumber')}" />
                                </td>
                            </tr> 
                        
                            <tr class="prop">
                                <td valign="top" class="name">
                                    <label for="distributionFormat">Distribution Format:</label>
                                </td>
                                <td valign="top" class="value ${hasErrors(bean:dataset,field:'distributionFormat','errors')}">
                                    <g:select optionKey="id" from="${Format.list()}" name="distributionFormat.id" value="${dataset?.distributionFormat?.id}" noSelection="['null':'-choose a format-']" ></g:select>
                                </td>
                            </tr> 
                        
                            <tr class="prop">
                                <td valign="top" class="name">
                                    <label for="progress">Progress:</label>
                                </td>
                                <td valign="top" class="value ${hasErrors(bean:dataset,field:'progress','errors')}">
                                    <g:select id="progress" name="progress" from="${dataset.constraints.progress.inList}" value="${dataset.progress}" ></g:select>
                                </td>
                            </tr> 

                            <tr class="prop">
                                <td valign="top" class="name">
                                    <label for="xlink1">Related Resource:</label>
                                </td>
                                <td valign="top" class="value ${hasErrors(bean:dataset,field:'xlink1','errors')}">
                                    <input type="text" id="xlink1" name="xlink1" size='47' value="${fieldValue(bean:dataset,field:'xlink1').encodeAsURL()}"/>
                                </td>
                            </tr> 
                        
                            <tr class="prop">
                                <td valign="top" class="name">
                                    <label for="xlink1Purpose">  </label>
                                </td>
                                <td valign="top" class="value ${hasErrors(bean:dataset,field:'xlink1Purpose','errors')}">
                                    <g:select optionKey="id" from="${XlinkType.list()}" name="xlink1Purpose.id" value="${dataset?.xlink1Purpose?.id}" noSelection="['null':'-purpose-']"></g:select>
                                </td>
                            </tr> 
                        
                            <tr class="prop">
                                <td valign="top" class="name">
                                    <label for="xlink2">Related Resource:</label>
                                </td>
                                <td valign="top" class="value ${hasErrors(bean:dataset,field:'xlink2','errors')}">
                                    <input type="text" id="xlink2" name="xlink2" size='47' value="${fieldValue(bean:dataset,field:'xlink2').encodeAsURL()}"/>
                                </td>
                            </tr> 
                        
                            <tr class="prop">
                                <td valign="top" class="name">
                                    <label for="xlink2Purpose">  </label>
                                </td>
                                <td valign="top" class="value ${hasErrors(bean:dataset,field:'xlink2Purpose','errors')}">
                                    <g:select optionKey="id" from="${XlinkType.list()}" name="xlink2Purpose.id" value="${dataset?.xlink2Purpose?.id}" noSelection="['null':'-purpose-']"></g:select>
                                </td>
                            </tr> 
                        
                            <tr class="prop">
                                <td valign="top" class="name">
                                    <label for="accessRestrictions">Access Restrictions:</label>
                                </td>
                                <td valign="top" class="value ${hasErrors(bean:dataset,field:'accessRestrictions','errors')}">
                                    <g:select id="accessRestrictions" name="accessRestrictions" from="${dataset.constraints.accessRestrictions.inList}" value="${dataset.accessRestrictions}" ></g:select>
                                </td>
                            </tr> 
                        
                            <tr class="prop">
                                <td valign="top" class="name">
                                    <label for="datasetLanguage">Dataset Language:</label>
                                </td>
                                <td valign="top" class="value ${hasErrors(bean:dataset,field:'datasetLanguage','errors')}">
                                    <input type="text" id="datasetLanguage" name="datasetLanguage" value="${fieldValue(bean:dataset,field:'datasetLanguage')}"/>
                                </td>
                            </tr> 
                        
                        </tbody>
                    </table>
                </div>
                <div class="buttons">
                    <span class="button"><input class="save" type="submit" value="Create" /></span>
                </div>
            </g:form>
        </div>
        <p> &nbsp;</p>
        <p> &nbsp;</p>
    </body>
</html>
