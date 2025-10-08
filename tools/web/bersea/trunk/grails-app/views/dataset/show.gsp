<html>
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
        <meta name="layout" content="main" />
        <title>Show Dataset</title>
    </head>
    <body>
        <div class="nav">
            <span class="menuButton"><a class="home" href="${createLinkTo(dir:'')}">Home</a></span>
            <span class="menuButton"><g:link class="list" action="list">Dataset List</g:link></span>
            <span class="menuButton"><g:link class="create" action="create">New Dataset</g:link></span>
        </div>
        <div class="body">
            <h1>Show Data Set</h1>
            <g:if test="${flash.message}">
            <div class="message">${flash.message}</div>
            </g:if>
            <div class="dialog">
                <table>
                    <tbody>

                        <tr class="prop">
                            <td valign="top" class="name">Project:</td>
                            
                            <td valign="top" class="value"><g:link controller="project" action="show" id="${dataset?.project?.id}">${dataset?.project?.encodeAsHTML()}</g:link></td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">Title:</td>
                            
                            <td valign="top" class="value">${fieldValue(bean:dataset, field:'title')}</td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">Cruise:</td>
                            
                            <td valign="top" class="value">${fieldValue(bean:dataset, field:'cruise')}</td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">Summary:</td>
                            
                            <td valign="top" class="value">${fieldValue(bean:dataset, field:'summary')}</td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">Begin Date:</td>
                            
                            <td valign="top" class="value">${fieldValue(bean:dataset, field:'beginDate')}</td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">End Date:</td>
                            
                            <td valign="top" class="value">${fieldValue(bean:dataset, field:'endDate')}</td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">Minimum Latitude:</td>
                            
                            <td valign="top" class="value">${fieldValue(bean:dataset, field:'minimumLatitude')}</td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">Maximum Latitude:</td>
                            
                            <td valign="top" class="value">${fieldValue(bean:dataset, field:'maximumLatitude')}</td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">Minimum Longitude:</td>
                            
                            <td valign="top" class="value">${fieldValue(bean:dataset, field:'minimumLongitude')}</td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">Maximum Longitude:</td>
                            
                            <td valign="top" class="value">${fieldValue(bean:dataset, field:'maximumLongitude')}</td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">Discipline:</td>
                            
                            <td valign="top" class="value">${fieldValue(bean:dataset, field:'discipline')}</td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">Location Keyword:</td>
                            
                            <td valign="top" class="value">${fieldValue(bean:dataset, field:'locationKeyword')}</td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">Frequency:</td>
                            
                            <td valign="top" class="value">${fieldValue(bean:dataset, field:'frequency')}</td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">Spatial Type:</td>
                            
                            <td valign="top" class="value">${fieldValue(bean:dataset, field:'spatialType')}</td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">Resolution:</td>
                            
                            <td valign="top" class="value">${fieldValue(bean:dataset, field:'resolution')}</td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">Platform Name:</td>
                            
                            <td valign="top" class="value">${fieldValue(bean:dataset, field:'platformKeyword')}</td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">Instrument Name:</td>
                            
                            <td valign="top" class="value">${fieldValue(bean:dataset, field:'instrumentKeyword')}</td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">Science Keyword:</td>
                            
                            <td valign="top" class="value">${fieldValue(bean:dataset, field:'scienceKeyword')}</td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">ISO Topic:</td>
                            
                            <td valign="top" class="value">${fieldValue(bean:dataset, field:'topic')}</td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">Metadata Contact:</td>
                            
                            <td valign="top" class="value">${fieldValue(bean:dataset, field:'metadataContact')}</td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">Data Access:</td>
                            
                            <td valign="top" class="value">${fieldValue(bean:dataset, field:'dataAccess')}</td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">Data Tags:</td>
                            
                            <td valign="top" class="value">${fieldValue(bean:dataset, field:'dataTags')}</td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">Version Number:</td>
                            
                            <td valign="top" class="value">${fieldValue(bean:dataset, field:'versionNumber')}</td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">Distribution Format:</td>
                            
                            <td valign="top" class="value">${fieldValue(bean:dataset, field:'distributionFormat')}</td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">Progress:</td>
                            
                            <td valign="top" class="value">${fieldValue(bean:dataset, field:'progress')}</td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">Related Resource:</td>
                            
                            <td valign="top" class="value">${fieldValue(bean:dataset, field:'xlink1')}</td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">Resource Purpose:</td>
                            
                            <td valign="top" class="value">${fieldValue(bean:dataset, field:'xlink1Purpose')}</td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">Related Resource:</td>
                            
                            <td valign="top" class="value">${fieldValue(bean:dataset, field:'xlink2')}</td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">Resource Purpose:</td>
                            
                            <td valign="top" class="value">${fieldValue(bean:dataset, field:'xlink2Purpose')}</td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">Access Restrictions:</td>
                            
                            <td valign="top" class="value">${fieldValue(bean:dataset, field:'accessRestrictions')}</td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">Dataset Language:</td>
                            
                            <td valign="top" class="value">${fieldValue(bean:dataset, field:'datasetLanguage')}</td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">Date Created:</td>
                            
                            <td valign="top" class="value">${fieldValue(bean:dataset, field:'dateCreated')}</td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">Last Updated:</td>
                            
                            <td valign="top" class="value">${fieldValue(bean:dataset, field:'lastUpdated')}</td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">Metadata Name:</td>
                            
                            <td valign="top" class="value">${fieldValue(bean:dataset, field:'metadataName')}</td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">Metadata Version:</td>
                            
                            <td valign="top" class="value">${fieldValue(bean:dataset, field:'metadataVersion')}</td>
                            
                        </tr>
                    
                    </tbody>
                </table>
            </div>
            <div class="buttons">
                <g:form>
                    <input type="hidden" name="id" value="${dataset?.id}" />
                    <span class="button"><g:actionSubmit class="edit" value="Edit" /></span>
					<g:loggedInUserInfo  field="username"/>
                    <span class="button"><g:actionSubmit class="delete" onclick="return confirm('Are you sure?');" value="Delete" /></span>
                </g:form>
            </div>
        </div>
        <p> &nbsp;</p>
        <p> &nbsp;</p>
    </body>
</html>
