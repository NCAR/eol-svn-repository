  
<html>
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
        <meta name="layout" content="main" />
        <title>Show Project</title>
    </head>
    <body>
        <div class="nav">
            <span class="menuButton"><a class="home" href="${createLinkTo(dir:'')}">Home</a></span>
            <span class="menuButton"><g:link class="list" action="list">Project List</g:link></span>
            <span class="menuButton"><g:link class="create" action="create">New Project</g:link></span>
        </div>
        <div class="body">
            <h1>Show Project</h1>
            <g:if test="${flash.message}">
            <div class="message">${flash.message}</div>
            </g:if>
            <div class="dialog">
                <table>
                    <tbody>
                    
                        <tr class="prop">
                            <td valign="top" class="name">Id:</td>
                            
                            <td valign="top" class="value">${project.id}</td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">Title:</td>
                            
                            <td valign="top" class="value">${project.title}</td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">Summary:</td>
                            
                            <td valign="top" class="value">${project.summary}</td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">Begin Date:</td>
                            
                            <td valign="top" class="value">${project.beginDate}</td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">End Date:</td>
                            
                            <td valign="top" class="value">${project.endDate}</td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">Minimum Latitude:</td>
                            
                            <td valign="top" class="value">${project.minimumLatitude}</td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">Maximum Latitude:</td>
                            
                            <td valign="top" class="value">${project.maximumLatitude}</td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">Minimum Longitude:</td>
                            
                            <td valign="top" class="value">${project.minimumLongitude}</td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">Maximum Longitude:</td>
                            
                            <td valign="top" class="value">${project.maximumLongitude}</td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">Pi Contact:</td>
                            
                            <td valign="top" class="value"><g:link controller="contact" action="show" id="${project?.piContact?.id}">${project?.piContact}</g:link></td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">Datasets:</td>
                            
                            <td  valign="top" style="text-align:left;" class="value">
                                <ul>
                                <g:each var="d" in="${project.datasets}">
                                    <li><g:link controller="dataset" action="show" id="${d.id}">${d}</g:link></li>
                                </g:each>
                                </ul>
                            </td>
                            
                        </tr>
                    
                        <tr class="prop">
                            <td valign="top" class="name">Nsf Award Number:</td>
                            
                            <td valign="top" class="value">${project.nsfAwardNumber}</td>
                            
                        </tr>
                    
                    </tbody>
                </table>
            </div>
            <div class="buttons">
                <g:form controller="project">
                    <input type="hidden" name="id" value="${project?.id}" />
                    <span class="button"><g:actionSubmit class="edit" value="Edit" /></span>
                    <span class="button"><g:actionSubmit class="delete" onclick="return confirm('Are you sure?');" value="Delete" /></span>
                </g:form>
            </div>
        </div>
    </body>
</html>
