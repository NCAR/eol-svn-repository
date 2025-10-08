  
<html>
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
        <meta name="layout" content="main" />
        <title>Create Project</title>         
    </head>
    <body>
        <div class="nav" align="center">
            <span class="menuButton"><g:link class="list" action="list">Project List</g:link></span>
            <g:render template="return_nav" />
        </div>

        <div class="body">
            <h1>Create Project</h1>
            <g:if test="${flash.message}">
            <div class="message">${flash.message}</div>
            </g:if>
            <g:hasErrors bean="${project}">
            <div class="errors">
                <g:renderErrors bean="${project}" as="list" />
            </div>
            </g:hasErrors>
            <g:form action="save" method="post" >
                <div class="dialog">
                    <table>
                        <tbody>
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='title'>Title:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:project,field:'title','errors')}'>
									<input type='text' name='title' maxlength='255' size='80' value="${project?.title?.encodeAsHTML()}" />
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='summary'>Summary:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:project,field:'summary','errors')}'>
									<textarea style='height:auto; width:auto;' rows='16' cols='79' name='summary'>${project?.summary?.encodeAsHTML()}</textarea>
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='beginDate'>Begin Date:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:project,field:'beginDate','errors')}'>
                                    <g:datePicker name='beginDate' value="${project?.beginDate}" ></g:datePicker>
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='endDate'>End Date:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:project,field:'endDate','errors')}'>
                                    <g:datePicker name='endDate' value="${project?.endDate}" ></g:datePicker>
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='minimumLatitude'>Minimum Latitude:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:project,field:'minimumLatitude','errors')}'>
                                    <input type='text' id='minimumLatitude' name='minimumLatitude' value="${fieldValue(bean:project,field:'minimumLatitude')}" />
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='maximumLatitude'>Maximum Latitude:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:project,field:'maximumLatitude','errors')}'>
                                    <input type='text' id='maximumLatitude' name='maximumLatitude' value="${fieldValue(bean:project,field:'maximumLatitude')}" />
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='minimumLongitude'>Minimum Longitude:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:project,field:'minimumLongitude','errors')}'>
                                    <input type='text' id='minimumLongitude' name='minimumLongitude' value="${fieldValue(bean:project,field:'minimumLongitude')}" />
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='maximumLongitude'>Maximum Longitude:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:project,field:'maximumLongitude','errors')}'>
                                    <input type='text' id='maximumLongitude' name='maximumLongitude' value="${fieldValue(bean:project,field:'maximumLongitude')}" />
                                </td>
                            </tr> 

                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='discipline'>Discipline:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:project,field:'discipline','errors')}'>
                                    <g:select id='discipline' name='discipline' from='${project.constraints.discipline.inList.collect{it.encodeAsHTML()}}' value="${fieldValue(bean:project,field:'discipline')}"></g:select>
                                </td>
                            </tr> 

                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='piContact'>Pi Contact:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:project,field:'piContact','errors')}'>
                                    <g:select optionKey="id" from="${Contact.list().sort()}" name='piContact.id' value="${project?.piContact?.id}" ></g:select>
                                </td>
                            </tr> 

                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='nsfAwardNumber'>Nsf Award Number:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:project,field:'nsfAwardNumber','errors')}'>
                                    <input type="text" id='nsfAwardNumber' name='nsfAwardNumber' value="${fieldValue(bean:project,field:'nsfAwardNumber')}"/>
                                </td>
                            </tr> 
                        
                        </tbody>
                    </table>
                </div>
                <div class="buttons">
                    <span class="button"><input class="save" type="submit" value="Create"></input></span>
                </div>
            </g:form>
        </div>
    </body>
</html>
