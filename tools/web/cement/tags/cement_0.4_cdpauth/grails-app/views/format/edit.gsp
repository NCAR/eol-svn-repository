  
<html>
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
        <meta name="layout" content="main" />
        <title>Edit Format</title>
    </head>
    <body>
        <div class="nav">
            <span class="menuButton"><a class="home" href="${createLinkTo(dir:'')}">Home</a></span>
            <span class="menuButton"><g:link class="list" action="list">Format List</g:link></span>
            <span class="menuButton"><g:link class="create" action="create">New Format</g:link></span>
        </div>
        <div class="body">
            <h1>Edit Format</h1>
            <g:if test="${flash.message}">
            <div class="message">${flash.message}</div>
            </g:if>
            <g:hasErrors bean="${format}">
            <div class="errors">
                <g:renderErrors bean="${format}" as="list" />
            </div>
            </g:hasErrors>
            <g:form controller="format" method="post" >
                <input type="hidden" name="id" value="${format?.id}" />
                <div class="dialog">
                    <table>
                        <tbody>
 
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='name'>Name:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:format,field:'name','errors')}'>
                                    <input type="text" id='name' name='name' value="${fieldValue(bean:format,field:'name')}"/>
                                </td>
                            </tr> 
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='description'>Description:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:format,field:'description','errors')}'>
                                    <input type="text" id='description' name='description' value="${fieldValue(bean:format,field:'description')}"/>
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
