  
<html>
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
        <meta name="layout" content="ds_create" />
        <title>Edit GcmdPlatform</title>
    </head>
    <body>
        <div class="nav" align="center">
            <span class="menuButton"><g:link class="list" action="list">GcmdPlatform List</g:link></span>
            <span class="menuButton"><g:link class="create" action="create">New GcmdPlatform</g:link></span>
        </div>
        <div class="body">
            <h1>Edit GcmdPlatform</h1>
            <g:if test="${flash.message}">
            <div class="message">${flash.message}</div>
            </g:if>
            <g:hasErrors bean="${gcmdPlatform}">
            <div class="errors">
                <g:renderErrors bean="${gcmdPlatform}" as="list" />
            </div>
            </g:hasErrors>
            <g:form controller="gcmdPlatform" method="post" >
                <input type="hidden" name="id" value="${gcmdPlatform?.id}" />
                <div class="dialog">
                    <table>
                        <tbody>
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='keyword'>Keyword:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:gcmdPlatform,field:'keyword','errors')}'>
                                    <input type="text" id='keyword' name='keyword' value="${fieldValue(bean:gcmdPlatform,field:'keyword')}"/>
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
