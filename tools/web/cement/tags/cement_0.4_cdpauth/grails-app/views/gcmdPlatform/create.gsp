  
<html>
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
        <meta name="layout" content="main" />
        <title>Create GcmdPlatform</title>         
    </head>
    <body>
        <div class="nav">
            <span class="menuButton"><a class="home" href="${createLinkTo(dir:'')}">Home</a></span>
            <span class="menuButton"><g:link class="list" action="list">GcmdPlatform List</g:link></span>
        </div>
        <div class="body">
            <h1>Create GcmdPlatform</h1>
            <g:if test="${flash.message}">
            <div class="message">${flash.message}</div>
            </g:if>
            <g:hasErrors bean="${gcmdPlatform}">
            <div class="errors">
                <g:renderErrors bean="${gcmdPlatform}" as="list" />
            </div>
            </g:hasErrors>
            <g:form action="save" method="post" >
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
                    <span class="button"><input class="save" type="submit" value="Create"></input></span>
                </div>
            </g:form>
        </div>
    </body>
</html>
