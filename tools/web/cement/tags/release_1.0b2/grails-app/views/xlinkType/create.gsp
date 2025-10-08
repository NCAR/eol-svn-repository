

<html>
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
        <meta name="layout" content="main" />
        <title>Create XlinkType</title>         
    </head>
    <body>
        <div class="nav">
            <span class="menuButton"><a class="home" href="${createLinkTo(dir:'')}">Home</a></span>
            <span class="menuButton"><g:link class="list" action="list">XlinkType List</g:link></span>
        </div>
        <div class="body">
            <h1>Create XlinkType</h1>
            <g:if test="${flash.message}">
            <div class="message">${flash.message}</div>
            </g:if>
            <g:hasErrors bean="${xlinkType}">
            <div class="errors">
                <g:renderErrors bean="${xlinkType}" as="list" />
            </div>
            </g:hasErrors>
            <g:form action="save" method="post" >
                <div class="dialog">
                    <table>
                        <tbody>
                        
                            <tr class="prop">
                                <td valign="top" class="name">
                                    <label for="purpose">Purpose:</label>
                                </td>
                                <td valign="top" class="value ${hasErrors(bean:xlinkType,field:'purpose','errors')}">
                                    <input type="text" id="purpose" name="purpose" value="${fieldValue(bean:xlinkType,field:'purpose')}"/>
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
    </body>
</html>
