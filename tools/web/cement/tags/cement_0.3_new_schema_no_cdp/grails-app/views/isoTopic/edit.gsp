  
<html>
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
        <meta name="layout" content="main" />
        <title>Edit IsoTopic</title>
    </head>
    <body>
        <div class="nav">
            <span class="menuButton"><a class="home" href="${createLinkTo(dir:'')}">Home</a></span>
            <span class="menuButton"><g:link class="list" action="list">IsoTopic List</g:link></span>
            <span class="menuButton"><g:link class="create" action="create">New IsoTopic</g:link></span>
        </div>
        <div class="body">
            <h1>Edit IsoTopic</h1>
            <g:if test="${flash.message}">
            <div class="message">${flash.message}</div>
            </g:if>
            <g:hasErrors bean="${isoTopic}">
            <div class="errors">
                <g:renderErrors bean="${isoTopic}" as="list" />
            </div>
            </g:hasErrors>
            <g:form controller="isoTopic" method="post" >
                <input type="hidden" name="id" value="${isoTopic?.id}" />
                <div class="dialog">
                    <table>
                        <tbody>
                        
                            <tr class='prop'>
                                <td valign='top' class='name'>
                                    <label for='keyword'>Keyword:</label>
                                </td>
                                <td valign='top' class='value ${hasErrors(bean:isoTopic,field:'keyword','errors')}'>
                                    <input type="text" id='keyword' name='keyword' value="${fieldValue(bean:isoTopic,field:'keyword')}"/>
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
