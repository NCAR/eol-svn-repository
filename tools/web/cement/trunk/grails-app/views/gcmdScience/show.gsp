  
<%@ defaultCodec="HTML" %>
<html>
    <head>
         <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
          <meta name="layout" content="main" />
         <title>Show GcmdScience</title>
    </head>
    <body>
        <div class="nav">
            <span class="menuButton"><a href="${createLinkTo(dir:'')}">Home</a></span>
            <span class="menuButton"><g:link action="list">GcmdScience List</g:link></span>
            <span class="menuButton"><g:link action="create">New GcmdScience</g:link></span>
        </div>
        <div class="body">
           <h1>Show GcmdScience</h1>
           <g:if test="${flash.message}">
                 <div class="message">${flash.message}</div>
           </g:if>
           <div class="dialog">
                 <table>
                   
                   <tbody>
                   
                        <tr class="prop">
                              <td valign="top" class="name">Id:</td>
                              
                                    <td valign="top" class="value">${gcmdScience.id}</td>
                              
                        </tr>
                   
                        <tr class="prop">
                              <td valign="top" class="name">Keyword:</td>
                              
                                    <td valign="top" class="value">${gcmdScience.keyword}</td>
                              
                        </tr>
                   
                   </tbody>
                 </table>
           </div>
           <div class="buttons">
               <g:form controller="gcmdScience">
                 <input type="hidden" name="id" value="${gcmdScience?.id}" />
                 <span class="button"><g:actionSubmit value="Edit" /></span>
                 <span class="button"><g:actionSubmit value="Delete" /></span>
               </g:form>
           </div>
        </div>
    </body>
</html>
