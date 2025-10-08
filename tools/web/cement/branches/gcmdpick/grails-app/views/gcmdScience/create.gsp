  
<html>
    <head>
         <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
         <meta name="layout" content="main" />
         <title>Create GcmdScience</title>         
    </head>
    <body>
        <div class="nav">
            <span class="menuButton"><a href="${createLinkTo(dir:'')}">Home</a></span>
            <span class="menuButton"><g:link action="list">GcmdScience List</g:link></span>
        </div>
        <div class="body">
           <h1>Create GcmdScience</h1>
           <g:if test="${flash.message}">
                 <div class="message">${flash.message}</div>
           </g:if>
           <g:hasErrors bean="${gcmdScience}">
                <div class="errors">
                    <g:renderErrors bean="${gcmdScience}" as="list" />
                </div>
           </g:hasErrors>
           <g:form action="save" method="post" >
               <div class="dialog">
                <table>
                    <tbody>

                       
                       
                                  <tr class='prop'><td valign='top' class='name'><label for='keyword'>Keyword:</label></td><td valign='top' class='value ${hasErrors(bean:gcmdScience,field:'keyword','errors')}'><input type="text" name='keyword' value="${gcmdScience?.keyword?.encodeAsHTML()}"/></td></tr>
                       
                    </tbody>
               </table>
               </div>
               <div class="buttons">
                     <span class="formButton">
                        <input type="submit" value="Create"></input>
                     </span>
               </div>
            </g:form>
        </div>
    </body>
</html>
