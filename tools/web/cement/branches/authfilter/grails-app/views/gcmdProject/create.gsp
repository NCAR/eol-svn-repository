  
<html>
    <head>
         <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
         <meta name="layout" content="main" />
         <title>Create GcmdProject</title>         
    </head>
    <body>
        <div class="nav">
            <span class="menuButton"><a href="${createLinkTo(dir:'')}">Home</a></span>
            <span class="menuButton"><g:link action="list">GcmdProject List</g:link></span>
        </div>
        <div class="body">
           <h1>Create GcmdProject</h1>
           <g:if test="${flash.message}">
                 <div class="message">${flash.message}</div>
           </g:if>
           <g:hasErrors bean="${gcmdProject}">
                <div class="errors">
                    <g:renderErrors bean="${gcmdProject}" as="list" />
                </div>
           </g:hasErrors>
           <g:form action="save" method="post" >
               <div class="dialog">
                <table>
                    <tbody>

                       
                       
                                  <tr class='prop'><td valign='top' class='name'><label for='keyword'>Keyword:</label></td><td valign='top' class='value ${hasErrors(bean:gcmdProject,field:'keyword','errors')}'><input type="text" name='keyword' value="${gcmdProject?.keyword?.encodeAsHTML()}"/></td></tr>
                       
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
