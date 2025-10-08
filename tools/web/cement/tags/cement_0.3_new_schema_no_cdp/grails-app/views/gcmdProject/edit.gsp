  
<html>
    <head>
         <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
         <meta name="layout" content="main" />
         <title>Edit GcmdProject</title>
    </head>
    <body>
        <div class="nav">
            <span class="menuButton"><a href="${createLinkTo(dir:'')}">Home</a></span>
            <span class="menuButton"><g:link action="list">GcmdProject List</g:link></span>
            <span class="menuButton"><g:link action="create">New GcmdProject</g:link></span>
        </div>
        <div class="body">
           <h1>Edit GcmdProject</h1>
           <g:if test="${flash.message}">
                 <div class="message">${flash.message}</div>
           </g:if>
           <g:hasErrors bean="${gcmdProject}">
                <div class="errors">
                    <g:renderErrors bean="${gcmdProject}" as="list" />
                </div>
           </g:hasErrors>
           <div class="prop">
	      <span class="name">Id:</span>
	      <span class="value">${gcmdProject?.id}</span>
           </div>           
           <g:form controller="gcmdProject" method="post" >
               <input type="hidden" name="id" value="${gcmdProject?.id}" />
               <div class="dialog">
                <table>
                    <tbody>

                       
                       
				<tr class='prop'><td valign='top' class='name'><label for='keyword'>Keyword:</label></td><td valign='top' class='value ${hasErrors(bean:gcmdProject,field:'keyword','errors')}'><input type="text" name='keyword' value="${gcmdProject?.keyword?.encodeAsHTML()}"/></td></tr>
                       
                    </tbody>
                </table>
               </div>

               <div class="buttons">
                     <span class="button"><g:actionSubmit value="Update" /></span>
                     <span class="button"><g:actionSubmit value="Delete" /></span>
               </div>
            </g:form>
        </div>
    </body>
</html>
