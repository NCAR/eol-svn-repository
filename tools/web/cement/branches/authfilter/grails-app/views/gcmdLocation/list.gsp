  
<html>
    <head>
         <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
         <meta name="layout" content="main" />
         <title>GcmdLocation List</title>
    </head>
    <body>
        <div class="nav">
            <span class="menuButton"><a href="${createLinkTo(dir:'')}">Home</a></span>
            <span class="menuButton"><g:link action="create">New GcmdLocation</g:link></span>
        </div>
        <div class="body">
           <h1>GcmdLocation List</h1>
            <g:if test="${flash.message}">
                 <div class="message">
                       ${flash.message}
                 </div>
            </g:if>
           <table>
             <thead>
               <tr>
               
                   	    <g:sortableColumn property="id" title="Id" />
                  
                   	    <g:sortableColumn property="keyword" title="Keyword" />
                  
                        <th></th>
               </tr>
             </thead>
             <tbody>
               <g:each in="${gcmdLocationList}">
                    <tr>
                       
                            <td>${it.id?.encodeAsHTML()}</td>
                       
                            <td>${it.keyword?.encodeAsHTML()}</td>
                       
                       <td class="actionButtons">
                            <span class="actionButton"><g:link action="show" id="${it.id}">Show</g:link></span>
                       </td>
                    </tr>
               </g:each>
             </tbody>
           </table>
               <div class="paginateButtons">
                   <g:paginate total="${GcmdLocation.count()}" />
               </div>
        </div>
    </body>
</html>
