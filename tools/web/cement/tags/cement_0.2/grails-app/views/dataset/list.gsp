  
<html>
    <head>
         <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
         <meta name="layout" content="main" />
         <title>Dataset List</title>
    </head>
    <body>
        <div class="nav">
		    <g:render template="/adminmenubar" /> 
        </div>
        <div class="body">
           <h1>Dataset List</h1>
            <g:if test="${flash.message}">
                 <div class="message">
                       ${flash.message}
                 </div>
            </g:if>
           <table>
             <thead>
               <tr>
               
                   	    <g:sortableColumn property="piContact" title="PI" />
                  
                   	    <g:sortableColumn property="title" title="Title" />
                  
                   	    <g:sortableColumn property="beginDate" title="Begin Date" />
                  
                   	    <g:sortableColumn property="endDate" title="End Date" />
                  
                        <th></th>
               </tr>
             </thead>
             <tbody>
               <g:each in="${datasetList}">
                    <tr>
                       
                            <td>${it.piContact.personName?.encodeAsHTML()}</td>
                       
                            <td>${it.title?.encodeAsHTML()}</td>
                       
                            <td><g:formatDate date="${it.beginDate}" format="yyyy-MMM-dd"/> </td>
                       
                            <td><g:formatDate date="${it.endDate}" format="yyyy-MMM-dd"/> </td>
                       
                       <td class="actionButtons">
                            <span class="actionButton"><g:link action="show" id="${it.id}">Show</g:link></span>
                       </td>
                    </tr>
               </g:each>
             </tbody>
           </table>
               <div class="paginateButtons">
                   <g:paginate total="${Dataset.count()}" />
               </div>
        </div>
    </body>
</html>
