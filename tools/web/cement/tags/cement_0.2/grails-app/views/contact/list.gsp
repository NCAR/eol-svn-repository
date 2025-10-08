  
<html>
    <head>
         <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
         <meta name="layout" content="main" />
         <title>Contact List</title>
    </head>
    <body>
        <div class="nav">
            <span class="menuButton"><a href="${createLinkTo(dir:'')}">Home</a></span>
            <span class="menuButton"><g:link action="create">New Contact</g:link></span>
            <g:render template="/adminmenubar" /> 
        </div>
        <div class="body">
           <h1>Contact List</h1>
            <g:if test="${flash.message}">
                 <div class="message">
                       ${flash.message}
                 </div>
            </g:if>
           <table>
             <thead>
               <tr>
               
                   	    <g:sortableColumn property="personName" title="Person Name" />
                  
                   	    <g:sortableColumn property="organizationName" title="Organization Name" />
                  
                   	    <g:sortableColumn property="shortName" title="Short Name" />
                  
                   	    <g:sortableColumn property="address" title="Address" />
                  
                   	    <g:sortableColumn property="city" title="City" />
                  
                        <th></th>
               </tr>
             </thead>
             <tbody>
               <g:each in="${contactList}">
                    <tr>
                       
                            <td>${it.personName?.encodeAsHTML()}</td>
                       
                            <td>${it.organizationName?.encodeAsHTML()}</td>
                       
                            <td>${it.shortName?.encodeAsHTML()}</td>
                       
                            <td>${it.address?.encodeAsHTML()}</td>
                       
                            <td>${it.city?.encodeAsHTML()}</td>
                       
                       <td class="actionButtons">
                            <span class="actionButton"><g:link action="show" id="${it.id}">Show</g:link></span>
                       </td>
                    </tr>
               </g:each>
             </tbody>
           </table>
               <div class="paginateButtons">
                   <g:paginate total="${Contact.count()}" />
               </div>
        </div>
    </body>
</html>
