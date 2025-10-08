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
                  <g:sortableColumn property="shortName" title="Short Name" />
                  <g:sortableColumn property="personName" title="Person Name" />
                  <g:sortableColumn property="organizationName" title="Organization Name" />
               </tr>
             </thead>
             <tbody>
               <g:each in="${contactList}" status="i" var="contact">
                 <tr class="${(i % 2) == 0 ? 'odd' : 'even'}">
                    <td>${contact.shortName?.encodeAsHTML()}</td>
                    <td><g:link action="show" id="${contact.id}">${contact.personName?.encodeAsHTML()}</g:link></td>
                    <td>${contact.organizationName?.encodeAsHTML()}</td>
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
