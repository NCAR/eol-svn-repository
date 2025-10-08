  
<html>
  <head>
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
    <meta name="layout" content="main" />
    <title>Investigator List</title>
  </head>
  <body>
    <div class="nav">
      <span class="menuButton"><a href="${createLinkTo(dir:'')}">Home</a></span>
	  <g:ifAllGranted role="ROLE_USER,ROLE_ADMIN">      
      	<span class="menuButton"><g:link action="create">New Researcher</g:link></span>
	  </g:ifAllGranted>
    </div>
    <div class="body">
      <h1>Investigator List</h1>
      <g:if test="${flash.message}">
        <div class="message">
          ${flash.message}
        </div>
      </g:if>
      <table>
        <thead>
          <tr>
               
            <g:sortableColumn property="userRealName" title="Full Name" />
                  
            <g:sortableColumn property="organization" title="Organization" />
                  
            <g:sortableColumn property="description" title="Notes" />

            <th></th>
          </tr>
        </thead>
        <tbody>
          <g:each in="${personList}">
            <tr>
                       
              <td>${it.userRealName?.encodeAsHTML()}</td>
                       
              <td>${it.organization?.encodeAsHTML()}</td>
                       
              <td>${it.description?.encodeAsHTML()}</td>

              <td class="actionButtons">
                <span class="actionButton">
                  <g:link action="show" id="${it.id}">Show</g:link>
                </span>
              </td>
            </tr>
          </g:each>
        </tbody>
      </table>
    
      <div class="paginateButtons">
        <g:paginate total="${Researcher.count()}" />
      </div>
      
    </div>
  </body>
</html>
