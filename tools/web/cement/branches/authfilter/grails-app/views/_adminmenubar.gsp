<g:if test="${!session.email}"> 
    <span class="menuButton"> 
        <g:link controller="user" action="login">Log in</g:link> 
    </span> 
</g:if> 
<g:else> 
    <span class="menuButton"><a href="http://dev.cdp.ucar.edu:3080/browse/browse.htm?uri=http://dataportal.ucar.edu/metadata/cadis/cadis.thredds.xml">CADIS Portal</a></span>
    <span class="menuButton"><a href="http://dev.cdp.ucar.edu:3080/security/doLogout.htm"> Log out </a></span> 
</g:else> 
