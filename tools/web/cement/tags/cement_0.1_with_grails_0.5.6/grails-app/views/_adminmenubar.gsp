<g:if test="${!session.email}"> 
    <span class="menuButton"> 
        <g:link controller="user" action="login">Log in</g:link> 
    </span> 
</g:if> 
<g:else> 
    <span class="menuButton"><a href="${createLinkTo(dir:'')}">Home</a></span>
	<span class="menuButton"><g:link action="list">Contact List</g:link></span>
    <span class="menuButton"> 
        <g:link controller="user" action="logout"> 
            Log out 
        </g:link> 
    </span> 
</g:else> 
