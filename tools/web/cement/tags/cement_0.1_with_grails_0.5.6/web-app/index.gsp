<g:if test="${!session.email}"> 
	<%response.sendRedirect(request.getContextPath()+'/user/login')%>
</g:if> 
<g:else> 
<html>
    <head>
        <title>Welcome to the CADIS Metadata Entry Tool</title>
		<meta name="layout" content="main" />
    </head>
    <body>
        <h1 style="margin-left:20px;">Welcome to CADIS Metadata Entry Tool</h1>
        <p style="margin-left:20px;width:80%">Below is a list of controllers,
        click on each to execute its default action:</p>
        <div class="dialog" style="margin-left:20px;width:60%;">
            <ul>
                    <li class="controller"><a href="dataset">Datasets</a></li>
                    <li class="controller"><a href="contact">Contacts</a></li>
            </ul>
        </div>
    </body>
</html>
</g:else> 