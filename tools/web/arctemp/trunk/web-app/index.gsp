<html>
    <head>
        <title>Welcome to BEST</title>
		<meta name="layout" content="main" />
    </head>
    <body>
		<p>&nbsp;&nbsp;</p>
		<p>&nbsp;&nbsp;</p>
        <h1 style="text-align:center;">Welcome to the NCAR/EOL BEST metadata templates</h1>
        <p style="text-align:center"> Click on the links below to display the items in the related database table:</p>
		<p>&nbsp;&nbsp;</p>
        <div class="dialog" style="text-align:center;">
            <ul>
              <g:each var="c" in="${grailsApplication.controllerClasses}">
                    <li class="controller"><a href="${c.logicalPropertyName}">${c.fullName}</a></li>
              </g:each>
<!--		  	<li class="controller"><a href="/arctemp/contact">Contact</a></li><br />
				<li class="controller"><a href="/arctemp/project">Project</a></li><br /> 
				<li class="controller"><a href="/arctemp/dataset">Dataset</a></li><br />
-->
            </ul>
        </div>
		<p>&nbsp;&nbsp;</p>
		<p>&nbsp;&nbsp;</p>
		<p style="text-align:center;">Click on the row's ID to see that record</p> 
		<p style="text-align:center;">Click on "New Dataset" in Dataset to see the metadata entry form</p>
    </body>
</html>