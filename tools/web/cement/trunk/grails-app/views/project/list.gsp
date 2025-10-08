<html>
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
        <meta name="layout" content="main" />
        <title>Project List</title>
    </head>
    <body>
        <div class="nav" align="center">
            <g:render template="return_nav" />
        </div>

        <div class="body">
            <h1>Project List</h1>
            <g:if test="${flash.message}">
            <div class="message">${flash.message}</div>
            </g:if>
            <div class="list">
                <table>
                    <thead>
                        <tr>
                        
                   	        <g:sortableColumn property="piContact" title="PI" />

                   	        <g:sortableColumn property="title" title="Title" />
                        
                        </tr>
                    </thead>
                    <tbody>
                    <g:each in="${projectList}" status="i" var="project">
                        <tr class="${(i % 2) == 0 ? 'odd' : 'even'}">
                        
                            <td>${project.piContact?.encodeAsHTML()}</td>

                            <td><g:link action="show" id="${project.id}">${project.title?.encodeAsHTML()}</g:link></td>
                        
                        </tr>
                    </g:each>
                    </tbody>
                </table>
            </div>
            <div class="paginateButtons">
                <g:paginate total="${Project.count()}" />
            </div>
        </div>
    </body>
</html>
