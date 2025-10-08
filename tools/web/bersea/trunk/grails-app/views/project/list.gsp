

<html>
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
        <meta name="layout" content="main" />
        <title>Project List</title>
    </head>
    <body>
        <div class="nav">
            <span class="menuButton"><a class="home" href="${createLinkTo(dir:'')}">Home</a></span>
            <span class="menuButton"><g:link class="create" action="create">New Project</g:link></span>
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
                        
                   	        <g:sortableColumn property="dataCenter" title="Data Center" />
                        
                   	        <g:sortableColumn property="title" title="Title" />
                        
                   	        <th>Pi</th>
                   	    
                   	        <g:sortableColumn property="fundingAgency" title="Funding Agency" />
                        
                   	        <g:sortableColumn property="awardNumber" title="Award Number" />
                        
                        </tr>
                    </thead>
                    <tbody>
                    <g:each in="${projectList}" status="i" var="project">
                        <tr class="${(i % 2) == 0 ? 'odd' : 'even'}">
                        
                            <td>${fieldValue(bean:project, field:'dataCenter')}</td>
                        
                            <td><g:link action="show" id="${project.id}">${fieldValue(bean:project, field:'title')}</g:link></td>
                        
                            <td>${fieldValue(bean:project, field:'pi')}</td>
                        
                            <td>${fieldValue(bean:project, field:'fundingAgency')}</td>
                        
                            <td>${fieldValue(bean:project, field:'awardNumber')}</td>
                        
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
