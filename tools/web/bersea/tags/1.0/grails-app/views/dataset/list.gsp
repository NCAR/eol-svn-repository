

<html>
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
        <meta name="layout" content="main" />
        <title>Dataset List</title>
    </head>
    <body>
        <div class="nav">
            <span class="menuButton"><a class="home" href="${createLinkTo(dir:'')}">Home</a></span>
            <span class="menuButton"><g:link class="create" action="create">New Dataset</g:link></span>
        </div>
        <div class="body">
            <h1>Dataset List</h1>
            <g:if test="${flash.message}">
            <div class="message">${flash.message}</div>
            </g:if>
            <div class="list">
                <table>
                    <thead>
                        <tr>
                        
                   	        <g:sortableColumn property="title" title="Title" />
                   	    
                   	        <g:sortableColumn property="project" title="Project" />
                        
                   	        <g:sortableColumn property="cruise" title="Cruise" />
                        
                        </tr>
                    </thead>
                    <tbody>
                    <g:each in="${datasetList}" status="i" var="dataset">
                        <tr class="${(i % 2) == 0 ? 'odd' : 'even'}">
                        
                            <td><g:link action="show" id="${dataset.id}">${fieldValue(bean:dataset, field:'title')}</g:link></td>
                        
                            <td>${fieldValue(bean:dataset, field:'project')}</td>
                        
                            <td>${fieldValue(bean:dataset, field:'cruise')}</td>
                        
                        </tr>
                    </g:each>
                    </tbody>
                </table>
            </div>
            <div class="paginateButtons">
                <g:paginate total="${Dataset.count()}" />
            </div>
        </div>
    </body>
</html>
