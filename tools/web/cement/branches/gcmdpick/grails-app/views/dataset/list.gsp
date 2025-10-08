  
<html>
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
        <meta name="layout" content="ds_create" />
        <title>Dataset List</title>
    </head>
    <body>
        <div class="body">
            <h1>Dataset List</h1>
            <g:if test="${flash.message}">
            <div class="message">${flash.message}</div>
            </g:if>
            <div class="list">
                <table>
                    <thead>
                        <tr>
                        
                   	        <g:sortableColumn property="piContact" title="PI" />
                        
                   	        <g:sortableColumn property="title" title="Title" />
                        
                   	        <g:sortableColumn property="beginDate" title="Begin Date" />
                        
                   	        <g:sortableColumn property="endDate" title="End Date" />
                        
                        </tr>
                    </thead>
                    <tbody>
                    <g:each in="${datasetList}" status="i" var="dataset">
                        <tr class="${(i % 2) == 0 ? 'odd' : 'even'}">
                        
                            <td>${dataset?.project?.piContact?.personName?.encodeAsHTML()}</td>
                        
                            <td><g:link style="display:block" controller="dataset" action="edit" id="${dataset.id}">${dataset.title?.encodeAsHTML()}</g:link>
                        
                            <td>${dataset.beginDate?.encodeAsHTML()}</td>
                        
                            <td>${dataset.endDate?.encodeAsHTML()}</td>
                        
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
