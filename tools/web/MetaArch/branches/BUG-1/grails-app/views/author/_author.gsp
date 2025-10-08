<div id="author${i}" class='ui-sortable ui-droppable author-div <g:if test="${dsAuthor}"></g:if><g:else>author-new</g:else>' <g:if test="${hidden}">style="display:none;"</g:if>>
	<span id='authors.sort'>${dsAuthor?.sortKey}</span>.  
	<span id='authors.name' title='Author Name'>${dsAuthor?.author}</span>

	<g:if test="${dsAuthor?.author}">
		<g:hiddenField name='authors.sortKeys.${dsAuthor.author.id}' value='${dsAuthor.sortKey}'/>
		<g:hiddenField name='authors.deleted.${dsAuthor.author.id}' value='false'/>
		<g:hiddenField name='authors.new.${dsAuthor.author.id}' value='false'/>
	</g:if><g:else>
		<!-- Couldn't find the author for this dataset -->
		<g:hiddenField name='authors.sortKeys.${i}' value='${dsAuthor?.sortKey}'/>
		<g:hiddenField name='authors.deleted.${i}' value='false'/>
		<g:hiddenField name='authors.new.${i}' value='false'/>
	</g:else>
	
	<span class="del-author">
		<img src="${resource(dir:'images/skin', file:'database_delete.png')}" />
	</span>
</div>