<div id="xlink${i}" class='xlink-div <g:if test="${xlink}"></g:if><g:else>xlink-new</g:else>' <g:if test="${hidden}">style="display:none;"</g:if>>
	<g:if test="${xlink}">
	<span>
		${xlink}
	</span>
	</g:if>
	<g:else><span>New URL:</span></g:else>
	<br />

	<g:hiddenField name='xlinks[${i}].id' value='${xlink?.id}'/>
	<g:hiddenField name='xlinks[${i}].deleted' value='false'/>
	<g:hiddenField name='xlinks[${i}].new' value='false'/>
	
	<g:select name='xlinks[${i}].type' from="${meta.XlinkType.values()}" optionKey="key" value="${xlink?.type?.key}" />
	Title: 
	<g:textField title='URL Title' name='xlinks[${i}].title' value='${xlink?.title}'/>
	URL: 
	<g:textField title='URL Href' id='xlinks.href' name='xlinks[${i}].href' value='${xlink?.href}'/>
	
	<span class="del-xlink">
		<img src="${resource(dir:'images/skin', file:'database_delete.png')}" />
	</span>
</div>