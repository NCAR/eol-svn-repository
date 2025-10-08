<html>
<head>
<title>Edit info for CDP</title>
<meta name="layout" content="main" />
</head>
<body>
<div class="body">
<h1>Edit info for CDP</h1>
<g:if test="${flash.message}">
<div class="message">${flash.message}</div>
</g:if>
Add some text to pass back to the CDP:
<g:form controller="work" method="post">
<div class="dialog">
<span class="prop"><label for='cdpid'>ID:</label><input type="text" name='cdpid' value="${cdpid?.encodeAsHTML()}" /></span><br />
<span class="prop"><label for='cdptext'>Text:</label><input type="text" name='cdptext' value="${cdptext?.encodeAsHTML()}" /></span><br />
<span class="prop">Redirect to:<br />
<input type="radio" name="redir" value="echo" checked="checked" /> Echo page<br />
<input type="radio" name="redir" value="cdp" /> CDP cadisEditorStoreThredds<br />
</span>
</div>
<div class="buttons">
<span class="button"><g:actionSubmit class="save" action="Done" value="Send to CDP" /></span>
</div>
</g:form>
</div>
</body>
</html>
