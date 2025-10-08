<html>
    <head>
        <title>Hello, CDP!</title>
    </head>
    <body>
        <h1>Hello, CDP!</h1>

<p>The <g:link controller="cdp" action="entry">CDP entry point</g:link>
can maybe find a SessionData if the CDP is using a JSESSIONID cookie on path /.
If not, it will redirect you to the CDP login (which won't redirect you back
so open in a new tab/window).
If you're already logged in, then inspect the CDP stdout or your cookies and use this form:
<g:form controller="cdp" action="entry" method="POST">
JSESSIONID: <input type="text" name="sid" value="" size="32" maxlength="32" /><br />
<g:actionSubmit class="save" action="Entry" value="Go to the Cadis Editor test" />
<input type="reset" name="reset" value="Clear" />
</g:form>
</p>

<p>The original index test <a href="list.gsp">lister gsp</a> using SessionData is also available.
<form action="list.gsp" method="GET">
JSESSIONID: <input type="text" name="sid" value="" size="32" maxlength="32" /><br />
<input type="submit" name="submit" value="List objects" />
<input type="reset" name="reset" value="Clear" />
</form>
</p>

    </body>
</html>
