completed with
${goodFiles.size()} good files saved
<ul>
<g:each in="${goodFiles}">
<li>${it}
</g:each>
</ul>
and ${badFiles.size()} bad files ignored
<ul>
<g:each in="${badFiles}">
<li>${it}
</g:each>
</ul>
