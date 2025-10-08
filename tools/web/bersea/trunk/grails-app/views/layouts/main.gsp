<html>
    <head>
        <title><g:layoutTitle default="BEST/BSIERP" /></title>
        <link rel="stylesheet" href="${createLinkTo(dir:'css',file:'main.css')}" />
        <link rel="shortcut icon" href="${createLinkTo(dir:'images',file:'favicon.ico')}" type="image/x-icon" />
        <g:layoutHead />
        <g:javascript library="application" />				
    </head>
    <body>
        <div id="spinner" class="spinner" style="display:none;">
            <img src="${createLinkTo(dir:'images',file:'spinner.gif')}" alt="Spinner" />
        </div>	
        <div class="logo">
            <table width="100%" border="0">
              <tr>
                <td>&nbsp;</td>
                <td>&nbsp;</td>
                <td>&nbsp;</td>
              </tr>
            </table>
			<div align="center"><img src="${createLinkTo(dir:'images',file:'nprb_logo.jpg')}" alt="NPRB" width="132" height="132" align="absbottom" /> &nbsp;&nbsp;&nbsp;&nbsp;
						  <img src="${createLinkTo(dir:'images',file:'BEST_logo.jpg')}" alt="BEST" width="132" height="132" align="absbottom" />
			</div>	
		</div>
        <g:layoutBody />		
    </body>	
</html>