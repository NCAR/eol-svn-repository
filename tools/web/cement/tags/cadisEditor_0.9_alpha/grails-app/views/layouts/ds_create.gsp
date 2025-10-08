<html>
<head>
<title><g:layoutTitle default="CADIS metadata" /></title>
<link type="text/css" rel="stylesheet" href="${createLinkTo(dir:'css',file:'ds_create.css')}" />
<g:layoutHead />
<g:javascript library="application" />				
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"
"http://www.w3.org/TR/html4/loose.dtd">
</head>

<body>
<div class="logo" align="right" ><img src="${createLinkTo(dir:'images',file:'cadis_logo.gif')}" alt="CADIS" /></div>	
<table width="100%" border="0" cellspacing="2" cellpadding="2" height="626" bordercolor="#CCCCCC">
  <tr>
    <td>
      <table width="100%" border="0" cellspacing="2" cellpadding="2" height="583" align="center">
        <tr>
          <td width="20%" align="left" valign="top">
  	    <p>
            <div id="main" style="text-align: left"> 
  	    <br>
  	    <br>
  	    <br>
  	    <br>
  	    <br>
  	    <br>
  	    <br>
  	    <br>
  	    <br>
  	    <br>
              <!--	*** side menu links ***	-->
              <div id="links"> 
                <div id="button"> 
                  <ul>
                    <li class="top">
                      <p>Contact Info
                    </li>
                    <li><a href="/cadisEditor/contact/myinfo">My profile.</a></li>
                  </ul>
				  
                  <ul>
                    <li class="top">
                      <p>Metadata for Data Sets
                    </li>
                    &nbsp;&nbsp; &lArr;
                    <li bgcolor="red"><a href="/cadisEditor/dataset/create">New Submission</a></li>
                    <li><a href="/cadisEditor">Previously Submitted</a></li>
                  </ul>
                  <ul>
                    <li class="top">
                      <p>Data Files
                    </li>
                    <li><a href="">Upload Data Files for a Data Set</a></li>
                  </ul>
		  <br>
		  <ul>
			<li><a href="/cadisEditor/cdp/exit">CADIS Home</a></li>
			<li><a href="/cadisEditor/css/help.html">Help</a></li>
		  </ul>

                </div>
              </div>
            </div>
          </td>
          <td width="80%">
		<g:layoutBody />		
	 </td>
        </tr>
      </table>
    </td>
  </tr>
</table>
</body>
</html>
