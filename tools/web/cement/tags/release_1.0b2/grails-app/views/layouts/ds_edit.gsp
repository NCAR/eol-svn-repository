<html>
<head>
<title><g:layoutTitle default="CADIS metadata" /></title>
<link type="text/css" rel="stylesheet" href="${createLinkTo(dir:'css',file:'cadis.css')}" />
<g:layoutHead />
<g:javascript library="application" />				
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"
"http://www.w3.org/TR/html4/loose.dtd">
</head>

<body>
<table align="center" border="0" cellpadding="0" cellspacing="0" width="100%" height="726">
  <tbody>
  <tr>
    <td align="center" height="120">
      <table border="0" cellpadding="0" cellspacing="0" width="100%" height="97" align="right">
	<tbody>
        <tr>
		  <td bgcolor="#ebf6ff" width="67%" height="90">&nbsp;</td>
          <td align="center" bgcolor="#ebf6ff"width-"31%" valign="bottom" height="88"><img src="${createLinkTo(dir:'images',file:'cadishead_small.png')}" alt="CADIS" />
          </td>
        </tr>
     </tbody>

      </table>
    </td>
  </tr>

  <tr>
    <td align="center">
  	  <table cellpadding="2" cellspacing="2" width="100%">
  		<tbody>
        <tr>
  			
          <td align="center">

            <!-- MAIN TABLE -->
            <table align="center" border="0" width="99%" height="725">
			<tbody>
              <tr>
                <td>
   	              <table align="center" border="0" cellpadding="2" cellspacing="2" width="100%">
				<tbody>
		            <tr>
                      <td>
			
 					  </td>
                    </tr>
                    <tr>
                      <td>
                        <table width="100%" height="700">
    	
    					<tbody>
                          <tr>
    		
    		                <td align="center" valign="top" width="137">

                              <div id="main" style="text-align: left;"> 
								<br>

                                <!--	*** side menu links ***	-->

		                        <div id="links"> 
		                          <div id="button"> 
                                    
                                    <ul>
                                      <font face="Arial, Helvetica, sans-serif">		
                                      <li class="top">Contacts</li>
                                      <li><a href="/cadisEditor/contact/myinfo">My Profile</a></li>
                                      </font>
                                    </ul>
                                    <br>
                                    <ul>
									<font face="Arial, Helvetica, sans-serif">
                                      <li class="top">Metadata for Datasets</li>
                                      <li><a href="/cadisEditor/dataset/create">New submission</a></li>
                                      <li><a href="/cadisEditor">Previously submitted</a></li>
                                      </font>
                                    </ul>
                                    <br>
								    <ul>
									<font face="Arial, Helvetica, sans-serif">
                                      <li class="top">Editing</li>
                    				  <li><a href="/cadisEditor">Cancel editing</a></li>
        							  <li><a><span style="border-width: 3px; padding: 3px 0.2em; color: gray; font-size: 8.5pt; font-weight: bold; text-align: center;">Submit metadata</span></a></li>
                                    <br>
                                      <li class="top">Links</li>
									  <li><a href="/cadisEditor/help-files/help.html" target="_blank">Help</a></li>
									  <li><a href="/cadisEditor/cdp/exit">CADIS Home</a></li>
                                      </font>
								    </ul>
                             </td>

							<td  align="left" valign="top" width="7">
								&nbsp;
							</td>

                            <td style="vertical-align: top;" width="728">&nbsp;&nbsp;&nbsp;
								<g:layoutBody />		
          	                </td>

                          </tr>
	  					</tbody>
                        </table>
                      </td>
                    </tr>
				   </tbody>
                  </table>
                </td>
              </tr>
              </tbody>
            </table>
          </td>
        </tr>
  	   </tbody>
      </table>
    </td>
  </tr>
  </tbody>
</table>
</body>
</html>
