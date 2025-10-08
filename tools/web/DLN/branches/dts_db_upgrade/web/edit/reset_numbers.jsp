<%---------------------------------------------------------------
  reset_numbers.jsp:  Resets some numbers in the DisplayBean.  

  Before use of the <%@ include .. %> directive:
   -The DisplayBean 'display' must be initialized in session scope
   -The DatasetBean 'dataset' must be initialized in request scope
 
  Session/application beans used:
   display
	 dataset

  Author: Dan Sullivan
  Date: 1-3-2003 
---------------------------------------------------------------%>

<%@ page import="java.util.*,dln.format.*,dln.beans.*,dln.dba.*"%>

<%
	Vector dss = DatasetDBA.getDatasets( display );
%>

<%
	display.setDatasetCount( dss.size() );
	Enumeration edss = dss.elements();
	int n = -1;
	int c = 0;

	while( edss.hasMoreElements() )
	{
		DatasetBean ds = (DatasetBean) edss.nextElement();

		if( dataset.getDatasetId().equals(ds.getDatasetId()) )
		{
			n = c;
			break;
		}
		else
			c++;
	}

	if( n == -1 && display.getCurrentDatasetNumber() != 0 )
		n = display.getCurrentDatasetNumber() - 1;
	else if( n == -1 )
		n = 0;
	
	if( n > display.getDatasetCount() - 1 )
		display.setCurrentDatasetNumber( display.getDatasetCount() - 1 );
	else
		display.setCurrentDatasetNumber( n );

	if( display.getCurrentDatasetNumber() < 0 )
		display.setCurrentView( DisplayBean.LISTING );
%>
