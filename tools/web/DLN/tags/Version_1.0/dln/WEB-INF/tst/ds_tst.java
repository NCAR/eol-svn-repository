
import dln.dba.*;
import dln.beans.*;
import dln.format.*;
import java.util.*;

public class ds_tst
{
	public static void main( String a[] )
	{
		DisplayBean dis = new DisplayBean();
		dis.setDisplayView( DisplayBean.PROJECT );
		dis.setDisplayId( "SALLJEX" );
		dis.setSortField( DisplayBean.TITLE );
		
		Enumeration dss = DatasetDBA.getDatasets( dis ).elements();
		
		while( dss.hasMoreElements() )
		{
			DatasetBean ds = (DatasetBean) dss.nextElement();

			System.out.println( ds.getTitle() );
			System.out.println( "  " + ds.getLoaderBean().getFirstName() + "  " + ds.getCheckerBean().getFirstName() );
		}
		
	}
}
