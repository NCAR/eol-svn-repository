
import dln.beans.*;
import dln.dba.*;
import java.util.*;

public class ds_tst2
{
	public static void main( String a[] )
	{
		DatasetBean ds = new DatasetBean() ;
		DatasetDBA.getFromDB( ds, 1061 );

		StringTokenizer st = new StringTokenizer( ds.getNotes(), "\r\n" );

		while( st.hasMoreTokens() )
		{
			System.out.println( "token: " + st.nextToken() );
		}
	}
}
