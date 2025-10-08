
import java.util.*;
import dln.beans.*;
import dln.dba.*;

public class tst
{
	public static void main( String args[] )
	{
		Vector v = UserDBA.getAllUsers();
		for( int x = 0; x < v.size(); x++ )
		{
			UserBean user = (UserBean) v.get(x);
			System.out.println( user.getUid() );
		}		
	}
}
