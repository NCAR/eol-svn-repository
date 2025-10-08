package dmg.ncepemc;

/**
 * The NcepException class is a specialized exception used for
 * ingesting the NCEP/EMC data.
 *
 * @author Joel Clawson
 **/
public class NcepException extends Exception {

    /**
     * Create a new NcepException.
     * @param msg The exception message.
     **/
    public NcepException(String msg) { super(msg); }
}
