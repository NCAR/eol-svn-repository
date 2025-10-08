package dmg.ua.sounding.convert.nws;

import java.util.*;

/**
 * <p>The RRSCodeTable is a generic table to contain code/value pairs from a
 * table in the
 * <a href="http://dmg.eol.ucar.edu/software/tools/upper_air/NWS_RRS_bufr_extractor/NWS_RRS_reference.doc">
 * NWS_RRS_reference.doc</a>.</p>
 * <p>The table should contain a  mapping of the integral code values to their String textual
 * values and only define values that are specifically defined in the RRS reference
 * document (not include any reserved values).</p>
 *
 * @author Joel Clawson
 * @version 1.01 This initial version of the table.
 *
 * @since 1.01
 **/
public abstract class RRSCodeTable extends TreeMap<Integer, String> {

    /**
     * Create a new instance of a RRSCodeTable.
     **/
    public RRSCodeTable() { populate(); }

    /**
     * Populate the table with the code/value pairs for the table.
     **/
    public abstract void populate();
} 
