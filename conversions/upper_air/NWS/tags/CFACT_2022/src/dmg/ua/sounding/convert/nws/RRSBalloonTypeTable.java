package dmg.ua.sounding.convert.nws;

/**
 * <p>The RRSBalloonTypeTable is the RRSCodeTable that holds the information
 * for the RRS Balloon Type from <code>Code Table 9-10a</code> from 
 * <a href="http://dmg.eol.ucar.edu/software/tools/upper_air/NWS_RRS_bufr_extractor/NWS_RRS_reference.doc">
 * NWS_RRS_reference.doc</a>.</p>
 * <p>The table contains a mapping of the integral code values to their String textual
 * values.  It only defines values that are specifically defined in the RRS reference
 * document.  (It does not include any reserved values.)</p>
 *
 * @author Joel Clawson
 * @version 1.01 This initial version of the table.
 * 
 * @since 1.01
 **/
public class RRSBalloonTypeTable extends RRSCodeTable {

    /**
     * Create a new instance of a RRSBalloonTypeTable.
     **/
    public RRSBalloonTypeTable() { super(); }

    /**
     * Populate the table with the code value pairs from Code Table 9-10a of the
     * NWS RRS reference document.
     **/
    public void populate() {
        put(-9, "Unknown");
	put(0, "GP26");
	put(1, "GP28");
	put(2, "GP30");
	put(3, "HM26");
	put(4, "HM28");
	put(5, "HM30");
	put(6, "SV16");
	put(30, "Other");
	put(31, "Missing");
    }
}
