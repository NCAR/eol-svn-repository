package dmg.ua.sounding.convert.nws;

/**
 * <p>The RRSBalloonManufacturerTable is the RRSCodeTable that holds the information
 * for the RRS Balloon Manufacturer from <code>Code Table 9-9</code> from 
 * <a href="http://dmg.eol.ucar.edu/software/tools/upper_air/NWS_RRS_bufr_extractor/NWS_RRS_reference.doc">
 * NWS_RRS_reference.doc</a>.</p>
 * <p>The table contains a mapping of the integral code values to their String textual
 * values.  It only defines values that are specifically defined in the RRS reference
 * document.  (It does not include any reserved values.)</p>
 *
 * @author Linda Cully 28 Feb 2022
 * @version 1.02 Added -9 of "Unknown" type per S.Loehrer.
 *
 * @author Joel Clawson
 * @version 1.01 This initial version of the table.
 * 
 * @since 1.01
 **/
public class RRSBalloonManufacturerTable extends RRSCodeTable {

    /**
     * Create a new instance of a RRSBalloonManufacturerTable.
     **/
    public RRSBalloonManufacturerTable() { super(); }

    /**
     * Populate the table with the code value pairs from Code Table 9-9 of the
     * NWS RRS reference document.
     **/
    public void populate() {
	put(-9, "Unknown");
	put(0, "Kaysam");
	put(1, "Totex");
	put(2, "KKS");
	put(62, "Other");
	put(63, "Missing");
    }
}
