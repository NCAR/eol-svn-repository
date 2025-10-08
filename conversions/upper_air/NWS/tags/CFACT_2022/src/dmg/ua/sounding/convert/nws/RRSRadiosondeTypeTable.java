package dmg.ua.sounding.convert.nws;

/**
 * <p>The RRSRadiosondeTypeTable is the RRSCodeTable that holds the information
 * for the RRS Radiosonde Type from <code>Code Table 9-1</code> from
 * <a href="http://dmg.eol.ucar.edu/software/tools/upper_air/NWS_RRS_bufr_extractor/NWS_RRS_reference.doc">
 * NWS_RRS_reference.doc</a>.</p>
 * <p>The table contains a mapping of the integral code values to their String textual
 * values.  It only defines values that are specifically defined in the RRS reference
 * document.  (It does not include any reserved values.)</p>
 *
 * @author Linda Cully 28 Feb 2022
 * @version 1.03 Added 141 and 154 sonde types to the table (provided by Scot Loehrer)
 *
 * @author Linda Echo-Hawk 24 April 2013
 * @version 1.02 Added values to the table (provided by Scot Loehrer)
 *
 * @author Joel Clawson
 * @version 1.01 This initial version of the table.
 *
 * @since 1.01
 **/
public class RRSRadiosondeTypeTable extends RRSCodeTable {

    /**
     * Create a new instance of a RRSRadiosondeTypeTable.
     **/
    public RRSRadiosondeTypeTable() { super(); }

    /**
     * Populate the table with the code/value pairs from Code Table 9-1 of the
     * NWS RRS referece document.
     **/
    public void populate() {
        put(51, "VIZ-B2 (USA)");
        put(52, "Vaisala RS80-57H");
        put(87, "Sippican Mark IIA with chip thermistor, pressure");
        put(141, "Vaisala RS41 with pressure derived from GPS height/DigiCORA MW41");
        put(152, "Vaisala RS92-NGP/Intermet IMS-2000");
        put(154, "Graw DFM-17");
        put(182, "Lockheed Martin Sippican LMS-6 GPS Radiosonde");
        put(255, "Missing");
    }
}
