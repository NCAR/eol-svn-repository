package dmg.ua.sounding.convert.nws;

/**
 * <p>The RRSRHSensorTypeTable is the RRSCodeTable that holds the information
 * for the RRS Relative Humidity Sensor Type from <code>Code Table 9-4</code> from
 * <a href="http://dmg.eol.ucar.edu/software/tools/upper_air/NWS_RRS_bufr_extractor/NWS_RRS_reference.doc">
 * NWS_RRS_reference.doc</a>.</p>
 * <p>The table contains a mapping of the integral code values to their String textual
 * values.  It only defines values that are specifically defined in the RRS reference
 * document.  (It does not include any reserved values.)</p>
 *
 * @author Linda Echo-Hawk 24 April 2013
 * @version 1.02 Added values to the table (provided by Scot Loehrer)
 *
 * @author Joel Clawson
 * @version 1.01 This initial version of the table.
 *
 * @since 1.01
 **/
public class RRSRHSensorTypeTable extends RRSCodeTable {

    /**
     * Create a new instance of a RRSRHSensorTypeTable.
     **/
    public RRSRHSensorTypeTable() { super(); }

    /**
     * Populate the table with the code/value pairs from Code Table 9-4 of
     * the NWS RRS reference document.
     **/
    public void populate() {
        put(0, "VIZ Mark II Carbon Hygristor");
        put(1, "VIZ B2 Hygristor");
        put(2, "Vaisala A-Humicap");
        put(3, "Vaisala H-Humicap");
        put(4, "Capacitance sensor");
        put(5, "Vaisala RS90");
        put(6, "Sippican Mark IIA Carbon Hygristor");
        put(7, "Twin alternatively heated Humicap capacitance sensor");
        put(8, "Humicap capacitance sensor with active de-icing method");
        put(30, "Other");
        put(31, "Missing");
    }
}
