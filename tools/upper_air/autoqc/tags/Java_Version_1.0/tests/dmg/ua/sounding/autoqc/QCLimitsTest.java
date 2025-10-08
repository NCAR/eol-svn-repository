package dmg.ua.sounding.autoqc;

import static org.junit.Assert.*;

import java.util.*;
import org.junit.*;

public class QCLimitsTest {

	private QCLimits limits;
	
	@Before public void setup() throws QCException {
		limits = new QCLimits(ResourceBundle.getBundle("qclimit_valid"));
	}
	
	@Test (expected = QCException.class)
	public void averagingValueBadValue() throws QCException {
		limits = new QCLimits(ResourceBundle.getBundle("qclimit_bad_average"));
	}
	
	@Test public void averagingValueGoodValue() {
		assertEquals("Averaging Value", 4, limits.getAveragingValue());
	}
	
	@Test public void averagingValueNoKey() throws QCException {
		limits = new QCLimits(ResourceBundle.getBundle("qclimit_no_average"));
		assertEquals("Default Averaging Value", 1, limits.getAveragingValue());
	}
	
	
	@Test (expected = QCException.class)
	public void adiabaticLapseRateBadValue() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_bad_adiabatic_lapse_rate"));
	}
	
	@Test public void adiabaticLapseRateGoodValue() throws QCException {
		assertEquals("Adiabatic Lapse Rate", .01,
				limits.getAdiabaticLapseRate());
	}
	
	@Test (expected = QCException.class)
	public void adiabaticLapseRateNoKey() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_no_adiabatic_lapse_rate"));
	}
	
	
	@Test (expected = QCException.class)
	public void badAscentRateChangeBadValue() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_bad_bad_ascrate_change"));
	}
	
	@Test public void badAscentRateChangeGoodValue() throws QCException {
		assertEquals("Bad Ascent Rate Change", 5.0,
				limits.getBadAscentRateChange());
	}
	
	@Test (expected = QCException.class)
	public void badAscentRateChangeNoKey() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_no_bad_ascrate_change"));
	}
	
	
	@Test (expected = QCException.class)
	public void badLapseRateBadValue() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_bad_bad_lapse_rate"));
	}
	
	@Test 
	public void badLapseRateGoodValue() throws QCException { 
		assertEquals("Bad Lapse Rate", -30.0,
				limits.getBadLapseRate());
	}
	
	@Test (expected = QCException.class)
	public void badLapseRateNoKey() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_no_bad_lapse_rate"));
	}
	
	
	@Test (expected = QCException.class)
	public void badRapidPressureIncreaseBadValue() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_bad_bad_rapid_press_inc"));
	}
	
	@Test 
	public void badRapidPressureIncreaseGoodValue() throws QCException { 
		assertEquals("Bad Rapid Pressure Increase", 2.0,
				limits.getBadRapidPressureIncrease());
	}
	
	@Test (expected = QCException.class)
	public void badRapidPressureIncreaseNoKey() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_no_bad_rapid_press_inc"));
	}	
	
	
	@Test (expected = QCException.class)
	public void badRapidTemperatureIncreaseBadValue() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_bad_bad_rapid_temp_inc"));
	}
	
	@Test public void badRapidTemperatureIncreaseGoodValue() throws QCException{
		assertEquals("Bad Rapid Temperature Increase", 100.0,
				limits.getBadRapidTemperatureIncrease());
	}
	
	@Test (expected = QCException.class)
	public void badRapidTemperatureIncreaseNoKey() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_no_bad_rapid_temp_inc"));
	}
	

	@Test (expected = QCException.class)
	public void badStratRapidTemperatureIncreaseBadValue() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_bad_bad_strat_rapid_temp_inc"));
	}
	
	@Test public void badStratRapidTemperatureIncreaseGoodValue() 
	throws QCException{
		assertEquals("Bad Strat Rapid Temperature Increase", 200.0,
				limits.getBadStratRapidTemperatureIncrease());
	}
	
	@Test (expected = QCException.class)
	public void badStratRapidTemperatureIncreaseNoKey() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_no_bad_strat_rapid_temp_inc"));
	}
	

	@Test (expected = QCException.class)
	public void badSurfaceRapidTemperatureIncreaseBadValue() throws QCException{
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_bad_bad_sfc_rapid_temp_inc"));
	}
	
	@Test public void badSurfaceRapidTemperatureIncreaseGoodValue() 
	throws QCException{
		assertEquals("Bad Surface Rapid Temperature Increase", 200.0,
				limits.getBadSurfaceRapidTemperatureIncrease());
	}
	
	@Test (expected = QCException.class)
	public void badSurfaceRapidTemperatureIncreaseNoKey() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_no_bad_sfc_rapid_temp_inc"));
	}
	

	@Test (expected = QCException.class)
	public void badSurfaceLapseRateBadValue() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_bad_bad_sfc_lapse_rate"));
	}
	
	@Test 
	public void badSurfaceLapseRateGoodValue() throws QCException { 
		assertEquals("Bad Surface Lapse Rate", -200.0,
				limits.getBadSurfaceLapseRate());
	}
	
	@Test (expected = QCException.class)
	public void badSurfaceLapseRateNoKey() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_no_bad_sfc_lapse_rate"));
	}	
	
	
	@Test (expected = QCException.class)
	public void decreasePressureCheckBadValue() throws QCException {
		limits = new QCLimits(
				ResourceBundle.getBundle("qclimit_bad_dec_press"));
	}
	
	@Test public void decreasePressureCheckGoodValue() throws QCException {
		assertEquals("Decreasing Pressure Check", 100.0,
				limits.getDecreasingPressureCheck());
	}
	
	@Test (expected = QCException.class)
	public void decreasePressureCheckNoKey() throws QCException {
		limits = new QCLimits(
				ResourceBundle.getBundle("qclimit_no_dec_press"));
	}
	
	
	
	@Test (expected = QCException.class)
	public void lapseRateDifferenceBadValue() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_bad_lapse_rate_diff"));
	}
	
	@Test public void lapseRateDifferenceGoodValue() throws QCException {
		assertEquals("Lapse Rate Difference", .25,
				limits.getLapseRateDifference());
	}
	
	@Test (expected = QCException.class)
	public void lapseRateDifferenceNoKey() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_no_lapse_rate_diff"));
	}

	
	@Test (expected = QCException.class)
	public void lapseRateLimitBadValue() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_bad_lapse_rate_limit"));
	}
	
	@Test public void lapseRateLimitGoodValue() throws QCException {
		assertEquals("Lapse Rate Limit", 1.5,
				limits.getLapseRateLimit());
	}
	
	@Test (expected = QCException.class)
	public void lapseRateLimitNoKey() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_no_lapse_rate_limit"));
	}

	
	@Test (expected = QCException.class)
	public void latitudeTypeBadValue() throws QCException {
		limits = new QCLimits(ResourceBundle.getBundle("qclimit_bad_lat"));
	}
	
	@Test public void latitudeTypeCold() throws QCException {
		assertFalse("Cold Latitude Type", limits.isHotLatitude());
	}
	
	@Test public void latitudeTypeHot() throws QCException {
		limits = new QCLimits(ResourceBundle.getBundle("qclimit_hot_lat"));
		assertTrue("Hot Latitude Type", limits.isHotLatitude());
	}

	@Test (expected = QCException.class)
	public void latitudeTypeNoKey() throws QCException {
		limits = new QCLimits(ResourceBundle.getBundle("qclimit_no_lat"));
	}	
	
	
	@Test (expected = QCException.class)
	public void maximumBadWindSpeedBadValue() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_bad_max_bad_windspd"));
	}
	
	@Test public void maximumBadWindSpeedGoodValue() {
		assertEquals("Max Bad Wind Speed", 150.0,
				limits.getMaximumBadWindSpeed());
	}
	
	@Test (expected = QCException.class)
	public void maximumBadWindSpeedNoKey() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_no_max_bad_windspd"));
	}
	
	
	@Test (expected = QCException.class)
	public void maximumMissingDewPointBadValue() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_bad_max_miss_dewpt"));
	}
	
	@Test public void maximumMissingDewPointGoodValue() {
		assertEquals("Max Missing Dew Point", 75.0,
				limits.getMaximumMissingDewPoint());
	}
	
	@Test (expected = QCException.class)
	public void maximumMissingDewPointNoKey() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_no_max_miss_dewpt"));
	}
	
	
	@Test (expected = QCException.class)
	public void maximumMissingPressureBadValue() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_bad_max_miss_press"));
	}
	
	@Test public void maximumMissingPressureGoodValue() {
		assertEquals("Max Missing Pressure", 1100.0,
				limits.getMaximumMissingPressure());
	}
	
	@Test (expected = QCException.class)
	public void maximumMissingPressureNoKey() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_no_max_miss_press"));
	}
	
	
	@Test (expected = QCException.class)
	public void maximumMissingTemperatureBadValue() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_bad_max_miss_temp"));
	}
	
	@Test public void maximumMissingTemperatureGoodValue() {
		assertEquals("Max Missing Temperature", 75.0,
				limits.getMaximumMissingTemperature());
	}
	
	@Test (expected = QCException.class)
	public void maximumMissingTemperatureNoKey() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_no_max_miss_temp"));
	}
	
	
	@Test (expected = QCException.class)
	public void maximumMissingWindSpeedBadValue() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_bad_max_miss_windspd"));
	}
	
	@Test public void maximumMissingWindSpeedGoodValue() {
		assertEquals("Max Missing Wind Speed", 200.0,
				limits.getMaximumMissingWindSpeed());
	}
	
	@Test (expected = QCException.class)
	public void maximumMissingWindSpeedNoKey() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_no_max_miss_windspd"));
	}
	
	
	@Test (expected = QCException.class)
	public void maximumQuestionableAltitudeBadValue() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_bad_max_quest_alt"));
	}
	
	@Test public void maximumQuestionableAltitudeGoodValue() {
		assertEquals("Max Questionable Altitude", 40000.0,
				limits.getMaximumQuestionableAltitude());
	}
	
	@Test (expected = QCException.class)
	public void maximumQuestionableAltitudeNoKey() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_no_max_quest_alt"));
	}
	
	
	@Test (expected = QCException.class)
	public void maximumQuestionableAscentRateBadValue() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_bad_max_quest_ascrate"));
	}
	
	@Test public void maximumQuestionableAscentRateGoodValue() {
		assertEquals("Max Questionable Ascent Rate", 10.0,
				limits.getMaximumQuestionableAscentRate());
	}
	
	@Test (expected = QCException.class)
	public void maximumQuestionableAscentRateNoKey() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_no_max_quest_ascrate"));
	}
	
	
	@Test (expected = QCException.class)
	public void maximumQuestionableDewPointBadValue() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_bad_max_quest_dewpt"));
	}
	
	@Test public void maximumQuestionableDewPointGoodValue() {
		assertEquals("Max Questionable Dew Point", 33.0,
				limits.getMaximumQuestionableDewPoint());
	}
	
	@Test (expected = QCException.class)
	public void maximumQuestionableDewPointNoKey() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_no_max_quest_dewpt"));
	}
	
	
	@Test (expected = QCException.class)
	public void maximumQuestionablePressureBadValue() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_bad_max_quest_press"));
	}
	
	@Test public void maximumQuestionablePressureGoodValue() {
		assertEquals("Max Questionable Pressure", 1050.0,
				limits.getMaximumQuestionablePressure());
	}
	
	@Test (expected = QCException.class)
	public void maximumQuestionablePressureNoKey() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_no_max_quest_press"));
	}
	
	
	@Test (expected = QCException.class)
	public void maximumQuestionableTemperatureBadValue() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_bad_max_quest_temp"));
	}
	
	@Test public void maximumQuestionableTemperatureGoodValue() {
		assertEquals("Max Questionable Temperature", 45.0,
				limits.getMaximumQuestionableTemperature());
	}
	
	@Test (expected = QCException.class)
	public void maximumQuestionableTemperatureNoKey() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_no_max_quest_temp"));
	}
	
	
	@Test (expected = QCException.class)
	public void maximumQuestionableWindSpeedBadValue() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_bad_max_quest_windspd"));
	}
	
	@Test public void maximumQuestionableWindSpeedGoodValue() {
		assertEquals("Max Questionable Wind Speed", 100.0,
				limits.getMaximumQuestionableWindSpeed());
	}
	
	@Test (expected = QCException.class)
	public void maximumQuestionableWindSpeedNoKey() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_no_max_quest_windspd"));
	}
	
	
	@Test (expected = QCException.class)
	public void minimumQuestionableAltitudeBadValue() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_bad_min_quest_alt"));
	}
	
	@Test public void minimumQuestionableAltitudeGoodValue() {
		assertEquals("Min Questionable Altitude", 0.0,
				limits.getMinimumQuestionableAltitude());
	}
	
	@Test (expected = QCException.class)
	public void minimumQuestionableAltitudeNoKey() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_no_min_quest_alt"));
	}
	
	
	@Test (expected = QCException.class)
	public void minimumQuestionableDewPointBadValue() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_bad_min_quest_dewpt"));
	}
	
	@Test public void minimumQuestionableDewPointGoodValue() {
		assertEquals("Min Questionable Dew Point", -99.9,
				limits.getMinimumQuestionableDewPoint());
	}
	
	@Test (expected = QCException.class)
	public void minimumQuestionableDewPointNoKey() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_no_min_quest_dewpt"));
	}
	
	
	@Test (expected = QCException.class)
	public void minimumQuestionablePressureBadValue() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_bad_min_quest_press"));
	}
	
	@Test public void minimumQuestionablePressureGoodValue() {
		assertEquals("Min Questionable Pressure", 0.0,
				limits.getMinimumQuestionablePressure());
	}
	
	@Test (expected = QCException.class)
	public void minimumQuestionablePressureNoKey() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_no_min_quest_press"));
	}
	
	
	@Test (expected = QCException.class)
	public void minimumQuestionableTemperatureBadValue() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_bad_min_quest_temp"));
	}
	
	@Test public void minimumQuestionableTemperatureGoodValue() {
		assertEquals("Min Questionable Temperature", -90.0,
				limits.getMinimumQuestionableTemperature());
	}
	
	@Test (expected = QCException.class)
	public void minimumQuestionableTemperatureNoKey() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_no_min_quest_temp"));
	}
	
	
	@Test (expected = QCException.class)
	public void minimumQuestionableWindSpeedBadValue() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_bad_min_quest_windspd"));
	}
	
	@Test public void minimumQuestionableWindSpeedGoodValue() {
		assertEquals("Min Questionable Wind Speed", 0.0,
				limits.getMinimumQuestionableWindSpeed());
	}
	
	@Test (expected = QCException.class)
	public void minimumQuestionableWindSpeedNoKey() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_no_min_quest_windspd"));
	}
	
	
	@Test (expected = QCException.class)
	public void pressureLimitBadValue() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_bad_press_limit"));
	}
	
	@Test public void pressureLimitGoodValue() throws QCException {
		assertEquals("Pressure Limit", 250.0,
				limits.getPressureLimit());
	}
	
	@Test (expected = QCException.class)
	public void pressureLimitNoKey() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_no_press_limit"));
	}
	
	@Test (expected = QCException.class)
	public void questionableAscentRateChangeBadValue() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_bad_quest_ascrate_change"));
	}
	
	@Test 
	public void questionableAscentRateChangeGoodValue() throws QCException { 
		assertEquals("Questionable Ascent Rate Change", 3.0,
				limits.getQuestionableAscentRateChange());
	}
	
	@Test (expected = QCException.class)
	public void questionableAscentRateChangeNoKey() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_no_quest_ascrate_change"));
	}
	
	
	@Test (expected = QCException.class)
	public void questionableLapseRateBadValue() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_bad_quest_lapse_rate"));
	}
	
	@Test 
	public void questionableLapseRateGoodValue() throws QCException { 
		assertEquals("Questionable Lapse Rate", -15.0,
				limits.getQuestionableLapseRate());
	}
	
	@Test (expected = QCException.class)
	public void questionableLapseRateNoKey() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_no_quest_lapse_rate"));
	}
	
	
	@Test (expected = QCException.class)
	public void questionableRapidPressureIncreaseBadValue() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_bad_quest_rapid_press_inc"));
	}
	
	@Test 
	public void questionableRapidPressureIncreaseGoodValue() throws QCException{ 
		assertEquals("Questionable Rapid Pressure Increase", 1.0,
				limits.getQuestionableRapidPressureIncrease());
	}
	
	@Test (expected = QCException.class)
	public void questionableRapidPressureIncreaseNoKey() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_no_quest_rapid_press_inc"));
	}	
	
	
	@Test (expected = QCException.class)
	public void questionableSurfaceLapseRateBadValue() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_bad_quest_sfc_lapse_rate"));
	}
	
	@Test 
	public void questionableSurfaceLapseRateGoodValue() throws QCException { 
		assertEquals("Questionable Surface Lapse Rate", -100.0,
				limits.getQuestionableSurfaceLapseRate());
	}
	
	@Test (expected = QCException.class)
	public void questionableSurfaceLapseRateNoKey() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_no_quest_sfc_lapse_rate"));
	}
	
	
	@Test (expected = QCException.class)
	public void rapidTemperatureIncreaseBadValue() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_bad_rapid_temp_inc"));
	}
	
	@Test public void rapidTemperatureIncreaseGoodValue() throws QCException {
		assertEquals("Rapid Temperature Increase", 5.0,
				limits.getRapidTemperatureIncrease());
	}
	
	@Test (expected = QCException.class)
	public void rapidTemperatureIncreaseNoKey() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_no_rapid_temp_inc"));
	}
	

	@Test (expected = QCException.class)
	public void stratRapidTemperatureIncreaseBadValue() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_bad_strat_rapid_temp_inc"));
	}
	
	@Test public void stratRapidTemperatureIncreaseGoodValue() 
	throws QCException {
		assertEquals("Rapid Temperature Increase", 10.0,
				limits.getStratRapidTemperatureIncrease());
	}
	
	@Test (expected = QCException.class)
	public void stratRapidTemperatureIncreaseNoKey() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_no_strat_rapid_temp_inc"));
	}
	

	@Test (expected = QCException.class)
	public void surfacePressureLimitBadValue() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_bad_sfc_press_limit"));
	}
	
	@Test public void surfacePressureLimitGoodValue() throws QCException {
		assertEquals("Surface Pressure Limit", 0.0,
				limits.getSurfacePressureLimit());
	}
	
	@Test (expected = QCException.class)
	public void surfacePressureLimitNoKey() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_no_sfc_press_limit"));
	}


	@Test (expected = QCException.class)
	public void surfaceRapidTemperatureIncreaseBadValue() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_bad_bad_sfc_rapid_temp_inc"));
	}
	
	@Test public void surfaceRapidTemperatureIncreaseGoodValue() 
	throws QCException {
		assertEquals("Surface Rapid Temperature Increase", 0.0,
				limits.getSurfaceRapidTemperatureIncrease());
	}
	
	@Test (expected = QCException.class)
	public void surfaceRapidTemperatureIncreaseNoKey() throws QCException {
		limits = new QCLimits(ResourceBundle.
				getBundle("qclimit_no_sfc_rapid_temp_inc"));
	}
}
