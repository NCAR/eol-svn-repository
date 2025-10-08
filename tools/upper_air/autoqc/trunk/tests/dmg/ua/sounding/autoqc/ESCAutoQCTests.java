package dmg.ua.sounding.autoqc;

import org.junit.runner.*;
import org.junit.runners.*;

@RunWith (Suite.class)
@Suite.SuiteClasses({
	QCLimitsTest.class,
	UpsondeESCAutoQCTest.class,
	DropsondeESCAutoQCTest.class,
	ISSESCAutoQCTest.class,
	NWSESCAutoQCTest.class
})

public class ESCAutoQCTests {

	public static void main(String[] args) {
		org.junit.runner.JUnitCore.
				main("dmg.ua.sounding.autoqc.ESCAutoQCTests");
	}
}
