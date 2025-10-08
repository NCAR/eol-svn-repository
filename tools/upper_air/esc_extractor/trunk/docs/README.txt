
This directory contains the documentation for the ESC Extractor program.  The sample-socrates-build.xml file is from the SOCRATES project and
is included in this directory as an example for how to run the 50 m extractor with ant.  The file needs to be renamed to build.xml if it 
will be used for running the ant command.  There is a 5mb-extract command and a 50m-extract command that can be run
with ant.  Also, included in this file is the minimum snippet for how to run the 5mb-extract and the 50m-extract programs, so this 
snippet can also be used in the project build files.  This information is included here, since the project build files tend to be 
copied from the last project and are not taken from svn.  These are the only two places that the 50m-extract command information was included.

    <!-- Constants used in the build file. -->
    <property name="5mb" value="5mb" />
    <property name="50m" value="50m" />
    <property name="final" value="final" />

    <!-- Extract the 5mb pressure level data from the final QC'ed high res soundings. -->
    <target name="5mb-extract">
        <!-- Execute the pressure extraction. -->
        <java classname="dmg.ua.sounding.extract.ESC5mbExtractor" fork="yes">
            <arg value="-Z" />
            <arg value="${final}" />
            <arg value="${5mb}" />
            <arg value="${5mb}/extract.log" />
    <!-- This build file is set up to run .cls files.  If you want to run .cls.qc files,
         then you must change the arg below. -->
            <arg value="\.cls(\.gz)?$" />
            <classpath refid="classpath" />
        </java>
        <!-- Check the file format of the 5mb data files. -->
        <java classname="dmg.ua.sounding.check.ESCCheckFile" fork="yes">
            <arg value="${5mb}" />
            <arg value="${5mb}/check_format_5mb.log" />
            <arg value="\.05mb(\.gz)?$" />
            <classpath refid="classpath" />
        </java>
    </target>

   <!-- Extract the 50m height level data from the final QC'ed high res soundings. -->
    <target name="50m-extract">
        <!-- Execute the pressure extraction. -->
        <java classname="dmg.ua.sounding.extract.ESC50mExtractor" fork="yes">
            <arg value="-Z" />
            <arg value="${final}" />
            <arg value="${50m}" />
            <arg value="${50m}/extract.log" />
    <!-- This build file is set up to run .cls files.  If you want to run .cls.qc files,
         then you must change the arg below. -->
            <arg value="\.cls(\.gz)?$" />
            <classpath refid="classpath" />
        </java>
        <!-- Check the file format of the 50m data files. -->
        <java classname="dmg.ua.sounding.check.ESCCheckFile" fork="yes">
            <arg value="${50m}" />
            <arg value="${50m}/check_format_50m.log" />
            <arg value="\.50m(\.gz)?$" />
            <classpath refid="classpath" />
        </java>
    </target>

