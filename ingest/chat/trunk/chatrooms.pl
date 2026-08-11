#!/usr/bin/perl
package ChatMap;

###############################################################################
# This file contains two hash tables that map specific chatrooms for specific
# dates to the project they are associated with.
#
# Please update the tables each time a chatroom is created or deleted on
# rdcc.guest. Be sure to update both the project->date mapping and the
# chatroom->project mapping (two separate hashes below).
# When you are done, please commit changes to svn. Thanks!
###############################################################################


# Set dates to archive chatrooms for a project. This is necessary for chatrooms 
# like #gv that persist across projects and should be archived to different 
# projects during different time periods. Be sure to include a real end date
# (not 9999/99/99) because once the project end date passes, the script will
# send email to let me know we need to either turn off those chatrooms or (as
# in the case of the aircraft) assign them to a different project.
our %project_dates=(
    # You can delete these once a project is complete.
    'ATOM3' => ['2017/09/01','2018/11/01'],
    'ARISTO_2017' => ['2017/02/20','2017/03/28'],
    'SPICULE' => ['2021/04/26','2021/06/25'],
    'TRANS2AM' => ['2021/06/24','2021/10/15'],
    'TI3GER' => ['2022/01/20','2022/04/30'],
    'CHACHA' => ['2022/01/26','2022/04/16'],
    'ACCLIP' => ['2022/07/20','2022/09/07'],
    'CAESAR' => ['2024/01/20','2024/07/31'],
    'ACES' => ['2024/03/20','2024/04/10'],
    'CG_WAVES' => ['2025/04/29','2025/06/16'],
    'GOTHAAM' => ['2025/06/18','2025/09/05'],
    'MAIRE25' => ['2025/09/11','2025/10/10'],
    'TI3GER-2' => ['2026/04/20','2026/05/29'],
    'SLC-SOS' => ['2026/06/20','2026/09/01'],
    'INSPYRE' => ['2026/07/10','2026/09/15'],
    # for unspecified NASA projects EOL not archiving
    'NASA' => ['2018/09/01','2027/01/01'],
    'ECLIPSE' => ['2019/06/21','2019/07/15'],
    # CAMP2EX is a NASA project per Greg S. No data archival or other support.
    'CAMP2EX' => ['2019/09/01','2019/10/15'],
    #'METHANE_AIR' => ['2019/10/28','2022/10/20'],
    'MAIRE24' => ['2024/06/17','2024/10/11'],
    'APAR' => ['2024/06/17','2025/06/25'],
    #The C130 will be assigned to the APAR dev project until 
    #summer 2024 when it will be used in GOTHAAM


    ###################################
    # NSF NCAR AIRCRAFT (GV AND C-130)
    ###################################
    # Temporary, until new projects come in.
    #'/operational/aircraft/c-130' => ['2013/01/01','9999/99/99'],
    #'/operational/aircraft/GV' => ['2013/01/01','9999/99/99'],
    # see note above stating not to use the 999 end date
    #'/operational/aircraft/c-130' => ['2013/01/01','2024/12/31'],
    #'/operational/aircraft/GV' => ['2013/01/01','2024/12/31'],
   
    ###################################
    # OTHER AIRCRAFT
    ###################################
    # Don't delete these. They are ongoing rooms and are not project specific.
    'operational/aircraft/n42rf' => ['2013/01/01','9999/99/99'],
    'operational/aircraft/n43rf' => ['2013/01/01','9999/99/99'],
    'operational/aircraft/n49rf' => ['2013/01/01','9999/99/99'],
    'operational/aircraft/g1' => ['2013/01/01','9999/99/99'],
    'operational/aircraft/p3b' => ['2013/01/01','9999/99/99'],
    'operational/aircraft/kingair' => ['2014/02/01','9999/99/99'],
    'operational/aircraft/dc8' => ['2014/08/01','9999/99/99'],
    'operational/aircraft/er2' => ['2013/01/01','9999/99/99'],
    # UND citation
    'operational/aircraft/citation' => ['2014/04/01','9999/99/99'],
    'operational/carcah' => ['2013/01/01','9999/99/99'],

    ###################################
    # DO NOT SAVE
    ###################################
    'doNotSave' => ['2015/08/29','2024/12/31'],
);


# Map chatrooms to the project they are associated with. To see what
# operational dirs exist, look in
# /net/work/Projects/chatlog_recovery/logs/operational. Project dirs are
# created in /net/work/Projects/chatlog_recovery/logs/Projects.
our %chatmap=(
    # You can delete these once a project is complete.
    'avaps' => 'doNotSave',
    #'spicule' => 'SPICULE',
    #'trans2am' => 'TRANS2AM',
    #'chacha' => 'CHACHA',
    #'ti3ger' => 'TI3GER',
    #'acclip' => 'ACCLIP',
    'caesar' => 'CAESAR',
    'cgwaves' => 'CG_WAVES',
    'gothaam' => 'GOTHAAM',
    'kingair' => 'SLC-SOS',
    'inspyre' => 'INSPYRE',
    'mcchat' => 'INSPYRE',
    # avaps should be ignored indefinitely because it is usually used
    # for non-NCAR non-NSF non-project operational troubleshooting
    'gv' => 'INSPYRE',
    'daq-gv' => 'INSPYRE',
    'gv_mx' => 'INSPYRE',
    #'gv-ffp' => 'ACCLIP',
    
    'c-130' => 'GOTHAAM',
    'daq-c130' => 'GOTHAAM',
    'c130q' => 'GOTHAAM',
    
    'atom' => 'ATOM3',
    'ao2med' => 'ATOM3',
    'hsrl' => 'NASA',
    #'hcr' => 'SPICULE',
    # 'otrec' => 'OTREC',
    # 'hsrl' => 'OTREC',
    #'dropsonde' => 'CAESAR',
    # 'dropsonde_otrec' => 'OTREC',
    #camp2ex' => 'CAMP2EX',
    #'camp2ex_fltsci' => 'CAMP2EX',
    #'mcchat' => 'CAESAR',
    #'cvi' => 'CAESAR',

    ###################################
    # NSF NCAR AIRCRAFT (GV AND C-130)
    ###################################
    #Temp, until new projects come in. 
    #'c-130' => 'operational/aircraft/c-130',
    #'c-130q' => 'operational/aircraft/c-130',
    #'daq-c130' => 'operational/aircraft/c-130',
    
    #'gv' => 'operational/aircraft/GV',
    #'gv-ffp' => 'operational/aircraft/GV',
    #'daq-gv' => 'operational/aircraft/GV',

    ###################################
    # OTHER AIRCRAFT
    ###################################
    # Don't delete these. They are ongoing rooms and are not project specific.
    'citation' => 'operational/aircraft/citation',
    'kingair' => 'operational/aircraft/kingair',
    'dc8' => 'operational/aircraft/dc8',
    'n42rf' => 'operational/aircraft/n42rf',
    'n43rf' => 'operational/aircraft/n43rf',
    'n49rf' => 'operational/aircraft/n49rf',
    'g1' => 'operational/aircraft/g1',
    'p3b' => 'operational/aircraft/p3b',
    'er2' => 'operational/aircraft/er2',
    'carcah' => 'operational/carcah',
    
    ###################################
    # DO NOT SAVE
    ###################################
    'network' => 'doNotSave',
);

1;
