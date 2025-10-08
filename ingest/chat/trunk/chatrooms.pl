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
    # for unspecified NASA projects EOL not archiving
    'NASA' => ['2018/09/01','2021/10/31'],
    'ECLIPSE' => ['2019/06/21','2019/07/15'],
    # CAMP2EX is a NASA project per Greg S. No data archival or other support.
    'CAMP2EX' => ['2019/09/01','2019/10/15'],
    'METHANE_AIR' => ['2019/10/28','2021/03/31'],



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
    'doNotSave' => ['2015/08/29','2020/12/31'],
);


# Map chatrooms to the project they are associated with. To see what
# operational dirs exist, look in
# /net/work/Projects/chatlog_recovery/logs/operational. Project dirs are
# created in /net/work/Projects/chatlog_recovery/logs/Projects.
our %chatmap=(
    # You can delete these once a project is complete.
    'avaps' => 'doNotSave',
    'spicule' => 'SPICULE',
    # avaps should be ignored indefinitely because it is usually used
    # for non-NCAR non-NSF non-project operational troubleshooting
    'gv' => 'SPICULE',
    'atom' => 'ATOM3',
    'ao2med' => 'ATOM3',
    'hsrl' => 'NASA',
    'hcr' => 'SPICULE',
    # 'otrec' => 'OTREC',
    'daq-gv' => 'SPICULE',
    # 'hsrl' => 'OTREC',
    # 'dropsonde' => 'OTREC',
    # 'dropsonde_otrec' => 'OTREC',
    'camp2ex' => 'CAMP2EX',
    'camp2ex_fltsci' => 'CAMP2EX',


    #
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
    'network' => 'doNotSave',
);

1;
