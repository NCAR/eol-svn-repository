#  Author: Eric Dattore
#  Date: 7/20/2013
#
#  Rev 11/11/2015
#  Fix whitespace
#  - Eric Dattore
#
#-----------------------------------------------------------------------------#
#                                Subroutines                                  #
#-----------------------------------------------------------------------------#
# new()
#   * Creates a new instance of the Order_Statistics module.
#
# shortDesc()
#   * Returns simple text title/description of the tool
#
# longDesc()
#   * Returns a long text description of the function of this tool
#
# linkTitle()
#   * Returns the name of the module for links
#
# queryForm()
#   * Displays the HTML form that provides input for the module
#
# queryResult()
#   * Displays the results based on the inputs
#
# drawMetrics()
#   * Draws the visualization metrics based on inputs from the user
#
# getProjects()
#   * Gets all projects in the database and puts them into a select HTML item
#
# (my $title, my $form) = queryForm()
#   * Returns the tool title and the text for an HTML form
#
# group
#   * the tool group this tool belongs to (dataset, project, general)
#-----------------------------------------------------------------------------#
#!/usr/bin/perl -w
package Order_Statistics;

use Date::Calc qw(:all);
use Data::Dumper;
use lib ".";
use EQuery;
use Utils;
use URLparams;

# ------------------------------------------------------------------------
# Called when instantiating a new Order Statistics module
# ------------------------------------------------------------------------
sub new {
    my $class = shift;
    my $self = {};
    bless($self, $class);

    $self->{identifier} = "order_statistics";
    $self->{group} = "project";

    return $self;
}

# ------------------------------------------------------------------------
# Returns a short description of the module (the name of the module)
# ------------------------------------------------------------------------
sub shortDesc {
    return "Order Metrics";
}

# ------------------------------------------------------------------------
# Returns the long description, detailing its output
# ------------------------------------------------------------------------
sub longDesc {
    my $description = <<DESC;
    <table>
        <tr>
            <td>
                Displays the total order statistics. Based on the project selected. Output includes:
                <ul>
                    <li>Number of orders</li>
                    <li>Number of unique orders</li>
                    <li>Number of users</li>
                    <li>Size of orders</li>
                </ul>
            </td>
        </tr>
    </table>
DESC

    return $description;
}

# ------------------------------------------------------------------------
# Returns the title for links to this module
# ------------------------------------------------------------------------
sub linkTitle {
    return "Order Metrics";
}

# ------------------------------------------------------------------------
# Subroutine draws the form that the user interacts with
# ------------------------------------------------------------------------
sub queryForm {
    my $self = shift;

    my $script = '<script>
       $(\'#startdate_show\').on(\'click\', function(e) {
       $(\'#start_date\').AnyTime_noPicker()
       .AnyTime_picker({ format: "%Y-%m-%d", earliest: new Date(1994, 9, 31), latest: new Date() }).focus();
       e.preventDefault();
       });
       $(\'#enddate_show\').on(\'click\', function(e) {
       $(\'#end_date\').AnyTime_noPicker()
       .AnyTime_picker({ format: "%Y-%m-%d", earliest: new Date(1994, 9, 31), latest: new Date() }).focus();
       e.preventDefault();
       });
       $("#clear_startdate").click( function(e) {
       $("#start_date").val("").change();
       e.preventDefault();
       });
       $("#clear_enddate").click( function(e) {
       $("#end_date").val("").change();
       e.preventDefault();
       });
       </script>';

    # HEREDOC containing all the markup for the form
    # The "@{[ getProjects() ]}" contains a call to the getProjects subroutine to populate the select element
    my $form = <<FORM;
    <form action="supervisor" method="POST" onsubmit="return checkMetricForm()" style="margin:auto;padding:auto;">
        <h4 style="text-align:center; margin:5px;">Project Metric Filtering</h4>
        <fieldset>
            <legend>Projects</legend>
            <p>Select one or more projects from the listing to view metrics on those projects. You can only select up to five projects</p>
            <br>
            <select multiple name="project" size="20">
                @{[ getProjects() ]}
                </select>
        </fieldset>
        <fieldset>
            <legend>Metrics Options</legend>
            <p>Check at least one of the following options to view the desired metrics for the projects selected.</p>
            <label for="orders">Orders (Total and Unique): </label>
            <input type="checkbox" id="orders" name="orders">
            <br>
            <label for="unique_users">Unique Users: </label>
            <input type="checkbox" id="unique_users" name="unique_users">
            <br>
            <label for="total_size">Total Order Size: </label>
            <input type="checkbox" id="total_size" name="total_size">
        </fieldset>
        <fieldset>
            <legend>Visualization Options</legend>
            <p>Any of the options in this category are optional. Selecting an option will display a graphical interpretation of the metrics you selected above.</p>
            <label for="orders_graph">Orders over time: </label>
            <input type="checkbox" id="orders_graph" name="orders_graph">
            <br>
            <label for="size_graph">Volume of data ordered over time</label>
            <input type="checkbox" id="size_graph" name="size_graph">
            <br>
            <label for="files_graph">Number of files ordered over time</label>
            <input type="checkbox" id="files_graph" name="files_graph">
            <br>
            <br>
            <label for="time_offset">Time selection (optional, if you don't select an option, the selection will be automatically determined):</label>
            <br>
            <input type="radio" name="time_offset" value="monthly">Monthly
            <br>
            <input type="radio" name="time_offset" value="yearly">Yearly
        </fieldset>
        <fieldset>
            <legend>Time Subsetting</legend>
            <p>Please click the calendar icon to open up the date picker. All dates are in YYYY-MM-DD format.
                    Please click clear or leave the fields blank if you don't want to use time subsetting
            </p>
            <label for="start_date">Start Date: </label>
            <input type="text" id="start_date" name="start_date" />
            <button id="startdate_show"><img src=\"https://dmg.eol.ucar.edu/software/tools/web/qcodiac/calendar.png\"></button>
            <button id="clear_startdate">Clear</button>
            <br>
            <label for="end_date">End Date: </label>
            <input type="text" id="end_date" name="end_date" />
            <button id="enddate_show"><img src=\"https://dmg.eol.ucar.edu/software/tools/web/qcodiac/calendar.png\"></button>
            <button id="clear_enddate">Clear</button>
        </fieldset>
        <br />
        <input type="hidden" name="results" value="@{[ $self->{identifier} ]}">
        <button name="query" type="submit" style="">Submit</button>
    </form>
    @{[ $script ]}
FORM

    my $title = "Order Metrics: Filter";
    return ($title, $form);
}

# ------------------------------------------------------------------------
# Subroutine to output the results of the user's query from queryForm
# ------------------------------------------------------------------------
sub queryResult {
    my $self = shift;

    # fetch URL params
    my $params = shift;
    my @projects = ($params->exists("project")) ? $params->get("project") : undef;
    my $uniqueUsers = ($params->get("unique_users") eq "on") ? 1 : 0;
    my $totalSize = ($params->get("total_size") eq "on") ? 1 : 0;
    my $orders = ($params->get("orders") eq "on") ? 1 : 0;
    my $ordersGraph = ($params->get("orders_graph") eq "on") ? 1 : 0;
    my $sizeGraph = ($params->get("size_graph") eq "on") ? 1 : 0;
    my $filesGraph = ($params->get("files_graph") eq "on") ? 1 : 0;
    my $timeOffset = ($params->exists("time_offset")) ? $params->get("time_offset") : undef;
    my $startDate = ($params->exists("start_date")) ? $params->get("start_date") : undef;
    my $endDate = $params->exists("end_date") ? $params->get("end_date") : undef;

    # Construct a new EQuery object
    my $db = EQuery->new();

    # Check for child projects in current project list
    my @arrayToAdd;
    for my $project (@projects) {
        my $sql = "SELECT name FROM project WHERE parent_project_id = (SELECT id FROM project WHERE name = \"$project\") ORDER BY name";
        my $indexi = 0;

        $db->query($sql);
        while (my %row = $db->getRow()) {
            push(@arrayToAdd, $row{"name"});
        }
        if ($db->getRowCount() > 0) {
            $index++ until $projects[$index] eq $project;
            splice(@projects, $index, 1);
        }
    }

    @projects = (@projects, @arrayToAdd);

    # Closure to generate the breakdown by dataset
    my $breakdown = sub {
        my $ordersVar = shift;
        my $totalSizeVar = shift;
        my %details = @_;

        my $breakdownStr = "";

        if($ordersVar == 1 || $totalSizeVar == 1) {
            $breakdownStr .= "<tr><td colspan=\"100%\" style=\"text-align:center;\"><a href=\"#\" class=\"toggle\">Click here to show the breakdown by dataset</a>
            </td></tr></table><table id=\"sortable\" class=\"hidden tablesorter\" border=\"1\" width=\"100%\">
            <thead><tr><th>Archive ID</th>";
            $breakdownStr .= "<th>Total Orders</th><th>Unique Orders</th>" unless($ordersVar == 0);
            $breakdownStr .= "<th>Order Size</th>" unless($totalSizeVar == 0);
            $breakdownStr .= "</tr></thead><tbody>";

            my @tempDetails;
            foreach(sort keys %details) {
                @tempDetails = split(',', $details{$_});
                if($ordersVar == 1) {
                    $breakdownStr .= "<tr><td>" . $_ . "</td><td>" . $tempDetails[0] . "</td>
                    <td>" . $tempDetails[1] . "</td>";
                    if($totalSizeVar == 1) {
                        $breakdownStr .= "<td>" . Utils::humanReadableFileSize($tempDetails[2]) . "</td>";
                    }
                    $breakdownStr .= "</tr>";
                }
                elsif($ordersVar == 0) {
                    if($totalSizeVar == 1) {
                        $breakdownStr .= "<tr><td>$_</td><td>" . Utils::humanReadableFileSize($tempDetails[1]) .
                        "</td></tr>";
                    }
                }
            }
        }
        return ($breakdownStr . "</tbody></table>");
    };

    # ------------------------------------------------------------------------
    # Assign the output generation logic to a closure
    # ------------------------------------------------------------------------
    my $outputGeneration = sub {
        my $resultStr = "";
        my %breakdownHash;
        foreach my $project (@projects) {
            undef %breakdownHash;
            $resultStr .= "<table border=\"1\" width=\"100%\">";
            $sql = "SELECT title FROM project WHERE name=\'$project\' LIMIT 1;";
            $db->query($sql);
            while(my %row = $db->getRow()) {
                $resultStr .= "<tr><th class=\"title\" colspan=\"4\">$project: $row{title} Metrics";
                $resultStr .= " (from " . Utils::humanReadableDate($startDate) . " to " . Utils::humanReadableDate($endDate) . ")" if($startDate && $endDate);
                $resultStr .= "</th></tr>";
            }

            $resultStr .= "<tr>";
            if($uniqueUsers == 1) {
                $sql = "select count(distinct email) from codiac_web_order_log where archive_ident in (select d.archive_ident
                   from dataset d left join dataset_project dp on d.id=dp.dataset_id left join project p on
                   dp.project_id=p.id where p.name = " . $db->quote($project) . ") and email not in (" . Utils::getIgnoredEmailAddresses() . ")";
                $sql =~ s/(.*)and(.*)/$1 and delivery_date between '$startDate 00:00:00' and '$endDate 23:59:59' and $2/ if($startDate && $endDate);
                $db->query($sql);

                while(my %row = $db->getRow()) {
                    $resultStr .= "<td class=\"users\" colspan=\"2\">Unique Users: " . $row{"count(distinct email)"} . "</td>";
                }
            }

            if($orders == 1 || $totalSize == 1) {
                $sql = "select archive_ident from codiac_web_order_log where archive_ident in
                   (select d.archive_ident from dataset d left join dataset_project dp on d.id=dp.dataset_id left join
                   project p on dp.project_id=p.id where p.name = " . $db->quote($project) . ") and email not in (" . Utils::getIgnoredEmailAddresses() . ") group by archive_ident";
                $sql =~ s/(.*?)(from)/$1,count(email),count(distinct email) $2/ if($orders == 1);
                $sql =~ s/(.*?)(from)/$1,sum(size_kb) $2/ if($totalSize == 1);
                $sql =~ s/(.*)and(.*)/$1 and delivery_date between '$startDate 00:00:00' and '$endDate 23:59:59' and $2/ if($startDate && $endDate);
                $db->query($sql);

                my $total = 0;
                my $unique = 0;
                my $totalData = 0;
                while(my %row = $db->getRow()) {
                    if($orders == 1) {
                        $total += $row{"count(email)"};
                        $unique += $row{"count(distinct email)"};
                        $breakdownHash{$row{"archive_ident"}} = $row{"count(email)"} . "," .
                        $row{"count(distinct email)"};
                    }
                    if($totalSize == 1) {
                        $totalData += $row{"sum(size_kb)"};
                        $breakdownHash{$row{"archive_ident"}} .= "," . $row{"sum(size_kb)"};
                    }
                }

                $resultStr .= "<td class=\"downloads\" colspan=\"2\">Total Orders: " . $total . "&nbsp;&nbsp;&nbsp;&nbsp;Unique Orders: " . $unique . "</td>" if($orders == 1);

                $resultStr .= "</tr>";

                $resultStr .= "<tr><td colspan=\"4\" class=\"data\">Total Data Served: " . Utils::humanReadableFileSize($totalData) .
                   "</td></tr>" if($totalSize == 1);
            }

            my $archiveIdents = "";
            foreach(sort keys %breakdownHash) {
                $archiveIdents .= "$_,";
            }
            if ($uniqueUsers != 0 || $totalSize != 0 || $orders != 0) {
                $resultStr .= "<tr><td colspan=\"4\" style=\"text-align:center\"><a href=\"/cgi-bin/qcodiac/supervisor?dataset_id=$archiveIdents&results=file_list&fields=purpose&fields=size_kb&fields=hide\">Click here to view file listings</a></td></tr>";
            }

            $resultStr .= $breakdown->($orders, $totalSize, %breakdownHash) . "<br>";
            my $metricsArray = join('', $ordersGraph, $sizeGraph, $filesGraph);
            $resultStr .= $self->drawMetrics($metricsArray, $project, $startDate, $endDate, $timeOffset);
        }

        return $resultStr;
    };

    my $script = '<script type="text/javascript">
       jQuery(document).ready( function() {
          jQuery.tablesorter.addParser({
             id: \'filesize\',
             is: function(s) {
                return s.match(new RegExp( /[0-9]+(\.[0-9]+)?\ (KB|B|GB|MB|TB)/ ));
              },
              format: function(s) {
                 var suf = s.match(new RegExp( /(KB|B|GB|MB|TB)$/ ))[1];
                 var num = parseFloat(s.match( new RegExp( /^[0-9]+(\.[0-9]+)?/ ))[0]);
                 switch(suf) {
                    case \'B\':
                       return num;
                    case \'KB\':
                       return num * 1024;
                    case \'MB\':
                       return num * 1024 * 1024;
                    case \'GB\':
                       return num * 1024 * 1024 * 1024;
                    case \'TB\':
                       return num * 1024 * 1024 * 1024 * 1024;
                 }
              },
              type: \'numeric\'
          });

          jQuery(".tablesorter").tablesorter({
             headers: {
                3: { sorter: \'filesize\' }
             }
          });
       });

       $(\'.toggle\').on(\'click\', function() {
          $(this).closest(\'table\').next().toggle();
          var $self = $(this);
          if ($self.text() == "Click here to show the breakdown by dataset") {
             $self.text("Click here to hide the breakdown by dataset");
          } else if ($self.text() == "Click here to hide the breakdown by dataset") {
             $self.text("Click here to show the breakdown by dataset");
          }
       });
       </script>';

    my $result = <<RESULT;
    @{[ $outputGeneration->() ]}
    @{[ $script ]}
RESULT

    my $title = "Metrics";
    return ($title, $result);
}

# ------------------------------------------------------------------------
# Sub routine for drawing metrics
# ------------------------------------------------------------------------
sub drawMetrics {

    # --------------------------------------------------------------------
    # Closure to fill in years. Generalized to be used for all chart types
    # --------------------------------------------------------------------
    my $fillInYears = sub {
        my $firstTime = shift;
        my $previousDate = shift;
        my $data = shift;
        my $columnToFetch = shift;
        my $firstYear = shift;

        if ($firstTime == 1) {
            if ($firstYear != $row{$columnToFetch}) {
                for (my $i = $firstYear; $i < $row{$columnToFetch}; $i++) {
                    $data .= "[\"$i\t\",0],";
                    $previousDate = $i;
                }
            }
            $firstTime = 0;
        }
        if ($previousDate != undef && $row{$columnToFetch} - 1 != $previousDate) {
            for ($i = $previousDate + 1; $i < $row{$columnToFetch}; $i++) {
                $data .= "[" . $i . ", 0],";
            }
        }
        return ($firstTime, $previousDate, $data);
    };
    # --------------------------------------------------------------------
    # End closure to fill in years
    # --------------------------------------------------------------------

    # --------------------------------------------------------------------
    # Closure to fill in years from the last record to the end date
    # --------------------------------------------------------------------
    my $fillInEndYears = sub {
        my $currentYear = shift;
        my $endYear = shift;
        my $data = shift;

        for (my $i = $currentYear + 1; $i <= $endYear; $i++) {
            $data .= "[\"$i\",0],";
        }

        return $data;
    };
    # --------------------------------------------------------------------
    # End closure to fill in years from last record to the end date
    # --------------------------------------------------------------------

    # --------------------------------------------------------------------
    # Closure to fill in months. Generalized to be used for all chart types
    # --------------------------------------------------------------------
    my $fillInMonths = sub {
        my $firstTime = shift;
        my @previousDate = (shift, shift);
        my $data = shift;
        my @columnsToFetch = (shift, shift);
        my @firstDate = (shift, shift);
        my @monthList = qw(January February March April May June July August September October November December);

        if ($firstTime == 1) {
            if ($firstDate[1] != $row{$columnsToFetch[1]} || $firstDate[0] <= $row{$coumnToFetch[0]}) {
                my $j = $firstDate[1];
                for (my $i = $firstDate[0]; $i <= $row{$columnsToFetch[0]}; $i++) {
                    for( ; $j <= 12; $j++) {
                        if ($i == $row{$columnsToFetch[0]} && $j == $row{$columnsToFetch[1]}) {
                            last;
                        }
                        $data .= "[\"$monthList[$j - 1] $i\",0],";
                        $previousDate[0] = $i;
                        $previousDate[1] = $j;
                    }
                    $j = 1;
                }
            }
            $firstTime = 0;
        }

        if ($previousDate[0] != undef && $previousDate[1] != undef && $row{$columnsToFetch[0]} >= $previousDate[0] && $row{$columnsToFetch[1]} - 1 != $previousDate[1]) {
            my $j = $previousDate[1] + 1;
            for (my $i = $previousDate[0]; $i <= $row{$columnsToFetch[0]}; $i++) {
                for ( ; $j <= 12; $j++) {
                    if ($j == $row{$columnsToFetch[1]} && $i == $row{$columnsToFetch[0]}) {
                        last;
                    }
                    $data .= "[\"$monthList[$j - 1] $i\", 0],";
                    $previousDate[0] = $i;
                    $previousDate[1] = $j;
                }

                $j = 1;
            }
        }

        return ($firstTime, $previousDate[0], $previousDate[1], $data);
    };
    # --------------------------------------------------------------------
    # End closure to fill in months
    # --------------------------------------------------------------------

    # --------------------------------------------------------------------
    # Closure to fill in months from the last record to the end date.
    # --------------------------------------------------------------------
    my $fillInEndMonths = sub {
        my $currentYear = shift;
        my $currentMonth = shift;
        my $endYear = shift;
        my $endMonth = shift;
        my $endDay = shift;
        my $data = shift;
        my @monthList = qw(January February March April May June July August September October November December);

        if ($endDay > 1) {
            $endMonth += 1;
        }

        my $j = $currentMonth + 1;

        for (my $i = $currentYear; $i <= $endYear; $i++) {
            for ( ; $j < 13; $j++) {
                if ($i == $endYear && $j + 1 > $endMonth) {
                    last;
                }
                $data .= "[\"$monthList[$j - 1] $i\", 0],";
            }

            $j = 1;
        }

        return $data;

    };
    # --------------------------------------------------------------------
    # End closure to fill in months from the last record to the end date.
    # --------------------------------------------------------------------

    # --------------------------------------------------------------------
    # Main body of the function. All main logic happens here.
    # --------------------------------------------------------------------
    # Get arguments from function
    my $self = shift;
    my $options = shift;
    my $project = shift;
    my $startDate = shift;
    my $endDate = shift;
    my $timeOffset = shift;
    my $db = EQuery->new();
    my $chartCounter = 1;
    my $markup = "";
    my $js = "";
    my $timeFunctions = "";
    my $groupTimeFunctions = "";

    my @optionsArray = split('', $options);

    # If no date range was supplied, fetch the default min and max order dates
    unless ($startDate =~ /^(\d{4}-\d{2})$/ || $endDate =~ /^(\d{4}-\d{2})$/) {
        $sql = "SELECT DATE(MIN(delivery_date)), DATE(MAX(delivery_date)) FROM codiac_web_order_log WHERE archive_ident IN (SELECT d.archive_ident FROM dataset d LEFT JOIN dataset_project dp ON d.id=dp.dataset_id LEFT JOIN project p ON dp.project_id=p.id WHERE p.name = '${project}') AND email NOT IN (" . Utils::getIgnoredEmailAddresses() . ");";
        $db->query($sql);
        %row = $db->getRow();
        unless ($startDate =~ /^(\d{4}-\d{2}-\d{2})$/) {
            $startDate = $row{"DATE(MIN(delivery_date))"};
        }
        unless ($endDate =~ /^(\d{4}-\d{2}-\d{2})$/) {
            $endDate = $row{"DATE(MAX(delivery_date))"};
        }
    }

    # Get the number of days in between the start and end date to figure out grouping by month or year
    my @startDateArray = split('-', $startDate);
    my @endDateArray = split('-', $endDate);

    # If the user didn't specify a time subset, intelligently determine it now
    if (!defined $timeOffset) {
        my $deltaDays = Delta_Days(@startDateArray, @endDateArray);
        if ($deltaDays >= (365 * 2)) {
            $timeOffset = 'yearly';
        } else {
            $timeOffset = 'monthly';
        }
    }

    if ($timeOffset eq 'yearly') {
        $timeFunctions = "year(delivery_date)";
        $groupTimeFunctions = "year(delivery_date)";
    } else {
        $timeFunctions = "month(delivery_date), monthname(delivery_date), year(delivery_date)";
        $groupTimeFunctions = "month(delivery_date), year(delivery_date)";
    }

    # Iterate for each option that the user selected
    foreach my $option(@optionsArray) {
        my $sql = "SELECT FROM codiac_web_order_log WHERE archive_ident IN (SELECT d.archive_ident FROM dataset d LEFT JOIN dataset_project dp ON d.id=dp.dataset_id LEFT JOIN project p ON dp.project_id=p.id WHERE p.name = '" . $project . "') AND email NOT IN (" . Utils::getIgnoredEmailAddresses() . ") AND delivery_date BETWEEN \'$startDate 00:00:00\' AND \'$endDate 00:00:00\'";
        if ($option == 1) {
            my $xAxisLabel = "";
            my $yAxisLabel = "";
            my $data = "var data = [";
            $markup .= "<div id=\"${project}Chart${chartCounter}\" style=\"height:425px;margin-right:20px;margin-left:20px;\"></div>\n<div id=\"${project}Tooltip${chartCounter}\" style=\"position:absolute;display:none;border:1px solid #000;padding:2px;background-color:#438ac9;color:#fff;opacity:0.80;\"></div>\n<script></script><br><br><br>";
            my $firstTime = 1;
            my $previousDate = undef;
            my $previousMonth = undef;
            my $previousYear = undef;

            # Orders over time
            if ($chartCounter == 1) {
                $yAxisLabel = "Orders";
                $sql =~ s/(SELECT)(.*)/$1 count(email), $timeFunctions $2 group by $groupTimeFunctions order by delivery_date;/;
                $db->query($sql);
                while (%row = $db->getRow()) {
                    if ($row{"count(email)"} == 0) {
                        $row{"count(email)"} = 0;
                    }

                    # Add data to JS array
                    if ($timeOffset eq 'yearly') {
                        $xAxisLabel = "Years";

                        ($firstTime, $previousDate, $data) = $fillInYears->($firstTime, $previousDate, $data, "year(delivery_date)", $startDateArray[0]);

                        $data .= "[\"" . $row{"year(delivery_date)"} . "\t\"," . $row{"count(email)"} . "],";
                        $previousDate = $row{"year(delivery_date)"};
                    } else {
                        $xAxisLabel = "Months";

                        ($firstTime, $previousYear, $previousMonth, $data) = $fillInMonths->($firstTime, $previousYear, $previousMonth, $data, "year(delivery_date)", "month(delivery_date)", $startDateArray[0], $startDateArray[1]);

                        $data .= "[\"" . $row{"monthname(delivery_date)"} . " " . $row{"year(delivery_date)"} . "\"," . $row{"count(email)"} . "],";
                        $previousMonth = $row{"month(delivery_date)"};
                        $previousYear = $row{"year(delivery_date)"};
                    }
                }

                if ($timeOffset eq 'yearly') {
                    $data = $fillInEndYears->($previousDate, $endDateArray[0], $data);
                } else {
                    $data = $fillInEndMonths->($previousYear, $previousMonth, $endDateArray[0], $endDateArray[1], $endDateArray[2], $data);
                }
            } elsif ($chartCounter == 2) { # Volume served over time
                $yAxisLabel = "Filesize (MB)";
                $sql =~ s/(SELECT)(.*)/$1 sum(size_kb), $timeFunctions $2 group by $groupTimeFunctions order by delivery_date;/;
                $db->query($sql);
                while (%row = $db->getRow()) {
                    if ($row{"sum(size_kb)"} == undef) {
                        $row{"sum(size_kb)"} = 0;
                    }

                    if ($timeOffset eq 'yearly') {
                        $xAxisLabel = "Years";

                        ($firstTime, $previousDate, $data) = $fillInYears->($firstTime, $previousDate, $data, "year(delivery_date)", $startDateArray[0]);

                        $data .= "[\"" . $row{"year(delivery_date)"} . "\"," . int($row{"sum(size_kb)"} / 1024) . "],";
                        $previousDate = $row{"year(delivery_date)"};
                    } else {
                        $xAxisLabel = "Months";

                        ($firstTime, $previousYear, $previousMonth, $data) = $fillInMonths->($firstTime, $previousYear, $previousMonth, $data, "year(delivery_date)", "month(delivery_date)", $startDateArray[0], $startDateArray[1]);

                        $data .= "[\"" . $row{"monthname(delivery_date)"} . " " . $row{"year(delivery_date)"} . "\"," . int($row{"sum(size_kb)"} / 1024) . "],";
                        $previousMonth = $row{"month(delivery_date)"};
                        $previousYear = $row{"year(delivery_date)"};
                    }
                }

                if ($timeOffset eq 'yearly') {
                    $data = $fillInEndYears->($previousDate, $endDateArray[0], $data);
                } else {
                    $data = $fillInEndMonths->($previousYear, $previousMonth, $endDateArray[0], $endDateArray[1], $endDateArray[2], $data);
                }
            } elsif ($chartCounter == 3) { # Files served over time
                $yAxisLabel = "Number of Files";
                $sql =~ s/(SELECT)(.*)/$1 sum(num_files), $timeFunctions $2 group by $groupTimeFunctions order by delivery_date;/;
                $db->query($sql);
                while (%row = $db->getRow()) {
                    if ($row{"sum(num_files)"} == undef) {
                        $row{"sum(num_files)"} = 0;
                    }

                    if ($timeOffset eq 'yearly') {
                        $xAxisLabel = "Years";

                        ($firstTime, $previousDate, $data) = $fillInYears->($firstTime, $previousDate, $data, "year(delivery_date)", $startDateArray[0]);

                        $data .= "[\"" . $row{"year(delivery_date)"} . "\"," . $row{"sum(num_files)"} . "],";

                        $previousDate = $row{"year(delivery_date)"};
                    } else {
                        $xAxisLabel = "Months";

                        ($firstTime, $previousYear, $previousMonth, $data) = $fillInMonths->($firstTime, $previousYear, $previousMonth, $data, "year(delivery_date)", "month(delivery_date)", $startDateArray[0], $startDateArray[1]);

                        $data .= "[\"" . $row{"monthname(delivery_date)"} . " " . $row{"year(delivery_date)"} . "\"," . $row{"sum(num_files)"} . "],";
                        $previousMonth = $row{"month(delivery_date)"};
                        $previousYear = $row{"year(delivery_date)"};
                    }
                }

                if ($timeOffset eq 'yearly') {
                    $data = $fillInEndYears->($previousDate, $endDateArray[0], $data);
                } else {
                    $data = $fillInEndMonths->($previousYear, $previousMonth, $endDateArray[0], $endDateArray[1], $endDateArray[2], $data);
                }
            }
            $data .= "];";

            $markup =~ s/(<script>)(<\/script>)/$1\n\$(document).ready(function() {\n\t$data\n\t\$\.plot(\"#${project}Chart${chartCounter}\", [data], { colors:[\"#336699\"], canvas:true, series: { points: { show:false }, bars: { show:true,barWidth:0.8,align:\"center\" } }, xaxis: { mode:\"categories\", tickLength:0, rotateTicks:135, axisLabel:'$xAxisLabel', axisLabelUseCanvas:true, axisLabelPadding:20 }, yaxis:{axisLabel:'$yAxisLabel', axisLabelUseCanvas:true,axisLabelPadding:45 },
               grid: { hoverable:true, clickable:true }}); \$("#${project}Chart${chartCounter}").bind("plothover", function (event, pos, item) { if (item) { var x = item.datapoint[0],y = item.datapoint[1];\$("#${project}Tooltip${chartCounter}").html(y).css({top: item.pageY+5, left: item.pageX+5}).fadeIn(200); } else { \$("#${project}Tooltip${chartCounter}").hide(); } }); var url = \$('#${project}Chart${chartCounter}>:first-child')[0].toDataURL(); \$('#${project}Chart${chartCounter}').before('<a href=' + url  + ' target=_blank>Click to Save Graph as PNG<\/a>'); });\n$2/;
        }

        $chartCounter++;
    }

    return $markup . "<br>";
}

# ------------------------------------------------------------------------
# Fetches all projects in the database to display in a select box
# ------------------------------------------------------------------------
sub getProjects {
    my $options = "";
    my $db = EQuery->new();
    $sql = "SELECT id,name FROM project ORDER BY name ASC;";
    $db->query($sql);

    while(my %row = $db->getRow()) {
        $options = $options . "<option value=\"$row{name}\">$row{name}</option>"
    }

    return $options;
}

1;
