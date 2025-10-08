#! /usr/bin/perl -w

##Module----------------------------------------------------------------------
#<a href=http://dmg.eol.ucar.edu/cgi-bin/docperl/docperl?file=/net/work/lib/perl/mysql/MySqlDatabase.pm>Link to MySqlDatabase.pm</a><br />
#<a href=http://dmg.eol.ucar.edu/cgi-bin/docperl/docperl?file=/net/work/lib/perl/mysql/MySqlDataset.pm>Link to MySqlDataset.pm</a><br />
#<a href=http://dmg.eol.ucar.edu/cgi-bin/docperl/docperl?file=/net/work/lib/perl/mysql/MySqlFile.pm>Link to MySqlFile.pm</a><br />
#<a href=http://dmg.eol.ucar.edu/cgi-bin/docperl/docperl?file=/net/work/lib/perl/mysql/MySqlMSSFile.pm>Link to MySqlMSSFile.pm</a><br />
#<a href=http://dmg.eol.ucar.edu/cgi-bin/docperl/docperl?file=/net/work/lib/perl/mysql/MySqlInserter.pm>Link to MySqlInserter.pm</a><br />
#<a href=http://dmg.eol.ucar.edu/cgi-bin/docperl/docperl?file=/net/work/lib/perl/mysql/MySqlMSSInserter.pm>Link to MySqlMSSInserter.pm</a><br />
# <p>The MySqlDatabase.pm module is a class that controls the interaction
# between a Perl script and the MySQL database.  Its purpose is to maintain
# the connection and execute the commands to the database.  The connection
# is defined to be a manual commit to allow transactions to occur and allow
# rollback for error recover.</p>
#
# @author Joel Clawson
##Module----------------------------------------------------------------------
package MySqlDatabase;
use strict;
use DBI;
#use Log::Log4perl qw(:easy);

##----------------------------------------------------------------------------
# @signature MySqlDatabase new([String user, String pass, String host, String db, String contact, String my_defaults_file, String my_defaults_group])
# <p>Create a new object.
# All arguments are optional. If user and pass are given then db must also be given.
# If one of user,pass are not given then the mysql defaults file will be used.
# Examples:
# <ul>
# <li><blockquote><code>
#   # get all connection info from "perl" section of MySQL defaults file
#   $mysql = $MySqlDatabase->new();
# </code></blockquote></li>
# <li><blockquote><code>
#   # connect to "localhost"
#   $mysql = $MySqlDatabase->new("myuser","mypass",undef,"mydb");
# </code></blockquote></li>
# <li><blockquote><code>
#   # no user,pass so use MySQL defaults file plus override contact
#   $mysql = $MySqlDatabase->new(undef,undef,undef,undef,"foo");
# </code></blockquote></li>
# <li><blockquote><code>
#   # specify MySQL defaults file and group in code
#   $mysql = $MySqlDatabase->new(undef,undef,undef,undef,undef,"/path/to/.my.cnf","mygroup");
# </code></blockquote></li>
# </ul>
# </p>
# <p>
# MySQL defaults filename precendence:
# <ol>
# <li><code>new()</code> argument</li>
# <li>environment variable <code>DMG_MYCNF_PATH</code> if it exists</li>
# <li><code>$HOME/.my.cnf</code> if environment variable HOME exists</li>
# <li><code>~/.my.cnf</code></li>
# </ol>
# If the filename starts with "~" then it is expanded with perl's glob() function.
# Filename possibles are not tried, the first string found above will be used.
# If it does not exist or is unreadable, then the script die()s.
# </p>
#
# @input $user The name of the user to use to connect to the database.
# @input $pass The password for the user.
# @input $host The host where the database is located (default "localhost").
# @input $db The database name (default "zith9").
# @input $contact Optional short_name or email prefix for the audit contact (default $USER or $DMG_SCRIPT_USER).
# @input $my_defaults_file Optional filepath for the mysql defaults file ($DMG_MYCNF_PATH or ~/.my.cnf).
# @input $my_defaults_group Optional group name in the mysql defaults file ($DMG_MYCNF_GROUP or "perl"; if you want "client" then use DMG_MYCNF_GROUP).
# @output $self The new MySqlDatabase.
##----------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);

    # argument overrides
    my @vars = ( "user", "pass", "host", "db", "contact", "mycnf", "mygroup", );
    foreach my $key (@vars) {
        $self->{$key} = shift;
    }

    ## set any missing keys to defaults

    $self->{"verbose"} = $ENV{"DMG_VERBOSE"};

    # user,pass: no defaults (use mycnf)
    # host: DBI will use default "localhost"

    # when user,pass are given with no defaults file, provide a default db
    if ($self->{"user"} && $self->{"pass"} && !$self->{"mycnf"}) {
        if (!$self->{"db"}) {
            $self->{"db"}="zith9";
        }
        if (!$self->{"host"}) {
            $self->{"host"}="emdac.eol.ucar.edu";
        }
    }

    if (!$self->{"contact"}) {
        $self->{"contact"}=$ENV{'DMG_SCRIPT_USER'};
    }

    # when using mysql defaults file, be sure defaults are set
    if (!($self->{"user"} && $self->{"pass"}) || $self->{"mycnf"} || $self->{"mygroup"}) {
        if (!$self->{"mycnf"}) {
            print "No mycnf specified...\n" if ($self->{"verbose"});
            if ($ENV{'DMG_MYCNF_PATH'}) {
                $self->{"mycnf"} = $ENV{'DMG_MYCNF_PATH'};
                print "Using DMG_MYCNF_PATH as ", $self->{"mycnf"}, "\n" if ($self->{"verbose"});
            } elsif ($ENV{'HOME'}) {
                $self->{"mycnf"} = $ENV{'HOME'} . "/.my.cnf";
                print "Using HOME as ", $self->{"mycnf"}, "\n" if ($self->{"verbose"});
            } else {
                $self->{"mycnf"} = "~/.my.cnf";
                print "Using ~ as ", $self->{"mycnf"}, "\n" if ($self->{"verbose"});
            }
        }
        if (!$self->{"mygroup"}) {
            $self->{"mygroup"} = $ENV{'DMG_MYCNF_GROUP'} ? $ENV{'DMG_MYCNF_GROUP'} : "perl";
        }
    }

    # if we have a mycnf that starts with ~ for finding home dirs
    if ($self->{"mycnf"} && ($self->{"mycnf"} =~ /^~/)) {
        # expand path, DBD::mysql no longer does that
        my $globmycnf = glob($self->{"mycnf"});
        print "glob for ", $self->{"mycnf"}, " returns ",
          ($globmycnf ? $globmycnf : "(undefined)"),
          "\n" if ($self->{"verbose"});
        # but only if it works
        $self->{"mycnf"} = $globmycnf if ($globmycnf);
        print "mycnf now ", $self->{"mycnf"}, "\n" if ($self->{"verbose"});
    }
    if (!$self->{"mycnf"}) {
      die "no mysql config file specified, or glob expansion didn't work" .
        "\nSet DMG_MYCNF_PATH to desired file, or unset it and create ~/.my.cnf\n";
    }
    if (!-r $self->{"mycnf"}) {
      die "mysql config file unreadable or does not exist: " .
        $self->{"mycnf"} .
        "\nSet DMG_MYCNF_PATH to readable file, or unset it and create ~/.my.cnf\n";
    }
    print "Finally using mycnf file ", $self->{"mycnf"}, "\n" if ($self->{"verbose"});

    $self->{"maxaffectedrows"} = 1;

    #Log::Log4perl->easy_init($DEBUG);
    #$self->{'logger'} = get_logger();

    return $self;
}

##----------------------------------------------------------------------------
# @signature String commit()
# <p>Finalize the commands sent to the database so they will be remembered.</p>
#
# @output $err An error message if the commit failed or the empty string if
# the commit saved the data.
##----------------------------------------------------------------------------
sub commit {
    my $self = shift;
    $self->{"database"}->commit() || return "Unable to commit the commands.\n";
    return "";
}

##----------------------------------------------------------------------------
# @signature String connect()
# <p>Establish a connection to the database.</p>
#
# @output $err An error message if the connection could not be established or
# the empty string if it was created.
##----------------------------------------------------------------------------
sub connect {
    my $self = shift;

    my $dsn = "DBI:mysql:";
    my $attr = {
          AutoCommit => 0,
          RaiseError => 0,
    };

    # database goes first by convention (probably not strictly so)
    if ($self->{"db"}) {
        $dsn.="database=".$self->{"db"};
    }

    if ($self->{"host"}) {
        $dsn.=";host=".$self->{"host"};
    }

    if ($self->{"mycnf"}) {
        $dsn.=";mysql_read_default_file=".$self->{"mycnf"};
    }
    if ($self->{"mygroup"}) {
        $dsn.=";mysql_read_default_group=".$self->{"mygroup"};
    }

    my $dbh = DBI->connect($dsn, $self->{"user"}, $self->{"pass"}, $attr) or
      return "Unable to establish a database connection: ".$DBI::errstr."\n";

    $self->{"database"} = $dbh;
    return "";
}

##----------------------------------------------------------------------------
# @signature String disconnect()
# <p>Close the connection to the database.</p>
#
# @output $err An error message if the connection was not closed cleanly or
# the empty string if everything went okay.
##----------------------------------------------------------------------------
sub disconnect {
    my $self = shift;

    if (defined($self->{"database"})) {
	$self->{"database"}->disconnect() or
	    return "Unable to cleanly disconnect from the database.\n";
	undef($self->{"database"});
	return "";
    } else {
	return "Cannot disconnect from undefined database.\n";
    }
}

##----------------------------------------------------------------------------
# @signature String insert(String table, String attrs, String values)
# <p>Insert an entry into a table of the database.</p>
#
# @input $table The name of the database table.
# @input $attrs The attributes of the table.
# @input $values The values of the attributes to be inserted.
# @output $err An error message if the insert was not successful or the empty
# string if it was inserted correctly.
##----------------------------------------------------------------------------
sub insert {
    my $self = shift;
    my $table = shift;
    my $attrs = shift;
    my $values = shift;
    my $insert = 'Error: unknown database setup problem.';

    # Can only insert on a connected database.
    if (defined($self->{"database"})) {

        # Check for EMDAC audit fields when do insert.
        $insert = $self->_make_audit_insert_sql($table,$attrs,$values);

        return $insert if ($insert =~ m/^Error:/);
	    my $numrows = $self->{"database"}->do($insert);
	    return "Unable to perform insert: $insert\n" if (!defined($numrows));
	    return "No rows inserted for: $insert\n" if (! $numrows);
        return '';
    } else {
        return "Connection for database not found for insert.\n";
    }
}


##----------------------------------------------------------------------------
# @signature String rollback()
# <p>Undo all of the commands sent to the database since the last commit.</p>
#
# @output $err An error message if the rollback could not be done or the
# empty string if the rollback was successful.
##----------------------------------------------------------------------------
sub rollback {
    my $self = shift;
    $self->{"database"}->rollback() || return "Unable to rollback the commands.\n";
    return "";
}

##----------------------------------------------------------------------------
# @signature (String, Object[]) select(String table, String attrs, String tests)
# <p>Get the objects in the table that match the tests.</p>
#
# @input $table The table containing the data.
# @input $attrs The attributes to select from the table.
# @input $tests The list of tests that must be true for the selection to occur (the WHERE clause).
# @output $err An error message or the empty string.
# @output results[] The values for the selected attributes.
# @warning This method is meant to only select a single instance from the table. 
##----------------------------------------------------------------------------
sub select {
    my $self = shift;
    my $table = shift;
    my $attrs = shift;
    my $tests = shift;
    my $flag = shift;

    # Can only select if database is open
    if (defined($self->{"database"})) {

	my $select = sprintf("SELECT %s FROM %s",$attrs,$table);
	if (defined($tests)) { 
	    my @attr = ($tests);
	    if ($table eq "project") {$self->add_quotes_to_string($table,\@attr);}
	    $select .= sprintf(" WHERE %s",$attr[0]);
	}

	my $sth = $self->{"database"}->prepare($select);
	$sth->execute() or return "Unable to perform select: $select\n";

	my @results;
	$self->{"rows"} = $sth->rows();
	@results = $sth->fetchrow_array();

	return ("",@results);
    } else {
	return "Connection to database not found for select.\n";
    }
}


##----------------------------------------------------------------------------
# @signature (String msg, String[$row][$col] data) selectAll(String table, String attrs, String tests)
# <p>Get the objects in the table that match the tests.</p>
#
# @input $table The table containing the data.
# @input $attrs The attributes to select from the table.
# @input $tests The list of tests that must be true for the selection to occur (the WHERE clause).
# @output $msg An error message or an empty string.
# @output $data[$row][$col] 2D Array of row data.
##----------------------------------------------------------------------------
sub selectAll {
    my $self = shift;
    my $table = shift;
    my $attrs = shift;
    my $tests = shift;

    # Can only select if database is open
    if (defined($self->{"database"})) {
        my $select = sprintf("SELECT %s FROM %s",$attrs,$table);
        if (defined($tests)) {
            $select .= sprintf(" WHERE %s",$tests);
        }
        my $sth = $self->{"database"}->prepare($select);
        $sth->execute() or return "Unable to perform select: $select\n";

        my @results;

        $self->{"rows"} = $sth->rows();
	while (my @row = $sth->fetchrow_array())
	{
		push @results, [ @row ];
	}

        return ("",@results);
    } else {
        return "Connection to database not found for select.\n";
    }
}

##----------------------------------------------------------------------------
# @signature (String, Hash) selectFull(String table, String attrs, String tests)
# <p>Get the objects in the table that match the tests.</p>
#
# @input $table The table containing the data.
# @input $attrs The attributes to select from the table.
# @input $tests The list of tests that must be true for the selection to occur (the WHERE clause).
# @output $err An error message or the empty string.
# @output hash containg retrieved information
# @output %hash{"name"} contains an array of column names
# @output %hash{"type"} contains an array of column types (numeric)
# @output %hash{"row"} contains an array of arrays containing the data of a single row (See selectAll)
##----------------------------------------------------------------------------
sub selectFull {
    my $self = shift;
    my $table = shift;
    my $attrs = shift;
    my $tests = shift;
    my $limit = shift;

    # Can only select if database is open
    if (defined($self->{"database"})) {
        my $select = sprintf("SELECT %s FROM %s",$attrs,$table);
        if (defined($tests) && $tests ne "") {
            $select .= sprintf(" WHERE %s",$tests);
        }
        if (defined($limit)) {
            $select .= sprintf(" LIMIT %s",$limit);
        }
        my $sth = $self->{"database"}->prepare($select);
        $sth->execute() or return "Unable to perform select: $select\n";

        my %results;
	$results{"name"} = $sth->{NAME};
	$results{"type"} = $sth->{TYPE};
	
        my @rows1;
	$self->{"rows"} = $sth->rows();
	while (my @row = $sth->fetchrow_array())
	{
		push @rows1, [ @row ];
	}
	$results{"row"} = [ @rows1 ];

        return ("",%results);
    } else {
        return "Connection to database not found for select.\n";
    }
}

##----------------------------------------------------------------------------
# @signature String update(String table, String attrs, String tests)
# <p>Update the table in the database with the specified attributes.</p>
#
# @input $table The table to be updated.
# @input $attrs A list of attribute value pairs to be updated.
# @input $tests A list of conditions that must be met for the update to happen (the WHERE clause).
# @output $err An error message or the empty string.
##----------------------------------------------------------------------------
sub update {
    my $self = shift;
    my $table = shift;
    my $attrs = shift;
    my $tests = shift;
	my $update = 'Error: unknown database setup problem.';

    if (defined($self->{"database"})) {

        # Check for EMDAC audit fields when do insert.
	$update = $self->_make_audit_update_sql($table,$attrs,$tests);

        return $update if ($update =~ m/^Error:/);

	    my $sth = $self->{"database"}->prepare($update);
        return "Unable to prepare update SQL: $update\n" if (!defined($sth));
	    $sth->execute() or return "Unable to perform update: $update\n";

	    $self->{"rows"} = $sth->rows();
	    if ($self->{"rows"} > $self->{"maxaffectedrows"}) {
		    return $self->rollback() .
              "Too many rows affected by update (" .
              $self->{"rows"} .
              "), database rolled back.\n";
	        }
        return "";
    } else {
	return "Connection to database not found for update.\n";
    }
}

##----------------------------------------------------------------------------
# @signature String delete(String table, String tests)
# <p>Delete a row of a table in the database matching the specified test. This function will ONLY delete 1 row at a time.</p>
#
# @input $table The table to be updated.
# @input $tests A list of conditions that must be met for the delete to happen (the WHERE clause).
# @output $err An error message or the empty string.
##----------------------------------------------------------------------------
sub delete {
    my $self = shift;
    my $table = shift;
    my $tests = shift;

    if (defined($self->{"database"})) {
	my $delete = sprintf("DELETE FROM  %s WHERE %s LIMIT 1",$table,$tests);
	my $sth = $self->{"database"}->prepare($delete);
	$sth->execute() or return "Unable to perform deletion: $delete\n";
	$self->{"rows"} = $sth->rows();
	if ($self->{"rows"} > $self->{"maxaffectedrows"})
	{
		return $self->rollback() . "Too many rows affected by update (" . $self->{"rows"} . "), database rolled back.\n";
	}
        return "";
    } else {
	return "Connection to database not found for update.\n";
    }
}


sub getRows {
	my $self = shift;
	return $self->{"rows"};
}


##-----------------------------------------------------------------------------
# @signature String getDB()
# <p>Get the DB string</p>
#
# @output $db the mysql server host
##-----------------------------------------------------------------------------
sub getDB {
	my $self = shift;
	return $self->{"db"};
}

##-----------------------------------------------------------------------------
# @signature Void setDB(String host)
# <p>Set the DB string</p>
#
# @input $db the mysql server host
##-----------------------------------------------------------------------------
sub setDB {
	my $self = shift;
	$self->{"db"} = $_[0];
}
##-----------------------------------------------------------------------------
# @signature String getHost()
# <p>Get the Host string</p>
#
# @output $host the mysql server host
##-----------------------------------------------------------------------------
sub getHost {
	my $self = shift;
	return $self->{"host"};
}

##-----------------------------------------------------------------------------
# @signature Void setHost(String host)
# <p>Set the Host string</p>
#
# @input $host the mysql server host
##-----------------------------------------------------------------------------
sub setHost {
	my $self = shift;
	$self->{"host"} = $_[0];
}

##-----------------------------------------------------------------------------
# @signature Void setMaxAffectedRows(Int max)
# <p>Sets the maximum number of lines that my be updated by the update function in a single call</p>
#
# @input $max Maximum number of lines
##-----------------------------------------------------------------------------
sub setMaxAffectedRows{
	my $self = shift;
	$self->{"maxaffectedrows"} = $_[0];
}

##-----------------------------------------------------------------------------
# @signature String getRevisorId()
# <p>Get the RevisorId string</p>
#
# @output $host the mysql server host
##-----------------------------------------------------------------------------
sub getRevisorId {
	my $self = shift;
    #my $log = $self->{'logger'};

    if (!defined($self->{'revisor_id'})) {
        my $err = $self->_setRevisor();
        if ($err ne '') {
            #$log->error($err);
            print STDERR $err,"\n";
	        undef($self->{'revisor_id'});
            }
        }

	return $self->{'revisor_id'};
}

##-----------------------------------------------------------------------------
# @signature Void setRevisorId(String host)
# <p>Set the RevisorId string</p>
#
# @input $host the mysql server host
##-----------------------------------------------------------------------------
sub setRevisorId {
	my $self = shift;
	$self->{'revisor_id'} = $_[0];
}
##-----------------------------------------------------------------------------
# @signature Void add_quotes_to_string
##-----------------------------------------------------------------------------
sub add_quotes_to_string {
    my $self=shift;
    my $table = shift;
    my $test_ref = shift;
    my $val_ref = shift;

    my @fields = map {$_->[0]}
        @{$self->{"database"}->selectall_arrayref(qq{describe $table})};
    my @types = map {$_->[1]}
        @{$self->{"database"}->selectall_arrayref(qq{describe $table})};

    for (my $j=0;$j<scalar (@$test_ref);$j++) {
      for (my $i=0;$i<scalar (@fields);$i++) {
	my $field = $fields[$i];

	if (${$test_ref}[$j] =~ /$field = (.*)/) {
	    ${$test_ref}[$j] = "$fields[$i] = \"$1\"";
	last;
	} elsif ($$test_ref[$j] =~ /$field/) {
	    if ($types[$i] =~ /varchar/ || $types[$i] =~ /text/) {
		if ($$val_ref[$j] !~ /^['"]/) {
	            $$val_ref[$j] = "\"$$val_ref[$j]\"";
	        }
	last;
	    }
	}
      }
    }
}

#
# private function to fetch the revisor_id from the database
# (contact.id for the OS user)
#
sub _setRevisor {
	my $self = shift;
    #my $log = $self->{'logger'};

    my $username = getpwuid($<);
    #$log->debug("ruid $< username $username") if ($log->is_debug());
    return "Cannot get system username\n" if (!defined($username));

    my $email = $username . '@ucar.edu';

    return "Connection for database not found for revisor contact determination.\n"
        if (!defined($self->{"database"}));

    my $sql = 'select id from contact where email = ?';
	my $val = $self->{"database"}->selectrow_array($sql,
      {}, # { Columns=>[1] },
      ($email) );
    #if ( ($log->is_debug()) and (defined($val)) ) {
      #$log->debug("db result $val");
    #} else {
      #$log->debug('db result (undefined)');
    #}

        # ###
        #
        # The environment variable DMG_SCRIPT_USER allows a script to run anonymously,
        # using the generic "dmgscripts" database contact. This contact should only
        # be used when running un-attended scripts as a user without a real contact record.
        # If this is a long-term situation then a new contact needs to be created.
        #
        # short version:
        #  csh: setenv DMG_SCRIPT_USER dmgscripts
        #   sh: export DMG_SCRIPT_USER; DMG_SCRIPT_USER=dmgscripts
        # Read the full comments and talk to jja.
        #
        # This should normally not be needed, as the _I_am_interactive subroutine
        # will allow cron jobs from unknown users to act as "dmgscripts".
        # It might be needed when run interactively from a shared system
        # account (e.g. eoldata or joss), but that's a bad practice. Don't do it.
        # Shared accounts are bad. Groups and permissions are good.
        #
        # If you must, then ***temporarily*** set DMG_SCRIPT_USER to your
        # personal database contact email username (before the '@ucar.edu').
        # If for some reason, cron jobs don't work without DMG_SCRIPT_USER then
        # set it to "dmgscripts" and talk to jja about it.
        #
        # DO NOT SET DMG_SCRIPT_USER INSIDE YOUR SCRIPT AND LEAVE IT THAT WAY
        # AND COMMIT THAT CODE AND RUN LIKE THAT FOREVER!!!1!
        # An appropriate database contact should instead be created for
        # the real user(s) running the script. Please do not create
        # lots of fake contacts with fake email addresses.
        # A few specific script-like/automated users already exist
        # (they are found below via short_name when this code is NOT interactive).
        # Do not add fake email addresses to these special contacts (with IDs
        # close to 1000).
        #
        # ###
    if (!defined($val) && _I_am_interactive() && !$self->{"contact"}) {
        # couldn't find system user in DB,
        # and this is an interactive terminal,
        # and no env var
        #$log->debug('returning interactive error');
        my $pkg = __PACKAGE__;
        my $fil = __FILE__;
        my $lin = __LINE__;
        return <<EOMSG
Cannot find a database `contact` ID for you (${email}).
In order to run this interactively, you need to have
a proper `contact` record matching the above email,
or you need to specify which contact to use.

If you are running as your personal user, then
perhaps you need to be added as a database/Codiac/zinc/zith editor.

If you running interactively from a shared system account:
DO NOT just add a contact with the "missing" email.
DO NOT add this email to an existing contact.
Instead, please supply your personal `contact` username
to be used for the database changes.

The program you are running ($0) may have some command line argument or
other way to provide your personal username.
Otherwise, the environment variable DMG_SCRIPT_USER can be set temporarily.
DO NOT set this var in a shared account's startup files
(.cshrc, .my_defaults, etc).

For example:

    setenv DMG_SCRIPT_USER myusername

Instead of "myusername", use the the prefix of your email address
(before the "\@ucar.edu"). If that doesn't work, try the
`contact.short_name` (seek help in this case).

If you see this message from a script run from cron
and/or by a shared account user, then use "dmgscripts"
for the username and seek help for a permanent username.

Read the comments in the source code for explication and
talk to jja for further help:
 svn web: http://svn.eol.ucar.edu/websvn/filedetails.php?repname=dmg&path=%2Flibrary%2Fperl%2FMySQL%2Ftrunk%2Flib%2FMySqlDatabase.pm
 package: ${pkg}
 file:    ${fil}
 line:    ${lin}
EOMSG
    }

    if (!defined($val)) {
        # couldn't find system user in DB, so get default contact

        # use env var or default
        my $short_name = $self->{"contact"} ? $self->{"contact"} : 'dmgscripts';

        my $sql = 'select id from contact where short_name = ?';
        $val = $self->{"database"}->selectrow_array($sql,undef,$short_name);
        #if ( ($log->is_debug()) and (defined($val)) ) {
          #$log->debug("db result for short_name $short_name = $val");
        #} else {
          #$log->debug("db result for short_name $short_name = (undef)");
        #}

        if (!defined($val)) {
          my $email = $short_name . '@ucar.edu';
          my $sql = 'select id from contact where email = ?';
          $val = $self->{"database"}->selectrow_array($sql,undef,$email);
          #if ( ($log->is_debug()) and (defined($val)) ) {
            #$log->debug("db result for email $email = $val");
          #} else {
            #$log->debug("db result for email $email = (undef)");
          #}
          }
      }

        # still not found? then return error
    return "Unable to perform contact ID lookup -- database error or no contact found\n" if (!defined($val));

    if ($val =~ m/^(\d+)$/) {
        $self->setRevisorId($1);
        return '';
        }
    else {
        return "Bad contact ID from database.\n";
        }
}

#
# private function to add database auditing columns to an insert SQL
#
sub _make_audit_insert_sql {
    my $self = shift;
    my ($table,$attrs,$values) = @_;

    my $revisor;
    my $col_names = $self->{"database"}->selectcol_arrayref( qq{describe $table} );

    if ($attrs =~ m/row_(create|revise)/) {
        return "Error: specifying row_create_ or row_revise_ columns is not allowed. They are automatically set.\n";
    }

    if (grep(/row_(create|revise)_contact_id/,  @{$col_names}) ) {
        $revisor = $self->getRevisorId();
        if (defined($revisor)) {
            foreach my $field ('row_create_contact_id','row_revise_contact_id') {
                if ( grep(/^$field/, @{$col_names}) ) {
                    $attrs .= ',' . $field;
                    $values .= ',' . $revisor;
                }
            }
        }
        else {
            return "Error: no revisor contact ID.\n";;
        }
    }

    foreach my $field ('row_create_time') {
        if ( grep(/^$field/, @{$col_names}) ) {
            $attrs .= ',' . $field;
            $values .= ',NOW()';
        }
    }

    return sprintf("INSERT INTO %s (%s) VALUES (%s)",$table,$attrs,$values);


}

#
# private function to add database auditing columns to an update SQL
#
sub _make_audit_update_sql {
    my $self = shift;
    my ($table,$attrs,$tests) = @_;

    my $revisor;
    my $col_names = $self->{"database"}->selectcol_arrayref( qq{describe $table} );

    if ($attrs =~ m/row_(create|revise)/) {
        return "Error: specifying row_create_ or row_revise_ columns is not allowed. They are automatically set.\n";
    }

    if (grep(/row_(create|revise)_contact_id/,  @{$col_names}) ) {
        my $revisor = $self->getRevisorId();
        if (defined($revisor)) {
            if ( grep(/^row_revise_contact_id/, @{$col_names}) ) {
                $attrs .= ",row_revise_contact_id=${revisor}";
            }
        }
        else {
            return "Error: no revisor contact ID.\n";;
        }
    }

    if ( grep(/^row_revise_time/, @{$col_names}) ) {
        $attrs .= ",row_revise_time=NOW()";
    }

    return sprintf("UPDATE %s SET %s WHERE %s",$table,$attrs,$tests);

}

#
# private function to test whether a program is running interactively
# TODO: should move to a general-purpose library
# from Perl Cookbook, 2nd ed., 2003 (Bighorn Sheep), pp. 587-588
#
sub _I_am_interactive {
    use POSIX qw/getpgrp tcgetpgrp/;
    my $tty;
    #open($tty, '<', '/dev/tty') or die "can't open /dev/tty for testing interactiveness: $!";
    open($tty, '<', '/dev/tty') or return(0);
    my $tpgrp = tcgetpgrp(fileno($tty));
    my $pgrp = getpgrp();
    close $tty;
    return ($tpgrp == $pgrp);
}

1;
