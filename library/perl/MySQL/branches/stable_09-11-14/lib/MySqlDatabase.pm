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
# @signature MySqlDatabase new(String user, String pass)
# <p>Create a new object.</p>
#
# @input $user The name of the user to use to connect to the database.
# @input $pass The password for the user.
# @input $host The host where the database is located.
# @input $db The database
# @output $self The new MySqlDatabase.
##----------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);

    #Default
    #my $host = 'sferic.eol.ucar.edu';
    #my $host = 'localhost';
    my $host = 'farskol.eol.ucar.edu';
    my $db = 'zith9';

    if (@_ == 4) {
	$db = pop @_;
	$host = pop @_;
    }

    die("Did not specify a user and password for MySqlDatabase->new\n") if (@_ != 2);

    ($self->{"user"},$self->{"pass"}) = @_;
    $self->setHost($host);
    $self->setDB($db);
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
  
    my $db = DBI->connect(sprintf("DBI:mysql:database=".$self->getDB().";host=" . $self->getHost()),
			  $self->{"user"},$self->{"pass"},
			  {AutoCommit => 0, RaiseError => 0}) or 
			      return "Unable to establish a database connection to " . $self->getHost() . ".\n";

    $self->{"database"} = $db;
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

        # XXX only zith family of schemata have the EMDAC audit fields (?)
        if ($self->{"database"} =~ m/^zith/) {
          $insert = $self->_make_audit_insert_sql($table,$attrs,$values);
        } else {
          $insert = $self->_make_plain_insert_sql($table,$attrs,$values);
        }

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
	    $select .= sprintf(" WHERE %s",$tests);
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

    # Can only select if database is open
    if (defined($self->{"database"})) {
        my $select = sprintf("SELECT %s FROM %s",$attrs,$table);
        if (defined($tests)) {
            $select .= sprintf(" WHERE %s",$tests);
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

        # XXX only zith family of schemata have the EMDAC audit fields (?)
        if ($self->{"database"} =~ m/^zith/) {
	      $update = $self->_make_audit_update_sql($table,$attrs,$tests);
        } else {
	      $update = $self->_make_plain_update_sql($table,$attrs,$tests);
        }

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
        # using the generic "dmgscripts" database contact.
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
        # ###
    if (!defined($val) && _I_am_interactive() && !defined($ENV{'DMG_SCRIPT_USER'})) {
        # couldn't find system user in DB, and this is an interactive terminal, and no env var
        #$log->debug('returning interactive error');
        my $pkg = __PACKAGE__;
        my $fil = __FILE__;
        my $lin = __LINE__;
        return <<EOMSG
Cannot find a database contact ID for you (${email}).
Perhaps you need to be added as a database/Codiac/zinc/zith editor.
If this script is not working from a cron job or shared account,
then the environment variable DMG_SCRIPT_USER can be set.
Talk to jja. If not immediately available, read the comments in
the source code for explication:
 svn web: http://svn.eol.ucar.edu/websvn/filedetails.php?repname=dmg&path=%2Flibrary%2Fperl%2FMySQL%2Fbranches%2Fnew-with-revisor%2Flib%2FMySqlDatabase.pm
 package: ${pkg}
 file:    ${fil}
 line:    ${lin}
EOMSG
    }

	if (!defined($val)) {
        # couldn't find system user in DB, so get default contact

        # use env var or default
        my $short_name = defined($ENV{'DMG_SCRIPT_USER'}) ? $ENV{'DMG_SCRIPT_USER'} : 'dmgscripts';

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

    my $revisor = $self->getRevisorId();

    if (defined($revisor)) {
        if ($attrs =~ m/row_(create|revise)/) {
            return "Error: specifying row_create_ or row_revise_ columns is not allowed. They are automatically set.\n";
        }

        $attrs .= ',row_create_contact_id,row_revise_contact_id,row_create_time';
        $values .= ',' . $revisor . ',' . $revisor . ',NOW()';

        return sprintf("INSERT INTO %s (%s) VALUES (%s)",$table,$attrs,$values);
        }
    else {
        return "Error: no revisor contact ID.\n";;
        }

    return "Error: unknown problem making insert SQL.\n";;
}

#
# private function to add database auditing columns to an update SQL
#
sub _make_audit_update_sql {
	my $self = shift;
    my ($table,$attrs,$tests) = @_;

    my $revisor = $self->getRevisorId();

    if (defined($revisor)) {
        if ($attrs =~ m/row_(create|revise)/) {
            return "Error: specifying row_create_ or row_revise_ columns is not allowed. They are automatically set.\n";
        }

        $attrs .= ",row_revise_contact_id=${revisor}";
        return sprintf("UPDATE %s SET %s WHERE %s",$table,$attrs,$tests);
        }
    else {
        return "Error: no revisor contact ID.\n";;
        }

    return "Error: unknown problem making update SQL.\n";;
}

#
# private function to build insert SQL without audit columns
#
sub _make_plain_insert_sql {
	my $self = shift;
    my ($table,$attrs,$values) = @_;

    return sprintf("INSERT INTO %s (%s) VALUES (%s)",$table,$attrs,$values);
}

#
# private function to build update SQL without audit columns
#
sub _make_plain_update_sql {
	my $self = shift;
    my ($table,$attrs,$tests) = @_;

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
