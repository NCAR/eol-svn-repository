#! /usr/bin/perl -w

##Module----------------------------------------------------------------------
#<a href=http://dmg.eol.ucar.edu/cgi-bin/docperl/docperl?file=/net/work/lib/perl/mysql/MySqlTestDatabase.pm>Link to MySqlTestDatabase.pm</a><br />
#<a href=http://dmg.eol.ucar.edu/cgi-bin/docperl/docperl?file=/net/work/lib/perl/mysql/MySqlTestDataset.pm>Link to MySqlTestDataset.pm</a><br />
#<a href=http://dmg.eol.ucar.edu/cgi-bin/docperl/docperl?file=/net/work/lib/perl/mysql/MySqlFile.pm>Link to MySqlFile.pm</a><br />
#<a href=http://dmg.eol.ucar.edu/cgi-bin/docperl/docperl?file=/net/work/lib/perl/mysql/MySqlMSSFile.pm>Link to MySqlMSSFile.pm</a><br />
#<a href=http://dmg.eol.ucar.edu/cgi-bin/docperl/docperl?file=/net/work/lib/perl/mysql/MySqlInserter.pm>Link to MySqlInserter.pm</a><br />
#<a href=http://dmg.eol.ucar.edu/cgi-bin/docperl/docperl?file=/net/work/lib/perl/mysql/MySqlMSSInserter.pm>Link to MySqlMSSInserter.pm</a><br />
# <p>The MySqlTestDatabase.pm module is a class that controls the interaction
# between a Perl script and the MySQL database.  Its purpose is to maintain
# the connection and execute the commands to the database.  The connection
# is defined to be a manual commit to allow transactions to occur and allow
# rollback for error recover.</p>
#
# @author Joel Clawson
##Module----------------------------------------------------------------------
package MySqlTestDatabase;
use strict;
use lib qw(/dmsdev/perl/lib/site_perl);
use DBI;

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
    
    my $db = DBI->connect(sprintf("DBI:mysql:database=zith9b;host=merlot.eol.ucar.edu;"), #localhost;"),
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

    # Can only insert on a connected database.
    if (defined($self->{"database"})) {
	my $insert = sprintf("INSERT INTO %s(%s) VALUES(%s)",$table,$attrs,$values);
	$self->{"database"}->prepare($insert)->execute() ||
	    return "Unable to perform insert: $insert\n";
	return "";
    } else {
	return "Connection for database not found for insert.\n";
    }
}

##----------------------------------------------------------------------------
# @signature MySqlTestDatabase new(String user, String pass)
# <p>Create a new object.</p>
#
# @input $user The name of the user to use to connect to the database.
# @input $pass The password for the user.
# @output $self The new MySqlTestDatabase.
##----------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);

    die("Did not specify a user and password for MySqlTestDatabase->new\n") if (@_ != 2);

    ($self->{"user"},$self->{"pass"}) = @_;
    $self->setHost("merlot.eol.ucar.edu");
    $self->{"maxaffectedrows"} = 1;
    return $self;
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

    if (defined($self->{"database"})) {
	my $update = sprintf("UPDATE %s SET %s WHERE %s",$table,$attrs,$tests);
	my $sth = $self->{"database"}->prepare($update);
	$sth->execute() or return "Unable to perform update: $update\n";
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
1;
