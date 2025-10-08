#!/usr/bin/perl -w

#---------------------------------------------------------------------
# This object stores a row for the tables of the ivotuk CD.
# It performs the sorting and printing out of the html code.
# For use with mk_tables2.pl
#
# Author: Dan Sullivan
# Date:   Summer, 2002
#---------------------------------------------------------------------

package Row;

use strict;
use Util;

sub new
{
    my $classname = shift;
    my $self = {};
    bless( $self, $classname );
    $self->{group} = [];
    $self->{members} = [];
    $self->{date} = [];
    $self->{site} = [];
    $self->{sitegroup} = [];
    $self->{disc} = [];
    $self->{dataset} = undef;
    $self->{readme} = undef;

    return $self;
}   

sub addFields
{
    my $self = shift;
    my $new = shift;

    $self->addField( "group", $new->{group}[0] );
    $self->addField( "members", $new->{members}[0] );
    $self->addField( "date", $new->{date}[0] );
    $self->addField( "site", $new->{site}[0] );
    $self->addField( "sitegroup", $new->{sitegroup}[0] );
    $self->addField( "disc", $new->{disc}[0] );
}

# row->addField( "date", $val );
sub addField
{
    my $self = shift;
    my $field = shift;
    my $val = shift;

    my @arr = @{$self->{$field}};
    my $size = @arr; 
    my $exists = 0;

    for( my $x = 0; $x < $size; $x++ )
    {
        if( $self->{$field}[$x] eq $val )
        {
#if( $field eq "site" ) { print( "self: $self->{$field}[$x]   val: $val\n" ) };
            $exists = 1;
            last;   
        }
    }   

#print( "field: $field   size: $size  exists: $exists\n" );

    if( !$exists )
    {
        $self->{$field}[$size] = $val;
    }
}

sub sortAllFields
{
    my $self = shift;
    
    #@{$self->{group}} = sort( @{$self->{group}} );
    #@{$self->{members}} = sort( @{$self->{members}} );
    @{$self->{date}} = sort( @{$self->{date}} );
    @{$self->{site}} = sort_site( @{$self->{site}} );
    #@{$self->{disc}} = sort( @{$self->{disc}} );
}

sub sort
{
    my @arr = @_;
    my $size = @arr;

    for( my $a = 0; $a < $size - 1; $a++ )
    {
        for( my $b = $a + 1; $b < $size; $b++ )
        {
            if( $arr[$a] gt $arr[$b] )
            {
                my $tmp = $arr[$a];
                $arr[$a] = $arr[$b];
                $arr[$b] = $tmp;
            }
        }
    }

    return @arr;
}

sub sort_site
{
    my @arr = @_;
    my $size = @arr;

    for( my $a = 0; $a < $size - 1; $a++ )
    {
        for( my $b = $a + 1; $b < $size; $b++ )
        {
            if( $sites{$arr[$a]} > $sites{$arr[$b]} )
            {
                my $tmp = $arr[$a];
                $arr[$a] = $arr[$b];
                $arr[$b] = $tmp;
            }
        }
    }
    return @arr;
}

sub showRow
{
    my $self = shift;
    *OUT = shift;

#if( !defined( $self->{group}[0] )){ print( "no", "\n" );}
#print( $self->{dataset}, "\n" );

    # Group string
    my @arr = @{$self->{group}};
    my $size = @arr;
    my $group = $self->{group}[0];
    for( my $x = 1; $x < $size; $x++ )
    {
    #   $group = $group . "<br>" . $self->{group}[$x];  
        $group = $group . "<br><hr noshade width=75%>" . $self->{group}[$x];    
    }

    # Members string
    my @arr = @{$self->{members}};
    my $size = @arr;
    my $members = $self->{members}[0];
    for( my $x = 1; $x < $size; $x++ )
    {
        #$members = $members . "<br>" . $self->{members}[$x];   
        $members = $members . "<br><hr noshade width=75%>" . $self->{members}[$x];  
    }

    # Date string
    my @arr = @{$self->{date}};
    my $size = @arr;
    my $date = $self->{date}[0];
    for( my $x = 1; $x < $size; $x++ )
    {
        $date = $date . "<br>" . $self->{date}[$x]; 
    }

    # Site string
    my @arr = @{$self->{site}};
    my $size = @arr;
    my $site = $self->{site}[0];
    for( my $x = 1; $x < $size; $x++ )
    {
        $site = $site . "<br>" . $self->{site}[$x]; 
    }

    # Site Group string
    my @arr = @{$self->{sitegroup}};
    my $size = @arr;
    my $sitegroup = $self->{sitegroup}[0];
    for( my $x = 1; $x < $size; $x++ )
    {
        $sitegroup = $sitegroup . "<br>" . $self->{sitegroup}[$x]; 
    }

    # Discipline string
    my @arr = @{$self->{disc}};
    my $size = @arr;
    my $disc = $self->{disc}[0];
    for( my $x = 1; $x < $size; $x++ )
    {
        $disc = $disc . "<br><hr noshade width=75%>" . $self->{disc}[$x];   
    }

    println( *OUT, "\t<tr align=left>" );
        println( *OUT, "\t\t<td align=center><font face=\"Verdana, Arial, Helvetica, sans-serif\" size=\"2\">$group</td>" );
        println( *OUT, "\t\t<td align=center><font face=\"Verdana, Arial, Helvetica, sans-serif\" size=\"2\">$members</td>" );
        println( *OUT, "\t\t<td align=center><font face=\"Verdana, Arial, Helvetica, sans-serif\" size=\"2\">$self->{dataset}</td>" );
        println( *OUT, "\t\t<td align=center><font face=\"Verdana, Arial, Helvetica, sans-serif\" size=\"2\">$date</td>" );
        println( *OUT, "\t\t<td align=center><font face=\"Verdana, Arial, Helvetica, sans-serif\" size=\"2\">$site</td>" );
        println( *OUT, "\t\t<td align=center><font face=\"Verdana, Arial, Helvetica, sans-serif\" size=\"2\">$sitegroup</td>" );
        println( *OUT, "\t\t<td align=center><font face=\"Verdana, Arial, Helvetica, sans-serif\" size=\"2\">$disc</td>" );
        println( *OUT, "\t\t<td align=center><font face=\"Verdana, Arial, Helvetica, sans-serif\" size=\"2\">$self->{readme}</td>" );
    println( *OUT, "\t</tr>" );
}
1;


