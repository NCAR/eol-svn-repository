#! /usr/bin/perl -w

# Reformats a bad LeMoore sounding file into one that can be used
# by the regular converter.
use strict;

my $data = {};
my $press = {};

&main();

sub main {
    open(my $FILE,$ARGV[0]) or die("Can't open file: $ARGV[0]\n");
    readSection1($FILE);
    readSection2($FILE);
    readSection3($FILE);
    close($FILE);

    write_data();
}

sub readSection1 {
    my ($FILE) = @_;
    my $line = <$FILE>;
    $line = <$FILE>;
    $line = <$FILE>;
    $line = <$FILE>;
    while ($line !~ /^\s*$/) {
	my @info = split(' ',$line);
	$data->{$info[0]}->{$info[1]}->{"height"} = $info[2];
	$data->{$info[0]}->{$info[1]}->{"press"} = $info[3];
	$data->{$info[0]}->{$info[1]}->{"temp"} = $info[4];
	$data->{$info[0]}->{$info[1]}->{"rh"} = $info[5];
	$data->{$info[0]}->{$info[1]}->{"dewpt"} = $info[6];
	$line = <$FILE>;
    }
}

sub readSection2 {
    my ($FILE) = @_;
    my $line = <$FILE>;
    $line = <$FILE>;
    $line = <$FILE>;
    $line = <$FILE>;
    while ($line !~ /^\s*$/) {
	my @info = split(' ',$line);
	$data->{$info[0]}->{$info[1]}->{"height"} = $info[2];
	$data->{$info[0]}->{$info[1]}->{"press"} = $info[3];
	$data->{$info[0]}->{$info[1]}->{"dir"} = $info[4];
	$data->{$info[0]}->{$info[1]}->{"spd"} = $info[5];
	$line = <$FILE>;
    }
}

sub readSection3 {
    my ($FILE) = @_;
    my $line = <$FILE>;
    $line = <$FILE>;
    $line = <$FILE>;
    $line = <$FILE>;
    while ($line !~ /^\s*$/) {
	my @info = split(' ',$line);
	$press->{$info[0]}->{"height"} = $info[1];
	$press->{$info[0]}->{"temp"} = $info[2];
	$press->{$info[0]}->{"rh"} = $info[3];
	$press->{$info[0]}->{"dewpt"} = $info[4];
	$press->{$info[0]}->{"dir"} = $info[5];
	$press->{$info[0]}->{"spd"} = $info[6];
	$line = <$FILE>;
    }
}

sub write_data {
    my @mandatory = reverse(sort({$a <=> $b} keys(%{ $press})));

    my $mand_index = 0;
    my $last_press = 10000;
    foreach my $min (sort({$a <=> $b} keys(%{$data}))) {
	foreach my $sec (sort({$a <=> $b} keys(%{$data->{$min}}))) {
            if ($mand_index < @mandatory && $mandatory[$mand_index] > $data->{$min}->{$sec}->{"press"}) {
		printf("%4s %2s /////// %7s %8s %6s %3s %6s %4s %5s\n","///","//",$press->{$mandatory[$mand_index]}->{"height"},$mandatory[$mand_index],$press->{$mandatory[$mand_index]}->{"temp"},$press->{$mandatory[$mand_index]}->{"rh"},$press->{$mandatory[$mand_index]}->{"dewpt"},$press->{$mandatory[$mand_index]}->{"dir"},$press->{$mandatory[$mand_index]}->{"spd"});
                $mand_index++;
            }
	    printf("%4s %2s /////// %7s %8s %6s %3s %6s %4s %5s\n",$min,$sec,$data->{$min}->{$sec}->{"height"},$data->{$min}->{$sec}->{"press"},defined($data->{$min}->{$sec}->{"temp"}) ? $data->{$min}->{$sec}->{"temp"} : "//////",defined($data->{$min}->{$sec}->{"rh"}) ? $data->{$min}->{$sec}->{"rh"} : "///",defined($data->{$min}->{$sec}->{"dewpt"}) ? $data->{$min}->{$sec}->{"dewpt"} : "//////",defined($data->{$min}->{$sec}->{"dir"}) ? $data->{$min}->{$sec}->{"dir"} : "////",defined($data->{$min}->{$sec}->{"spd"}) ? $data->{$min}->{$sec}->{"spd"} : "/////");
	    $last_press = $data->{$min}->{$sec}->{"press"};

	}
    }
}
