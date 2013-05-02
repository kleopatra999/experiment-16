#!/usr/bin/perl
use strict;
use warnings;
use FindBin;
use if !$^S, lib => "$FindBin::Bin/.";
use Make_pl;

sub ghc {
    my ($to, $from) = @_;
    rule $to, $from, sub {
        run "ghc \Q$from\E -o \Q$to\E";
    };
}

sub scan_imports {
    use File::Spec::Functions qw(:ALL);
    my ($file) = @_;
    $file =~ /\.hs$/ or return ();
    open my $F, '<', $file or (warn "Could not open $file: $!\n" and return);
    read $F, my $head, 2048;
    close $F;
    my @r;
    for ($head =~ /\bimport\s+(?:qualified)?\s+(Ex16\.[a-zA-Z_.]+)/g) {
        s/\./\//g;
        push @r, "$_.hs"
    }
    return @r;
}

workflow {
    subdep \&scan_imports;

    phony 'test', 'test/parser.exe', sub {
        run "prove -e '' test/*.exe";
    };

    ghc 'test/parser.exe', 'test/parser.hs';
    #ghc 'test/syntax.exe', 'test/syntax.hs';

    rule 'clean', [], sub {
        unlink glob '*.hi *.o *.exe */*.hi */*.o */*.exe';
    };
};
