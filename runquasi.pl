#!/usr/bin/perl

use File::Find;
use File::Copy;

my $tmp = "";
my $orig = "";
my $qq = "";

find(\&wanted, '.');

sub wanted {
    if ( /^\.Z[0-9][0-9]*.*thesis\.tex/ ) {
	$orig = $_;
	$tmp = ".old" . $_;
	$qq = ".qq" . $_;
	print "Will's quasiquote magic\n";
	print "Processing ";
	print $_;
	print "\n";
	move($_, $tmp) or die "move failed: $!";
	quasi();
	copy($qq, $orig) or die "copy failed: $!";
    }
    if ( /^\.Z1[0-9].*thesis\.tex/ ) {    
	$orig = $_;
	$tmp = ".old" . $_;
	$qq = ".qq" . $_;
	print "Will's quasiquote magic\n";
	print "Processing ";
	print $_;
	print "\n";
	move($_, $tmp) or die "move failed: $!";
	quasi();
	copy($qq, $orig) or die "copy failed: $!";
    }    
}

sub quasi {
    open(TMP, $tmp);
    open(QQ, ">" . $qq);
    my @stack;
    my $magic_turned_on = 1;
    while ($line = <TMP>) {
	if ($magic_turned_on) {
	    $line =~ s{(\\qqmagicoff)}{
		$magic_turned_on = 0;
		print "*** magic off ***\n";
		$1;
	    }eg;
	    $line =~ s/`,//g;	    
	    $line =~ s/'\\dt/\\dt/g;
	    $line =~ s/'\\cn/\\cn/g;
	    $line =~ s/`\\cn/\\cn/g;
	    $line =~ s/,\\va/\\va/g;
	    $line =~ s/,\\em/\\em/g;
	    $line =~ s/,\$/\$/g;
	    $line =~ s/,\{\\it /\{\\it /g;
	    $line =~ s/`\\=\(/\\=\(/g;
	    $line =~ s/'\\=\(/\\=\(/g;
	    $line =~ s/'\{\\sf/\{\\sf/g;
	    $line =~ s/'\{\\textsf/\{\\textsf/g;	    
	    $line =~ s/,\{\\/\{\\/g;
	    $line =~ s{(ZZZZschemeresponse)}{
		my $result = $&;
		if ($1 or @stack) {
		    @stack ? pop @stack : push @stack, $1;
		    $result = $1;
		}
		$result;
	    }eg;	
	    $line =~ s{(['`])?(\()|(\))|( \. )}{
		my $result = $&;
		if ($1 or @stack) {
		    push @stack, $1 if $2;
		    $result = $2 ? '$\textbf{(}\!\!\hspace*{-1pt}\textbf{(}$'
			: ($3 ? '$\textbf{)}\!\!\hspace*{-1pt}\textbf{)}$'
			   :  ' $\centerdot$ ');
		    pop @stack if $3;
		}
		$result;
	    }eg;
	    print QQ $line;
	} else {
	    $line =~ s{(\\qqmagicon)}{
		$magic_turned_on = 1;
		print "*** magic on ***\n";
		$1;
	    }eg;
	    print QQ $line;
	}
    }
    close(TMP);
    close(QQ);
}
