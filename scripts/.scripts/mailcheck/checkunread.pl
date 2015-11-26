#!/usr/bin/perl
# checkunread.pl by i_magnific0
# description: count number of unread messages on imap

use strict;
use Mail::IMAPClient;
use IO::Socket::SSL;

open(AUTHFILE, '<', $ENV{"HOME"} . '/.authinfo');

while (<AUTHFILE>) 
{      
    my @config = split(' ', $_);                              
    my $server = $config[1];
    my $username = $config[3];
    my $password = $config[5];
    print "Authenticating with $username @ $server\n";
    my $socket = IO::Socket::SSL->new(
	PeerAddr => $server,
	PeerPort => 993
	)
	or sortofdie();


    my $client = Mail::IMAPClient->new(
	Socket   => $socket,
	User     => $username,
	Password => $password,
	)
	or sortofdie();

    my $msgct = 'a';
    if ($client->IsAuthenticated()) {
	$client->select("INBOX");
	$msgct = $client->unseen_count||'0';
    }
    print "$msgct ";

    $client->logout();
}

close (OUTFILE);
close (AUTHFILE);

sub sortofdie {
    print OUTFILE "e\n";
    next;
}
