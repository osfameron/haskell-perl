package Programme;
use Moo;
use Types::Standard qw/ Maybe Str /;

has id => ( is => 'ro', isa => Maybe[Str] );
has id2 => ( is => 'ro', isa => Maybe[Str] );

package Brand;
use Moo;
extends 'Programme';

package Series;
use Moo;
extends 'Programme';

package Episode;
use Moo;
extends 'Programme';

package Database;
use Moo;
use MooX::HandlesVia;
use Types::Standard qw/ ArrayRef HashRef InstanceOf /;

use experimental 'signatures', 'lexical_subs';

has list => (
    is => 'ro',
    isa => ArrayRef[InstanceOf["Programme"]],
    handles_via => 'Array',
    handles => {
        filter => 'grep',
    }
);

has graph => (
    is => 'ro',
    isa => HashRef[InstanceOf["Programme"]],
    handles_via => 'Hash',
    handles => {
        lookup => 'get'
    }
);

sub search ($self, $prog1) {
    my sub match ($prog2) {
        return unless ref $prog1 eq ref $prog2;

        return unless $prog1->id or $prog1->id2;
        return unless $prog2->id or $prog2->id2;
        
        return unless $prog1->id or $prog2->id2;
        return unless $prog1->id2 or $prog2->id;

        return if $prog1->id and $prog2->id 
            and $prog1->id ne $prog2->id;
        return if $prog1->id2 and $prog2->id2 
            and $prog1->id2 ne $prog2->id2;

        return 1;
    }

    return $self->filter(\&match);
}

sub getHierarchy($self, $prog) {
    if (my $parent = $self->lookup($prog->id)) {
        return ($prog, $self->getHierarchy($parent))
    }
    else {
        return $prog;
    }

}

package main;
use Data::Dumper::Concise;

my $b1 = Brand->new( id => "bbc" );
my $s1 = Series->new( id => "hignfy");
my $e1 = Episode->new( id => "hignfy1");

my $db = Database->new(
    list => [$b1, $s1, $e1],
    graph => {
        $e1->id => $s1,
        $s1->id => $b1,
    }
);

print Dumper(
    $db->getHierarchy(
        $db->search(
            Episode->new( id => 'hignfy1' )
        )
    )
);
