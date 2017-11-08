use strict;
use warnings FATAL => 'all';

package Java::ClassFile::Parser;
use Bit::Vector qw//;
use Carp qw/croak/;
use Class::Tiny qw/from_memory from_file/;
use Config;
use IO::File;
use Math::BigFloat;

my @attributes = keys %{Class::Tiny->get_all_attribute_defaults_for(__PACKAGE__)};

my @bitsForFloatCmp =
  (
   Bit::Vector->new_Hex( 32, '7f800000' ),
   Bit::Vector->new_Hex( 32, 'ff800000' ),
   Bit::Vector->new_Hex( 32, '7f800001' ),
   Bit::Vector->new_Hex( 32, '7fffffff' ),
   Bit::Vector->new_Hex( 32, 'ff800001' ),
   Bit::Vector->new_Hex( 32, 'ffffffff' )
  );
my @bitsForFloatMantissa =
  (
   Bit::Vector->new_Hex( 32, 'ff'     ),
   Bit::Vector->new_Hex( 32, '7fffff' ),
   Bit::Vector->new_Hex( 32, '800000' )
  );
my @mathForFloat =
  (
   Math::BigFloat->new('150'),
   Math::BigFloat->new('2'),
  );

my @bitsForDoubleCmp = (
                       Bit::Vector->new_Hex( 64, "7ff0000000000000" ),
                       Bit::Vector->new_Hex( 64, "fff0000000000000" ),
                       Bit::Vector->new_Hex( 64, "7ff0000000000001" ),
                       Bit::Vector->new_Hex( 64, "7fffffffffffffff" ),
                       Bit::Vector->new_Hex( 64, "fff0000000000001" ),
                       Bit::Vector->new_Hex( 64, "ffffffffffffffff" )
                      );
my @bitsForDoubleMantissa =
  (
   Bit::Vector->new_Hex( 64, '7ff'           ),
   Bit::Vector->new_Hex( 64, 'fffffffffffff' ),
   Bit::Vector->new_Hex( 64, '10000000000000' )
  );
my @mathForDouble =
  (
   Math::BigFloat->new('1075'),
   Math::BigFloat->new('2'),
  );

sub BUILD {
    my ($self, $args) = @_;
    #
    # Check arguments
    #
    my $args_defined_count = grep { defined } map { $args->{$_} } @attributes;
    croak sprintf("Please specificy the origin using one of: " . join(', ', @attributes)) unless $args_defined_count;
    croak join(', ', @attributes) . " are exclusive" unless $args_defined_count == 1;
    #
    # Get data in memory if needed
    #
    my $from_file = $args->{from_file};
    if (defined($from_file)) {
	my $fh = IO::File->new($from_file, 'r') || croak "Failed to open $from_file, $!";
	$fh->binmode() || croak "Failed to set binary mode on $from_file, $!";
	$self->from_memory(do { local $/; <$fh> });
	$fh->close() || warn "Failed to close $from_file, $!";
    }
}

sub _bytesToVector {
    # Parameters are the perl value and the number of bytes wanted
    # You must say number of bytes + 1 if you want to be sure to have
    # an unsigned value -;
    my $vector = Bit::Vector->new($_[1]);
    #
    # Last statement is $vector at the end -;
    #
    $vector->Chunk_List_Store(8, reverse unpack('C*', $_[0])), $vector
}

sub _u1 {
    use bytes;
    croak "Not enough data to read a byte" unless length($_[0]);
    return unpack('C', substr($_[0], 0, 1, ''))
}

sub _signedu1 {
    use bytes;
    croak "Not enough data to read a byte" unless length($_[0]);
    return unpack('c', substr($_[0], 0, 1, ''))
}

sub _u2 {
    use bytes;
    croak "Not enough data to read two bytes" unless length($_[0]) >= 2;
    return unpack('n', substr($_[0], 0, 2, ''))
}

sub _signedu2 {
    use bytes;
    croak "Not enough data to read two bytes" unless length($_[0]) >= 2;
    return _bytesToVector(substr($_[0], 0, 2, ''), 16)->to_Dec     # Bit::Vector defaults to signed values, so 16 bits is ok
}

#
# For four bytes quantities, let's quote perldoc -f pack:
#
# q  A signed quad (64-bit) value.
#  Q  An unsigned quad value.
#     (Quads are available only if your system supports 64-bit
#      integer values _and_ if Perl has been compiled to support
#      those.  Raises an exception otherwise.)
#
# The Bit::Vector implementation below is working with any configuration
#

sub _u4 {
    use bytes;
    croak "Not enough data to read four bytes" unless length($_[0]) >= 4;
    return _bytesToVector(substr($_[0], 0, 4, ''), 65)->to_Dec	# Use 65 bits to be sure we have an unsigned value
}

sub _signedu4 {
    use bytes;
    croak "Not enough data to read four bytes" unless length($_[0]) >= 4;
    return _bytesToVector(substr($_[0], 0, 4, ''), 64)->to_Dec     # Bit::Vector defaults to signed values, so 64 bits is ok
}

sub float {
    my $u4 = _u4($_[0]);
    my $vector = _bytesToVector($u4, 32);

    if ($vector->equal($bitsForFloatCmp[0])) {
	return FLOAT_POSITIVE_INF->copy
    }
    if ($vector->equal( $bitsForFloatCmp[1])) {
	return FLOAT_NEGATIVE_INF->copy
    }
    if (
	(
	 $vector->Lexicompare( $bitsForFloatCmp[2] ) >= 0  &&
	 $vector->Lexicompare( $bitsForFloatCmp[3] ) <= 0
	)
	||
	(
	 $vector->Lexicompare( $bitsForFloatCmp[4] ) >= 0 &&
	 $vector->Lexicompare( $bitsForFloatCmp[5] ) <= 0
	)
        ) {
	return FLOAT_NAN->copy
    }

    #
    # int s = ((bits >> 31) == 0) ? 1 : -1;
    #
    my $s = $vector->Clone();
    $s->Move_Right(31);
    my $sf = ($s->to_Dec() == 0) ? FLOAT_POSITIVE_ONE->copy() : FLOAT_NEGATIVE_ONE->copy();
    #
    # int e = ((bits >> 23) & 0xff);
    #
    my $e = $vector->Clone();
    $e->Move_Right(23);
    $e->And( $e, $bitsForFloatMantissa[0] );
    #
    # int m = (e == 0) ? (bits & 0x7fffff) << 1 : (bits & 0x7fffff) | 0x800000;
    #                     ^^^^^^^^^^^^^^^^^^^^^    ^^^^^^^^^^^^^^^^^^^^^^^^^
    #                                       \       /
    #                                        \     /
    #                                      same things
    #
    my $m = $vector->Clone();
    $m->And( $m, $bitsForFloatMantissa[1] );
    if ( $e->to_Dec() == 0 ) {
	$m->Move_Left(1)
    } else {
	$m->Or( $m, $bitsForFloatMantissa[2] )
    }
    #
    # $value = $s * $m * (2 ** ($e - 150))
    # Note: Bit::Vector->to_Dec() returns a string
    my $mf = Math::BigFloat->new($m->to_Dec());
    my $ef = Math::BigFloat->new($e->to_Dec());
    
    $ef->bsub($mathForFloat[0]);              # $e - 150
    my $mantissaf = $mathForFloat[1]->copy(); # 2
    $mantissaf->bpow($ef);                    # 2 ** ($e - 150)
    $mf->bmul($mantissaf);                    # $m * (2 ** ($e - 150))
    $mf->bmul($sf);                           # $s * $m * (2 ** ($e - 150))
    return $mf
}

sub long {
  my $vhigh = _u4($_[0]);
  my $vlow  = _u4($_[0]);
  #
  # ((long) high_bytes << 32) + low_bytes
  #
  return Bit::Vector->Concat_List($vhigh, $vlow)->to_Dec()
}

sub double {
    my $vhigh = _u4($_[0]);
    my $vlow  = _u4($_[0]);
    #
    # ((long) high_bytes << 32) + low_bytes
    #
    my $vector = Bit::Vector->Concat_List($vhigh, $vlow);
    #
    # Same technique as in float
    #
    if ($vector->equal($bitsForDoubleCmp[0])) {
	return FLOAT_POSITIVE_INF->copy
    }
    if ($vector->equal( $bitsForDoubleCmp[1])) {
	return FLOAT_NEGATIVE_INF->copy
    }
    if (
	(
	 $vector->Lexicompare( $bitsForDoubleCmp[2] ) >= 0  &&
	 $vector->Lexicompare( $bitsForDoubleCmp[3] ) <= 0
	)
	||
	(
	 $vector->Lexicompare( $bitsForDoubleCmp[4] ) >= 0 &&
	 $vector->Lexicompare( $bitsForDoubleCmp[5] ) <= 0
	)
        ) {
	return FLOAT_NAN->copy
    }

    #
    # int s = ((bits >> 63) == 0) ? 1 : -1;
    #
    my $s = $vector->Clone();
    $s->Move_Right(63);
    my $sf = ($s->to_Dec() == 0) ? FLOAT_POSITIVE_ONE->copy() : FLOAT_NEGATIVE_ONE->copy();
    #
    # int e = (int)((bits >> 52) & 0x7ffL);
    #
    my $e = $vector->Clone();
    $e->Move_Right(52);
    $e->And( $e, $bitsForDoubleMantissa[0] );
    #
    # long m = (e == 0) ? (bits & 0xfffffffffffffL) << 1 : (bits & 0xfffffffffffffL) | 0x10000000000000L;
    #                     ^^^^^^^^^^^^^^^^^^^^^^^^^        ^^^^^^^^^^^^^^^^^^^^^^^^^
    #                                             \       /
    #                                              \     /
    #                                            same things
    my $m = $vector->Clone();
    $m->And( $m, $bitsForDoubleMantissa[1] );
    if ( $e->to_Dec() == 0 ) {
	$m->Move_Left(1)
    } else {
	$m->Or( $m, $bitsForDoubleMantissa[2] )
    }
    #
    # $value = $s * $m * (2 ** ($e - 1075))
    #
    my $mf = Math::BigFloat->new($m->to_Dec());
    my $ef = Math::BigFloat->new($e->to_Dec());
    
    $ef->bsub($mathForDouble[0]);              # $e - 1075
    my $mantissaf = $mathForDouble[1]->copy(); # 2
    $mantissaf->bpow($ef);                     # 2 ** ($e - 150)
    $mf->bmul($mantissaf);                     # $m * (2 ** ($e - 150))
    $mf->bmul($sf);                            # $s * $m * (2 ** ($e - 150))
    return $mf
}

sub utf8 {
    #
    # Disable all conversion warnings:
    # either we know we succeed, either we abort -;
    #
    no warnings;

    my $s = undef;
    return $s unless length($_[0]);

    my @bytes = unpack('C*', $_[0]);
    my ($val0, $val1, $val2, $val3, $val4, $val5) = '';

    while (@bytes) {
	#
	# This is to avoid a internal op with ';' : @bytes is guaranteed to be shifted
	#
	if ((($val0 = shift(@bytes)) & 0x80) == 0) {              # 0x80 == 10000000                   => 0xxxxxxx
	    #
	    # 1 byte
	    #
	    $s .= chr($val0)
	}
	elsif ((($val0 & 0xE0) == 0xC0) &&                        # 0xE0 == 11100000, 0xC0 == 11000000 => 110xxxxx
	       ($#bytes >= 0) &&
	       ((($val1 = $bytes[0]) & 0xC0) == 0x80)) {          # 0xC0 == 11000000, 0x80 == 10000000 => 10xxxxxx
	    #
	    # 2 bytes
	    #
	    shift(@bytes), $s .= chr((($val0 & 0x1F) << 6) + ($val1 & 0x3F))
	}
	elsif (($val0 == 0xED) &&                                 # 0xED == 11101101                   => 11101101
	       ($#bytes >= 4) &&
	       ((($val1 = $bytes[0]) & 0xF0) == 0xA0) &&          # 0xF0 == 11110000, 0xA0 == 10100000 => 1010xxxx
	       ((($val2 = $bytes[1]) & 0xC0) == 0x80) &&          # 0xC0 == 11000000, 0x80 == 10000000 => 10xxxxxx
	       ( ($val3 = $bytes[2])         == 0xED) &&          # 0xED == 11101101                   => 11101101
	       ((($val4 = $bytes[3]) & 0xF0) == 0xB0) &&          # 0xF0 == 11110000, 0xB0 == 10110000 => 1011xxxx
	       ((($val5 = $bytes[4]) & 0xC0) == 0x80)) {          # 0xC0 == 11000000, 0x80 == 10000000 => 10xxxxxx
	    #
	    # 6 bytes, for supplementary characters are tested BEFORE 3 bytes, because it is a doubled 3-bytes encoding
	    #
	    splice(@bytes, 0, 5), $s .= chr(0x10000 + (($val1 & 0x0F) << 16) + (($val2 & 0x3F) << 10) + (($val4 & 0x0F) <<  6) + ($val5 & 0x3F))
	}
	elsif ((($val0 & 0xF0) == 0xE0) &&                      # 0xF0 == 11110000, 0xE0 == 11100000   => 1110xxxx
	       ($#bytes >= 1) &&
	       ((($val1 = $bytes[0]) & 0xC0) == 0x80) &&        # 0xC0 == 11000000, 0x80 == 10000000   => 10xxxxxx
	       ((($val2 = $bytes[1]) & 0xC0) == 0x80)) {        # 0xC0 == 11000000, 0x80 == 10000000   => 10xxxxxx
	    #
	    # 3 bytes
	    #
	    splice(@bytes, 0, 2), $s .= chr((($val0 & 0xF ) << 12) + (($val1 & 0x3F) << 6) + ($val2 & 0x3F))
	}
	else {
	    croak sprintf('Unable to map byte with value 0x%x', $val0)
	}
    }

    return $s
}

sub parse {
    my ($self) = @_;

    return _ClassFile($self->from_memory)
}

sub _ClassFile {
    return bless {
	magic         => _u4($_[0]),
	minor_version => _u2($_[0]),
	major_version => _u2($_[0])
    }, 'ClassFile'
}

1;
