## Copyright (C) Graham Barr
## vim: ts=8:sw=2:expandtab:shiftround

package MooseX::DBIC::Scaffold::Mapping;

use Moose;

has name     => (is => 'rw');
has accessor => (is => 'rw');
has left     => (is => 'rw');
has right    => (is => 'rw');

1;

