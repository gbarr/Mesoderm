## Copyright (C) Graham Barr
## vim: ts=8:sw=2:expandtab:shiftround
## ABSTRACT: Schema class scaffold generator for DBIx::Class

package MooseX::DBIC::Scaffold;
use Moose;

use Lingua::EN::Inflect::Number qw(to_S to_PL);
use Data::Dumper;
use MooseX::DBIC::Scaffold::Component;
use MooseX::DBIC::Scaffold::Relationship;
use MooseX::DBIC::Scaffold::Mapping;


has 'schema' => (
  is       => 'ro',
  isa      => 'SQL::Translator::Object::Schema',
  required => 1,
);

has 'schema_class' => (
  is       => 'ro',
  isa      => 'Str',
  required => 1,
);

has 'result_class_namespace' => (
  is      => 'ro',
  isa     => 'Str',
  lazy    => 1,
  default => sub { shift->schema_class },
);

has 'resultset_class_namespace' => (
  is      => 'ro',
  isa     => 'Str',
  lazy    => 1,
  default => sub { shift->result_class_namespace . "::ResultSet" },
);

has 'result_role_namespace' => (
  is      => 'ro',
  isa     => 'Str',
  lazy    => 1,
  default => sub { shift->result_class_namespace . "::Role" },
);

has 'resultset_role_namespace' => (
  is      => 'ro',
  isa     => 'Str',
  lazy    => 1,
  default => sub { shift->resultset_class_namespace . "::Role" },
);

has '_role_map' => (
  traits     => ['Hash'],
  isa        => 'HashRef',
  handles    => {have_role => 'exists',},
  lazy_build => 1,
);

sub _build__role_map {
  my $self = shift;
  require Module::Pluggable::Object;
  my $finder = Module::Pluggable::Object->new(
    search_path => [    ##
      $self->result_role_namespace,
      $self->resultset_role_namespace
    ]
  );
  my %have_role = map { ($_, 1) } $finder->plugins;
  return \%have_role;
}

sub table_components {
  my ($self, $table) = @_;
  my @comp = qw(Core);
  my ($pk) = grep { $_->type eq 'PRIMARY KEY' } $table->get_indices;
  if ($pk ||= $table->primary_key) {
    push @comp, 'PK::Auto' if grep { $_->is_auto_increment } $pk->get_columns;
  }
  else {
    foreach my $k ($table->get_constraints) {
      warn $k->dump(1);
    }
  }
  push @comp, 'InflateColumn::DateTime'
    if grep { $_->data_type =~ /^(DATE|DATETIME|TIMESTAMP)$/i } $table->get_columns;
  return @comp;
}

sub column_components { my ($self, $column) = @_; return; }    # TODO

sub table_roles {
  my ($self, $table) = @_;
  grep { $self->have_role($_) }
    $self->result_role_namespace . "::" . $self->table_class_element($table);
}

sub resultset_roles {
  my ($self, $table) = @_;
  grep { $self->have_role($_) }
    $self->resultset_role_namespace . "::" . $self->table_class_element($table);
}

sub column_info {
  my ($self, $column) = @_;
  my %info;

  $info{data_type}         = $column->data_type;
  $info{default_value}     = $column->default_value;
  $info{is_nullable}       = $column->is_nullable ? 1 : 0;
  $info{is_auto_increment} = 1 if $column->is_auto_increment;

  if ($column->has_precision) {
    $info{size} = [int $column->length, int $column->precision];
  }
  else {
    $info{size} = int $column->length;
  }

  my %extra = $column->extra;
  delete $extra{ignore};
  $info{extra} = \%extra if keys %extra;

  return \%info;
}

has 'insert_defaults' => (
  is         => 'ro',
  isa        => 'HashRef[Str]',
  lazy_build => 1,
);
sub _build_insert_defaults { return {} }

sub insert_default {
  my ($self, $column) = @_;
  my $name     = $column->name;
  my $defaults = $self->insert_defaults;
  my $value    = $defaults->{$name};
  return $value if defined $value;
  $name  = $column->table->name . ".$name";
  $value = $defaults->{$name};
  return $value if defined $value;
  $name = $column->table->schema->name . ".$name";
  return $defaults->{$name};
}

sub ignore_table        { return 0 }
sub ignore_column       { return 0 }
sub ignore_index        { return 0 }
sub ignore_constraint   { return 0 }
sub ignore_relationship { return 0 }

sub _ignore_table {
  my ($self, $table) = @_;
  my $ignore = $table->get_extra('ignore');
  return $ignore if defined $ignore;
  $ignore = $self->ignore_table($table);
  $table->add_extra(ignore => $ignore || 0);
  return $ignore;
}

sub _ignore_column {
  my ($self, $column) = @_;
  my $ignore = $column->get_extra('ignore');
  return $ignore if defined $ignore;
  $ignore ||= $self->_ignore_table($column->table);
  $ignore ||= $self->ignore_column($column);
  $column->add_extra(ignore => $ignore || 0);
  return $ignore;
}

sub _ignore_index {
  my ($self, $index) = @_;
  my $ignore = $index->get_extra('ignore');
  return $ignore if defined $ignore;
  $ignore ||= $self->_ignore_table($index->table);
  $ignore ||= grep { $self->_ignore_column($_) } $index->get_columns;
  $ignore ||= $self->ignore_index($index);
  $index->add_extra(ignore => $ignore || 0);
  return $ignore;
}

sub _ignore_constraint {
  my ($self, $constraint) = @_;
  my $ignore = $constraint->get_extra('ignore');
  return $ignore if defined $ignore;
  $ignore ||= $self->_ignore_table($constraint->table);
  $ignore ||= grep { $self->_ignore_column($_) } $constraint->get_columns;
  unless ($ignore) {
    my $table = $constraint->table->schema->get_table($constraint->reference_table);
    $ignore ||= !$table || $self->_ignore_table($table);
    $ignore ||= grep {
      my $c = $table->get_column($_);
      !$c || $self->_ignore_column($c)
    } $constraint->reference_columns;
  }
  $ignore ||= $self->ignore_constraint($constraint);
  $constraint->add_extra(ignore => $ignore || 0);
  return $ignore;
}

sub mapping_accessor {
  my ($self, $mapping) = @_;
  my $method =
    ($mapping->left->type eq 'has_many' or $mapping->right->type eq 'has_many')
    ? 'to_plural'
    : 'to_singular';
  $self->$method(lc $mapping->right->foreign_table->name);
}

sub relationship_accessor {
  my ($self, $relationship) = @_;
  my $method = $relationship->type . "_accessor";
  $self->$method($relationship);
}

sub belongs_to_accessor {
  my ($self, $relationship) = @_;
  $self->to_singular(lc $relationship->foreign_table->name);
}

sub might_have_accessor {
  my ($self, $relationship) = @_;
  $self->to_singular(lc $relationship->foreign_table->name);
}

sub has_one_accessor {
  my ($self, $relationship) = @_;
  $self->to_singular(lc $relationship->foreign_table->name);
}

sub has_many_accessor {
  my ($self, $relationship) = @_;
  $self->to_plural(lc $relationship->foreign_table->name);
}

sub result_class {
  my ($self, $table) = @_;
  $self->result_class_namespace . "::" . $self->table_class_element($table);
}

sub resultset_class {
    my ($self, $table) = @_;
    $self->resultset_class_namespace . "::" . $self->table_class_element($table);
}

sub table_moniker {
    my ($self, $table) = @_;
    $self->to_singular(lc $table->name);
}

sub table_class_element {
  my ($self, $table) = @_;
  my $class = lc $self->table_moniker($table);
  $class =~ s/(^|_)(.)/\U$2/g;
  $class =~ s/[^\w:]/_/g;

  $class;

}

sub to_singular {
  my ($self, $what) = @_;
  to_S($what);
}

sub to_plural {
  my ($self, $what) = @_;
  to_PL($what);
}

sub column_accessor { my ($self, $column) = @_; return lc($column->name) }

sub reciprocate_relationship {
  my ($self, $r) = @_;
  $r->reciprocal;
}

sub is_mapping_table {
  my ($self, $t) = @_;
  my $rel = $t->get_extra('relationships') or return 0;
  ## FIXME, need better checks to determine a mapping table
  return 0 unless @$rel == 2;
  return 1;
}

sub produce {
  my ($self, $fh) = @_;
  my $schema = $self->schema;

  foreach my $t ($schema->get_tables) {
    if ($self->_ignore_table($t)) {
      $schema->remove_table($t->name);
      next;
    }
    foreach my $c ($t->get_columns) {
      $t->remove_column($c->name) if $self->_ignore_column($c);
    }
  }

  foreach my $t ($schema->get_tables) {
    foreach my $i ($t->get_indices) {
      $t->remove_index($i->name) if $self->_ignore_index($i);
    }

    foreach my $c ($t->get_constraints) {
      next if $c->type ne 'FOREIGN KEY' or $self->_ignore_constraint($c);

      if (my $r1 = $self->build_relationship($c)) {
        unless ($self->ignore_relationship($r1)) {
          my $rel1 = $r1->table->get_extra('relationships');
          $r1->table->add_extra(relationships => $rel1 = []) unless $rel1;
          push @$rel1, $r1;
        }
        my $r2 = $self->reciprocate_relationship($r1);
        if ($r2 and !$self->ignore_relationship($r2)) {
          my $rel2 = $r2->table->get_extra('relationships');
          $r2->table->add_extra(relationships => $rel2 = []) unless $rel2;
          push @$rel2, $r2;

        }
      }
    }
  }

  foreach my $t ($schema->get_tables) {
    if ($self->is_mapping_table($t)) {
      my $rel = $t->get_extra('relationships');
      next unless $rel and @$rel == 2;
      my ($left, $right) = @$rel;
      for my $loop (1, 2) {
        my $lr  = $left->reciprocal;
        my $m   = $self->build_mapping($lr, $right);
        my $map = $lr->table->get_extra('mappings');
        $lr->table->add_extra(mappings => $map = []) unless $map;
        push @$map, $m;

        ($left, $right) = ($right, $left);
      }
    }
  }

  # Build accessor names
  foreach my $t ($schema->get_tables) {
    my $rel = $t->get_extra('relationships') or next;
    $_->accessor($self->relationship_accessor($_)) for @$rel;
  }

  foreach my $t ($schema->get_tables) {
    my $map = $t->get_extra('mappings') or next;
    $_->accessor($self->mapping_accessor($_)) for @$map;
  }

  $self->write(output => $fh);
}

sub build_relationship {
  my ($self, $c) = @_;
  my $table     = $c->table;
  my @columns   = $c->get_columns;
  my $f_table   = $table->schema->get_table($c->reference_table);
  my @f_columns = map { $f_table->get_column($_) } $c->reference_columns;

  my $r = MooseX::DBIC::Scaffold::Relationship->new(
    name            => $c->name,
    table           => $table,
    columns         => \@columns,
    foreign_table   => $f_table,
    foreign_columns => \@f_columns,
  );
}

sub build_mapping {
  my ($self, $left, $right) = @_;

  my $r = MooseX::DBIC::Scaffold::Mapping->new(
    name  => $left->name . "__" . $right->name,
    left  => $left,
    right => $right,
  );
  return $r;
}

sub write {
  my ($self, %opt) = @_;

  my $out_fh = $opt{output};

  $self->write_preamble($out_fh);
  foreach my $table (sort { $a->name cmp $b->name } $self->schema->get_tables) {
    $self->write_table($out_fh, $table);
  }
  print $out_fh "\n1;\n";
}

sub write_tidy {
  my ($self, %opt) = @_;

  my $buffer;
  open(my $fh, ">", \$buffer);
  $self->write(%opt, output => $fh);
  close($fh);

  require Perl::Tidy;

  Getopt::Long::Configure('default');
  Perl::Tidy::perltidy(
    source      => \$buffer,
    destination => $opt{output},
    argv        => [qw(-npro -l 120)],
  );
}


sub _dump_data {
  local $Data::Dumper::Indent    = 1;
  local $Data::Dumper::Quotekeys = 0;
  local $Data::Dumper::Sortkeys  = 1;
  my $data = Data::Dumper::Dumper(shift);
  if (my $indent = shift) {
    $data =~ s/^/$indent/mg;
  }
  $data =~ s/;\s*\Z//;
  $data =~ s/^[^=]*=\s*//;
  return $data;
}

sub write_table {
  my ($self, $fh, $table) = @_;

  my $moniker         = $self->table_moniker($table);
  my $result_class    = $self->result_class($table);
  my $resultset_class = $self->resultset_class($table);
  my $dbic_class      = $self->schema_class . "::_dbic";
  my %insert_default;

  print $fh "##\n## Table: ", $table->name, "\n##\n\n";
  print $fh "has '$moniker' => (\n";
  print $fh "  is      => 'ro',\n";
  print $fh "  lazy    => 1,\n";
  print $fh "  isa     => '$resultset_class',\n";
  print $fh "  default => sub { shift->dbic->resultset('$moniker'); },\n";
  print $fh ");\n\n";

  print $fh "{\n";
  print $fh "  package $result_class;\n";
  print $fh "  use Moose;\n";
  print $fh "  extends 'DBIx::Class';\n";

  my $comp_init = $self->write_components($fh, $table);

  print $fh "  with '$_';\n" for $self->table_roles($table);
  print $fh "  no Moose;\n\n";

  print $fh "  __PACKAGE__->table('", $table->name, "');\n";
  print $fh "  __PACKAGE__->add_columns(\n";
  foreach my $column ($table->get_columns) {
    my $data = _dump_data($self->column_info($column), "    ");
    if (defined(my $default = $self->insert_default($column))) {
      $insert_default{$column->name} = $default;
    }
    print $fh "    ${ \($column->name) } => $data,\n";
  }
  print $fh "  );\n";

  print $fh $comp_init;

  my ($pk) = grep { $_->type eq 'PRIMARY KEY' } $table->get_indices;
  if ($pk ||= $table->primary_key) {
    my @pk_cols = map { $_->name } $pk->get_columns;
    print $fh "  __PACKAGE__->set_primary_key(qw/ @pk_cols /);\n";
  }

  foreach my $constraint (sort { $a->name cmp $b->name } $table->get_indices) {
    next if $constraint->name eq 'PRIMARY' or $constraint->type ne 'UNIQUE';
    my @cols = map { $_->name } $constraint->get_columns;
    my $accessor = $constraint->name;

    print $fh "  __PACKAGE__->add_unique_constraint( $accessor => [qw/ @cols /]);\n";
  }

  my @rel = @{$table->get_extra('relationships') || []};
  foreach my $rel (sort { $a->accessor cmp $b->accessor } @rel) {
    my $foreign_class   = $self->result_class($rel->foreign_table);
    my $type            = $rel->type;
    my $accessor        = $rel->accessor;
    my @foreign_columns = $rel->foreign_columns;
    my %column_map      = map { ("foreign." . shift @foreign_columns, "self.$_") } $rel->columns;
    my $column_map      = _dump_data(\%column_map, "  ");
    my $attr            = $rel->has_no_attrs ? "" : (",\n" . _dump_data($rel->attrs, "  "));
    print $fh "  __PACKAGE__->$type( $accessor => '$foreign_class',";
    print $fh $column_map, $attr, ");\n";
  }

  my @map = @{$table->get_extra('mappings') || []};
  foreach my $map (sort { $a->accessor cmp $b->accessor } @map) {
    printf $fh "  __PACKAGE__->many_to_many( '%s' => qw[ %s %s ]);\n",
      $map->accessor, $map->left->accessor, $map->right->accessor;
  }

  if (keys %insert_default) {
    print $fh "  sub insert {\n";
    print $fh "    my \$self = shift;\n";
    foreach my $name (sort keys %insert_default) {
      print $fh "    \$self->set_column( $name => scalar do { $insert_default{$name} } )\n";
      print $fh "      unless \$self->has_column_loaded('$name');\n";
    }
    print $fh "     \$self->next::method(\@_);\n";
    print $fh "  }\n";
  }

  print $fh "  { package $resultset_class;\n";
  print $fh "    use Moose;\n";
  print $fh "    extends 'DBIx::Class::ResultSet';\n";
  print $fh "    with '$_';\n" for $self->resultset_roles($table);
  print $fh "    no Moose;\n";
  print $fh "  }\n";
  print $fh "  __PACKAGE__->resultset_class('$resultset_class');\n";
  print $fh "  $dbic_class->register_class( $moniker => __PACKAGE__ );\n";
  print $fh "}\n\n";
}


sub write_preamble {
  my ($self, $fh) = @_;

  my $dbic_class = $self->schema_class . "::_dbic";
  print $fh "#\n";
  print $fh "# *** DO NOT EDIT THIS FILE ***\n";
  printf $fh "# Generated on %s UTC, by\n", scalar gmtime();
  local ($MooseX::DBIC::Scaffold::VERSION) = (0) unless defined $MooseX::DBIC::Scaffold::VERSION;
  foreach my $class ($self->meta->linearized_isa) {
    my $version = $class->VERSION;
    $version = 'undef' unless defined $version;
    printf $fh "#   %s (%s)\n", $class, $version;
  }
  print $fh "#\n\n";

  print $fh "package $dbic_class;\n";
  print $fh "use base qw(DBIx::Class::Schema);\n\n";
  print $fh "package ", $self->schema_class, "::_scaffold;\n";
  print $fh "use Moose::Role;\n";
  print $fh "requires 'connect_args';\n\n";

  print $fh "has 'dbic' => (\n";
  print $fh "  is => 'ro',\n";
  print $fh "  isa => '$dbic_class',\n";
  print $fh "  lazy => 1,\n";
  print $fh "  default => sub {\n";
  print $fh "    my \$self = shift;\n";
  print $fh "    $dbic_class->connect( \$self->connect_args );\n";
  print $fh "  },\n";
  print $fh ");\n\n";

}

sub write_components {
  my ($self, $fh, $table) = @_;
  my %comp;
  foreach my $comp ($self->table_components($table)) {
    $comp{$comp} = [];
  }
  foreach my $column ($table->get_columns) {
    foreach my $comp ($self->column_components($column)) {
      push @{$comp{$comp}}, $column->name;
    }
  }
  return '' unless keys %comp;

  my @comp =
    sort { $a->order <=> $b->order or $a->name cmp $b->name }
    map { MooseX::DBIC::Scaffold::Component->find($_) }
    keys %comp;

  my $list = join " ", map { $_->name } @comp;
  print $fh "\n";
  print $fh "  __PACKAGE__->load_components(qw/ $list /);\n\n";

  my $init_comp = "";

  foreach my $comp (@comp) {
    my $init = $comp->initializer or next;
    my @columns = sort @{$comp{$comp->name}};
    my $columns =
      @columns
      ? join(" ", "(qw<", @columns, ">)")
      : '';
    $init_comp .= "  __PACKAGE__->$init$columns;\n";
  }
  $init_comp;
}

1;
__END__

=head1 NAME

MooseX::DBIC::Scaffold - Schema class scaffold generator for DBIx::Class

=head1 SYNOPSIS

=head1 DESCRIPTION

=head2 Package Hierarchy

Given a C<schema_class> name of C<Schema> and a schema containing a single table C<foo> the
following packages would be created or search for

=over

=item Schema

Top level schema class. The user needs to provide this themselves. See L</Example Schema Class>

=item Schema::_scaffold

The main generated package that will be a L<Moose::Role> to be consumed into the top level schema
class. See L</The _scaffold Role>

=item Schema::_dbic

A subclass of L<DBIx::Class> that will be used to register the generated classes.

=item Schema::Foo

=item Schema::Role::Foo

=item Schema::ResultSet::Foo

=item Schema::ResultSet::Role::Foo

=back

=head2 The _scaffold Role

=head2 Example Schema Class

The minimum requirement for a schema class is that it providers a method C<connect_args>. The
result of calling this method will be passed to the connect method of L<DBIx::Class>

  package Schema;
  use Moose;
  with 'Schema::_scaffold';

  sub connect_args { 
      return @args_for_dbix_class_connect;
  }

  1;

Some other useful additions

  # delegate txn_* methods to the DBIx::Class object itself
  has '+dbic' => (handles => [qw(txn_do txn_scope_guard txn_begin txn_commit txn_rollback)]);

  # Fetch a DBI handle
  sub dbh {
    shift->dbic->storage->dbh;
  }

=head1 ATTRIBUTES

=over

=item schema

Required.
A L<SQL::Translator::Object::Schema> object that the scaffolding will be generated from

=item schema_class

Required.
Package name that the scaffold will be generated for. The actual package created will be a
L<Moose::Role> with the named C<schema_class> plus C<::_scaffold>

=item result_class_namespace

Optional.
Namespace used by default to prefix package names generated for L<DBIx::Class> result classes.
Defaults to C<schema_class>

=item resultset_class_namespace

Optional.
Namespace used by default to prefix package names generated for L<DBIx::Class> result set classes.
Defaults to C<result_class_namespace> plus C<::ResultSet>

=item result_role_namespace

Optional.
Namespace that will be searched for, during scaffolding, for roles to add to result classes. The
generated code will include C<with> statements for any role that is found during scaffolding.
Defaults to C<result_class_namespace> plus C<::Role>

=item resultset_role_namespace

Optional.
Namespace that will be searched for, during scaffolding, for roles to add to result set classes.
The generated code will include C<with> statements for any role that is found during scaffolding.
Defaults to C<resultset_class_namespace> plus C<::Role>

=back

=head1 METHODS

=over

=back

=head1 SEE ALSO

L<DBIx::Class>,
L<Moose>,
L<SQL::Translator|http://github.com/arcanez/SQL-Translator>

At time of writing the version required is not available on CPAN and needs
to be fetched from github. L<http://github.com/arcanez/SQL-Translator>

=head1 AUTHOR

Graham Barr C<< <gbarr@cpan.org> >>

=head1 COPYRIGHT AND LICENSE

This software is copyright (c) 2010 by Graham Barr.

This is free software; you can redistribute it and/or modify it under
the same terms as the Perl 5 programming language system itself.

=cut

