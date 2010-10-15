package inc::MakeMaker;
use Moose;

extends 'Dist::Zilla::Plugin::MakeMaker::Awesome';

my $preamble = <<CODE;

unless(eval { require SQL::Translator::Object } && SQL::Translator::Object->can('BUILDARGS')) {
  warn <<EOS;
****
To use MooseX::DBIC::Scaffold you need SQL::Translator from github
http://github.com/arcanez/SQL-Translator
****
EOS
  exit(0);
}

CODE

override _build_MakeFile_PL_template => sub {
  my ($self) = @_;
  my $template = super();
  $template =~ s/^(?=[^#])/$preamble/m;
  return $template;
};

__PACKAGE__->meta->make_immutable;


1;
