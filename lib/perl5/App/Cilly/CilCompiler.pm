# Define here your favorite compiler by overriding Merger methods
package App::Cilly::CilCompiler;

use File::Basename;
use strict;
BEGIN {
    @App::Cilly::CilCompiler::ISA = qw(App::Cilly);
    $App::Cilly::CilCompiler::base =
      "$FindBin::RealBin/$FindBin::RealScript";
    # Use the most recent version of cilly
    $App::Cilly::CilCompiler::mtime_native =
    int((stat("$App::Cilly::CilCompiler::base.native"))[9]);
    $App::Cilly::CilCompiler::mtime_byte =
    int((stat("$App::Cilly::CilCompiler::base.byte"))[9]);
    $App::Cilly::CilCompiler::use_debug =
         grep(/--bytecode/, @ARGV) ||
         grep(/--ocamldebug/, @ARGV) ||
        ($App::Cilly::CilCompiler::mtime_native < $App::Cilly::CilCompiler::mtime_byte);
    $App::Cilly::CilCompiler::compiler =
        $App::Cilly::CilCompiler::base .
            ($App::Cilly::CilCompiler::use_debug ? ".byte" : ".native");
    if($App::Cilly::CilCompiler::use_debug) {
        $ENV{"OCAMLRUNPARAM"} = "b" . $ENV{"OCAMLRUNPARAM"};
    }
    # Fix ocamlfind search path
    if(open (my $input_fh, "<",
        "$FindBin::RealBin/../share/cil/ocamlpath")) {
      my $ocamlpath = <$input_fh>;
      chomp($ocamlpath);
      close $input_fh;
      # ocamlpath separator is ':' on Unix, but ';' on Windows
      my $ocamlsep = ($^O eq "MSWin32" || $^O eq "cygwin") ? ';' : ':';
      $ENV{"OCAMLPATH"} = join($ocamlsep, $ocamlpath, $ENV{"OCAMLPATH"});
    }
}

# We need to customize the collection of arguments
sub collectOneArgument {
    my($self, $arg, $pargs) = @_;
    if($arg =~ m|--transval=(.+)$|)  {
        $self->{TRANSVAL} = $1; return 1;
    }
    if($arg eq '--ocamldebug')  {
        $self->{OCAMLDEBUG} = 1; return 1;
    }
    if($arg eq '--cabsonly') {
        $self->{CABSONLY} = 1; return 1;
    }
    # See if the super class understands this
    return $self->SUPER::collectOneArgument($arg, $pargs);
}

sub usage {
    print "Usage: $FindBin::Script [options] [gcc_or_mscl arguments]\n";
}

sub helpMessage {
    my($self) = @_;
    # Print first the original
    $self->SUPER::helpMessage();
    print <<EOF;

  All other arguments starting with -- are passed to the Cilly process.

The following are the arguments of the Cilly process
EOF
   my @cmd = ($App::Cilly::CilCompiler::compiler, '--help');
   if(defined $self->{CILARGS}) {
       push @cmd, @{$self->{CILARGS}};
   }
   $self->runShell(@cmd);
}


sub CillyCommand {
    my ($self, $ppsrc, $dest) = @_;

    my $aftercil;
    my @cmd = ($App::Cilly::CilCompiler::compiler);

    if(defined $ENV{OCAMLDEBUG} || $self->{OCAMLDEBUG}) {
        print "OCAMLDEBUG is on\n";
        my @idirs = (".", "src", "src/frontc", "src/ext",
                     "ocamlutil",
                     "obj/");
	my @iflags = map { ('-I', "$FindBin::RealBin/../$_") } @idirs;
        unshift @cmd, 'ocamldebug', '-emacs', @iflags;
    }
    if($::docxx) {
        push @cmd, '--cxx';
    }
    if($self->{CABSONLY}) {
        $aftercil = $self->cilOutputFile($dest, 'cabs.c');
        push @cmd, '--cabsonly', $aftercil;
    } else {
        if(defined $self->{CILLY_OUT}) {
            $aftercil = new App::Cilly::OutputFile($dest, $self->{CILLY_OUT});
            return ($aftercil, @cmd);
        }
	$aftercil = $self->cilOutputFile($dest, 'cil.c');
    }
    return ($aftercil, @cmd, '--out', $aftercil);
}

sub MergeCommand {
    my ($self, $ppsrc, $dir, $base) = @_;

    return ('', $App::Cilly::CilCompiler::compiler);
}

1;

__END__
