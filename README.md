# Sig-Tool

This is a tool for signing your Haskell packages with GPG. It makes it
easy to sign & upload signatures of packages to the
[sig-archive](https://github.com/commercialhaskell/sig-archive)
repository.

# Install

    cabal update
    cabal install sig-tool
    sig --help

# Signing Your Package

You can sign your own package & upload the signature to the
sig-service.  Try it!

First you need to make sure you have a valid GPG key with which to
sign packages.  Refer to the
[Debian Guide](https://wiki.debian.org/Keysigning) on key-signing
again if you don't.

Next you'll need a haskell package to sign. Grab one of your own
from hackage.  Then build sdist with the following commands:

    cabal check && cabal sdist

After you have a tarball on disk you can sign & upload the signature. Try this:

    sig sdist dist/mypackage-3.1.7.0.tar.gz

# Signing Your Entire Package Tree At Once

You can sign all of the versions of all of your packages at once with
the \`hackage\` command.

    sig hackage <hackage-username>
