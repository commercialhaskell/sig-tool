# Sig-Tool

This is a tool for signing Hackage packages in bulk. It works with Hackage to
download your packages and then signs each of them with OpenPGP (GnuPG) and
pushes those signatures to [sig-service](https://github.com/commercialhaskell/sig-archive) & [sig-archive](https://github.com/commercialhaskell/sig-archive).

# Install

    stack install sig-tool

# Setup

-   GPG Keys
    
    First you need to make sure you have a valid GPG key with which to sign
    packages. Refer to the [Debian Guide](https://wiki.debian.org/Keysigning) on key-signing again if you don't.

-   Cabal Install
    
    This tool leverages the command line tool \`cabal-install\`. If you don't have
    it already just issue a \`stack install cabal-install\`.

# Downloading Your Packages

\`sig-tool setup <hackage-username>\` will download all your packages from Hackage
and write a manifest file with all their SHA256 sums. Stack users may find it
easier to run \`stack exec &#x2013; sig-tool setup <hackage-username>\` so that GHC is
in your PATH.

# Inspecting Your Packages

The tool stops after \`setup\` so you can unpack & view your release tarballs. You
can also decide to trim out any packages you aren't interested in signing by
simply remove them from the manifest file.

# Signing Your Packages

After you are finished inspecting your packages & trimming the manifest (if
any). run \`sig-tool sign\` to sign all your packages.
