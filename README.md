# Sig-Tool

This is a tool for signing, verifying and installing signed packages
from sig-archive.

It talks to \`sig-service\` for getting the current archive from
[sig-archive](https://github.com/commercialhaskell/sig-archive) and also for pushing signatures of packages.

# Install

    cabal update
    cabal install sig-tool
    sig --help

# Getting Started

Initialize your local signature repository with the following:

    sig init

This will drop a default config file in <file:///home/tim/.sig/config.yaml> and
update your [signature archive](https://github.com/commercialhaskell/sig-archive) in the folder <file:///home/tim/.sig/sig-archive>.

# Listing Trusted Mappings (People to Package Signatures)

First let's look at a list of what \`sig\` thinks is the list of valid
package signers.  Type the following:

    sig mappings

# Updating Trusted Mappings (Periodically)

Just like cabal update, from time to time you should run \`sig
update\` to update your mappings & signatures.

    sig update

# Trusting Signers & Checking Package Signatures

Try checking the yesod package with the following:

    sig check yesod

You'll probably get an immediate error. "ERROR: no verifiable
signature for <file:///home/tim/.sig/sig-archive/mappings/yesod.yaml>"

This means we have a signature mapping file in the archive for yesod
but we don't trust it yet.  In order to trust the mapping file we
need to do two steps.  They are as follows:

-   You will need to import, verify, sign & trust the mapping file
    signer's GPG key.  (This is not covered in this short how-to. If
    you need help read the [Debian Guide](https://wiki.debian.org/Keysigning) on key-signing.)

-   You will then need to to tell sig-tool that you trust the signer
    of the mapping file.

This two step verification process indicates that you agree that the
person(s) signing the mapping file is trustworthy and that you agree
the signers in the file should be allowed to sign the packages
listed under their name.

Try trusting me, Tim Dysinger, as the signer of the test yesod.yaml
mapping file.  To do so issue the following command:

First fetch my key if you don't have it:

    gpg --keyserver pgp.mit.edu. --recv-keys 44A52A60

After you receive the key, you should contact me to verify the
fingerprint.  Then you should sign & trust the key with GPG.  Again,
read the Debian Key-signing guide for a how-to.

After you have my key all setup you can then tell sig-tool to trust
my key.

    sig trust 44A52A60 tim@dysinger.net

This will update your <file:///home/tim/.sig/config.yaml> file.  We can now go back
to verifying yesod.  Type the following:

    sig check yesod

and you should see that you get an 'OK' for each package as we
verify the signatures with GPG.

You can use specific versions or add cabal arguments to the end of
the sig command after a double-dash. This enables you to alter
cabal-install's behavior.

    sig check yesod-1.4.1.5 -- --enable-tests

# Installing a Package

Installation via cabal-install is just an extension to signature
checking. \`sig check\` is just a &#x2013;dry-run of \`sig install\`.  If you
would like to install a package after verifying it's source package
signatures, type the following:

    sig install yesod-1.4.1.5 -- --enable-tests

# Signing Your Package

You can sign your own package & upload the signature to the
sig-service.  Try it!

First you need to make sure you have a valid GPG key with which to
sign packages.  Refer to the Debian Key-signing documentation again
if you don't.

Next you'll need a haskell package to sign. Grab one of your own
from hackage.  Then build sdist with the following commands:

    cabal check && cabal sdist

After you have a tarball on disk you can sign & upload the signature. Try this:

    sig sign dist/mypackage-3.1.7.0.tar.gz

# Signing Your Entire Package Tree At Once

You can bulk sign using cabal fetch & GNU parallel.  This is most
useful while we are bootstrapping & we don't yet have all hackage
maintainers on board.

    % mv ~/.cabal ~/.cabal.bak
    % mv ~/.ghc ~/.ghc.bak
    % cabal update
    % cabal fetch yesod
    % find ~/.cabal/packages -name '*.tar.gz'|grep -v 00-index|parallel --no-notice sig sign

This is what I used to sign the entire yesod package tree.
