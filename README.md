# Sig-Tool

This is a tool for signing Hackage packages in bulk. It works with Hackage to
download your packages and then signs each of them with OpenPGP (GnuPG) and
pushes those signatures to [sig-service](https://github.com/commercialhaskell/sig-archive) & [sig-archive](https://github.com/commercialhaskell/sig-archive).

# Install

    stack install sig-tool

# Setup

First you need to make sure you have a valid GPG key with which to sign
packages. Refer to the [Debian Guide](https://wiki.debian.org/Keysigning) on key-signing again if you don't.

# Downloading Your Packages

<TODO after implementation>

# Inspect Your Packages

<TODO after implementation>

# Signing Your Packages

<TODO after implementation>
