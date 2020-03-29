- [About this project](#sec-1)
- [Using the project](#sec-2)
  - [Getting Nix](#sec-2-1)
  - [Setting up the cache](#sec-2-2)
  - [Installing the programs](#sec-2-3)
- [Release](#sec-3)
- [License](#sec-4)
- [Contribution](#sec-5)

[![img](https://github.com/shajra/shajra-nix-packages/workflows/CI/badge.svg)](https://github.com/shajra/shajra-nix-packages/actions)

# About this project<a id="sec-1"></a>

This project has a [Nix](https://nixos.org/nix) expression for the packages I use on my personal laptop. Later on it may have packages for more machines. For now, the expression evaluates to an attribute set of package derivations.

The likelihood that anyone else will want the exact packages I use is slim. Still, you can look at this project to see how I manage my expressions.

If I build something you like, you can import my expression into your own and select the packages of your choosing, benefiting from the cached build in my personal [Cachix](https://cachix.org/) instance.

# Using the project<a id="sec-2"></a>

There are two primary requirements:

-   You need to be running a GNU/Linux OS
-   You need to have the Nix package manager installed
    
    Optionally, you'll probably want to configure your Nix instance to pull pre-built binaries from my Cachix instance.

## Getting Nix<a id="sec-2-1"></a>

> **<span class="underline">NOTE:</span>** You don't need this step if you're running NixOS, which comes with Nix baked in.

> **<span class="underline">NOTE:</span>** Even though the Nix package manager can be installed on Macs, this project is not tested or supported for Macs.

If you don't already have Nix, the official installation script should work on a variety of GNU/Linux distributions. The easiest way to run this installation script is to execute the following shell command as a user other than root:

```shell
curl https://nixos.org/nix/install | sh
```

This script will download a distribution-independent binary tarball containing Nix and its dependencies, and unpack it in `/nix`.

If you prefer to install Nix another way, reference the [Nix manual](https://nixos.org/nix/manual/#chap-installation)

## Setting up the cache<a id="sec-2-2"></a>

Nix package derivations are deterministic recipes on how to build software packages, but these packages can sometimes take a while build. Nix *substituters* can point to a cache of pre-built Nix packages so you can download them instead of build them from scratch.

Cachix is a service to host Nix caches. I use it to cache all my Nix builds (using GitHub Actions and [cachix-action](https://github.com/marketplace/actions/cachix)). Cachix distributes a command-line utility to configure a Nix substituter for a Cachix cache.

Here's a quick way to get the Cachix binary:

```shell
nix-env -iA cachix -f https://cachix.org/api/v1/install
```

You can then use it to point to my "shajra" Cachix cache:

```shell
cachix use shajra
```

## Installing the programs<a id="sec-2-3"></a>

You can layer my programs on top of previously installed programs with the following call:

```shell
nix-env --install --file .
```

# Release<a id="sec-3"></a>

The "master" branch of the repository on GitHub has the latest released version of this code. There is currently no commitment to either forward or backward compatibility.

"user/shajra" branches are personal branches that may be force-pushed to. The "master" branch should not experience force-pushes and is recommended for general use.

# License<a id="sec-4"></a>

All files in this "shajra-nix-packages" project are licensed under the terms of GPLv3 or (at your option) any later version.

Please see the [./COPYING.md](./COPYING.md) file for more details.

# Contribution<a id="sec-5"></a>

Feel free to file issues and submit pull requests with GitHub.

There is only one author to date, so the following copyright covers all files in this project:

Copyright © 2020 Sukant Hajra
