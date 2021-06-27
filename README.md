- [About this project](#sec-1)
- [Using the project](#sec-2)
  - [Nix package manager setup](#sec-2-1)
  - [Cache setup](#sec-2-2)
  - [Installing the programs](#sec-2-3)
- [Release](#sec-3)
- [License](#sec-4)
- [Contribution](#sec-5)

[![img](https://github.com/shajra/shajra-machines/workflows/CI/badge.svg)](https://github.com/shajra/shajra-machines/actions)

# About this project<a id="sec-1"></a>

This project has a [Nix](https://nixos.org/nix) expression for the Nix packages I install on my personal laptops (one NixOS, another MacOS). Later on it may have packages for more machines. For now, the expression evaluates to an attribute set of package derivations. The package sets are different depending on the detected operating system.

The likelihood that anyone else will want the exact packages I use is slim. Still, you can look at this project to see how I manage my expressions.

If I build something you like, you can import my expression into your own and select the packages of your choosing, benefiting from the cached build in my personal [Cachix](https://cachix.org/) instance.

See [the provided documentation on Nix](doc/nix.md) for more on what Nix is, why we're motivated to use it, and how to get set up with it for this project.

# Using the project<a id="sec-2"></a>

## Nix package manager setup<a id="sec-2-1"></a>

> **<span class="underline">NOTE:</span>** You don't need this step if you're running NixOS, which comes with Nix baked in.

If you don't already have Nix, the official installation script should work on a variety of UNIX-like operating systems. The easiest way to run this installation script is to execute the following shell command as a user other than root:

```shell
curl https://nixos.org/nix/install | sh
```

This script will download a distribution-independent binary tarball containing Nix and its dependencies, and unpack it in `/nix`.

The Nix manual describes [other methods of installing Nix](https://nixos.org/nix/manual/#chap-installation) that may suit you more.

## Cache setup<a id="sec-2-2"></a>

It's recommended to configure Nix to use shajra.cachix.org as a Nix *substituter*. This project pushes built Nix packages to [Cachix](https://cachix.org/) as part of its continuous integration. Once configured, Nix will pull down these pre-built packages instead of building them locally.

You can configure shajra.cachix.org as a substituter with the following command:

```shell
nix run \
    --file https://cachix.org/api/v1/install \
    cachix \
    --command cachix use shajra
```

This will perform user-local configuration of Nix at `~/.config/nix/nix.conf`. This configuration will be available immediately, and any subsequent invocation of Nix commands will take advantage of the Cachix cache.

If you're running NixOS, you can configure Cachix globally by running the above command as a root user. The command will then configure `/etc/nixos/cachix/shajra.nix`, and will output instructions on how to tie this configuration into your NixOS configuration.

## Installing the programs<a id="sec-2-3"></a>

You can layer my programs on top of previously installed programs with the following call:

```shell
nix-env --install --file .
```

`nix-env` maintains a symlink tree of all installed packages at `~/.nix-profile`, so set your `PATH` variable accordingly if it's not already set from your Nix installation:

```shell
export PATH="$HOME/.nix-profile/bin:$PATH"
```

# Release<a id="sec-3"></a>

The "master" branch of the repository on GitHub has the latest released version of this code. There is currently no commitment to either forward or backward compatibility.

"user/shajra" branches are personal branches that may be force-pushed to. The "master" branch should not experience force-pushes and is recommended for general use.

# License<a id="sec-4"></a>

All files in this "shajra-machines" project are licensed under the terms of GPLv3 or (at your option) any later version.

Please see the [./COPYING.md](./COPYING.md) file for more details.

# Contribution<a id="sec-5"></a>

Feel free to file issues and submit pull requests with GitHub.

There is only one author to date, so the following copyright covers all files in this project:

Copyright © 2020 Sukant Hajra
