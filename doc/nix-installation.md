- [About this document](#sec-1)
- [Prerequisites](#sec-2)
- [Level of commitment/risk](#sec-3)
- [Nix package manager installation](#sec-4)
- [Cache setup](#sec-5)
  - [System-level cache configuration](#sec-5-1)
  - [User-level cache configuration](#sec-5-2)
  - [Testing your configuration](#sec-5-3)
- [Setting up experimental features](#sec-6)


# About this document<a id="sec-1"></a>

This document explains how to

-   install [the Nix package manager](https://nixos.org/nix)
-   set up Nix to download pre-built packages from a cache (optionally)
-   set up the Nix's experimental flakes feature (optionally)

If you're unsure if you want to enable flakes or not, read the provided [introduction to Nix](nix-introduction.md).

# Prerequisites<a id="sec-2"></a>

This project supports

-   Linux on x86-64 machines
-   MacOS on x86-64 machines
-   MacOS on ARM64 machines (M1 or M2).

All we need to use this project is to install Nix, which this document covers. Nix can be installed on a variety of Linux and Mac systems. Nix can also be installed in Windows via the Windows Subsystem for Linux (WSL). Installation on WSL may involve steps not covered in this documentation, though.

Note, some users may be using [NixOS](https://nixos.org), a Linux operating system built on top of Nix. Those users already have Nix and don't need to install it separately. You don't need to use NixOS to use this project.

# Level of commitment/risk<a id="sec-3"></a>

Unless you're on NixOS, you're likely already using another package manager for your operating system (APT, DNF, etc.). You don't have to worry about Nix or packages installed by Nix conflicting with anything already on your system. Running Nix alongside other package managers is safe.

All the files of a Nix package are located under `/nix` a directory, isolated from any other package manager. Nix won't touch critical directories under `/usr` or `/var`. Nix then symlinks files under `/nix` to your home directory under dot-files like `~/.nix-profile`. There is also some light configuration under `/etc/nix`.

Hopefully, this alleviates any worry about installing a complex program on your machine. Uninstallation is not too much more than deleting everything under `/nix`.

# Nix package manager installation<a id="sec-4"></a>

> **<span class="underline">NOTE:</span>** You don't need this step if you're running NixOS, which comes with Nix baked in.

Though the latest version of Nix is Nix 2.31.3, we'll be installing the version that the last release of NixOS (25.11) uses, specifically Nix 2.31.2. As discussed in the included [introduction to Nix](nix-introduction.md), this version is considered stable by the Nix community.

The following command calls the official installation script for the recommended version of Nix. Note, this script will require `sudo` access.

```bash
sh <(curl -L https://releases.nixos.org/nix/nix-2.31.2/install) --daemon
```

The `--daemon` option installs Nix in the multi-user mode, which is generally recommended (single-user installation with `--no-daemon` instead is recommended for WSL). The script reports everything it does and touches.

After installation, you may have to exit your terminal session and log back in to have environment variables configured, which puts Nix executables on your `PATH`.

Every six months or so, a new version of NixOS releases, and you should consider upgrading your installation of Nix. For NixOS 25.11, this command upgrades Nix:

```bash
NIXOS_VERSION="25.11"
NIX_STORE_PATHS_URL=https://github.com/NixOS/nixpkgs/raw/$NIXOS_VERSION/nixos/module/installer/tools/nix-fallback-paths.nix
sudo nix upgrade-nix --nix-store-paths-url "$NIX_STORE_PATHS_URL"
```

For [new releases of NixOS](https://nixos.org/manual/nixos/stable/release-notes.html), use the same command with new `NIXOS_VERSION`.

The Nix manual describes [other methods of installing Nix](https://nixos.org/manual/nix/stable/installation/installation) that may suit you more. If you later want to uninstall Nix, see the [uninstallation steps documented in the Nix manual](https://nixos.org/manual/nix/stable/installation/uninstall).

# Cache setup<a id="sec-5"></a>

This project pushes built Nix packages to two substituters, [Garnix](https://garnix.io) and [Cachix](https://cachix.org/), as part of its continuous integration. It's recommended to install at least one of these. Configuring both to have a fallback works as well. Garnix caches a few more packages than Cachix. Both should have similar availability.

We need to extend two settings in either the system-level Nix configuration file at `/etc/nix/nix.conf`, or the user-level configuration at `~/.config/nix/nix.conf` (which may not exist yet).

The choice of whether to perform these settings system-level or user-level is up to your preference.

First we need to specify one or both of the following substituters:

-   <https://cache.garnix.io>
-   <https://shajra.cachix.org>

For each substituter we use, we need to also configure Nix to trust their public keys:

-   cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g=
-   shajra.cachix.org-1:V0x7Wjgd/mHGk2KQwzXv8iydfIgLupbnZKLSQt5hh9o=

## System-level cache configuration<a id="sec-5-1"></a>

When editing the `/etc/nix/nix.conf` as root, suffix the new substituter(s), space-separated to any values already populating the `substituters` parameter.

Next, similarly suffix the key(s) to the `trusted-public-keys` parameter.

Your file will likely look like the following:

    …
    substituters = https://cache.nixos.org/ https://cache.garnix.io https://shajra.cachix.org
    trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g= shajra.cachix.org-1:V0x7Wjgd/mHGk2KQwzXv8iydfIgLupbnZKLSQt5hh9o=
    …

## User-level cache configuration<a id="sec-5-2"></a>

User-level Nix configuration overrides system-level settings. For user-level configuration, we can use the `extra-substituters` and `extra-trusted-public-keys` parameters to extend `substituters` and `trusted-public-keys` settings already in the system-level `/etc/nix/nix.conf` file. This way we don't need to worry about accidentally excluding the standard <https://cache.nixos.org> substituter where we get most of our cache hits.

For user-level Nix configuration, create a file at `~/.config/nix/nix.conf` with the following content:

    …
    extra-substituters = https://cache.garnix.io https://shajra.cachix.org
    extra-trusted-public-keys = cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g= shajra.cachix.org-1:V0x7Wjgd/mHGk2KQwzXv8iydfIgLupbnZKLSQt5hh9o=

## Testing your configuration<a id="sec-5-3"></a>

You can see that your configuration is correct with the following commands:

```sh
nix show-config substituters
nix show-config trusted-public-keys
```

Make sure you still have settings for <https://cache.nixos.org>.

# Setting up experimental features<a id="sec-6"></a>

This project can take advantage of two experimental Nix features:

-   `nix-command`
-   `flakes`

The provided [introduction to Nix](nix-introduction.md) covers in detail what these features are and can help you decide whether you want to enable them.

As you can guess, the `flakes` feature enables flakes functionality in Nix. The `nix-command` feature enables a variety of subcommands of Nix's newer `nix` command-line tool, some of which allow us to work with flakes.

If you don't enable experimental features globally, there is a option to enable features local to just a single command-line invocation. For example, to use flakes-related commands, we call `nix --extra-experimental-features 'nix-command flakes' …`. When not configuring globally, setting an alias for this can be useful. The following command illustrates setting an alias in most POSIX-compliant shells:

```sh
alias nix-flakes='nix --extra-experimental-features "nix-command flakes"'
```

As discussed in the introduction, `nix-command` is enabled by default. You don't need to enable it explicitly (though you could disable it).

The easiest way to turn on experimental features is to put the following setting into either the system-level `/etc/nix/nix.conf` file or the user-level `~/.config/nix/nix.conf` file:

```text
experimental-features = nix-command flakes
```

Then you should see that the appropriate features are enabled:

```sh
nix show-config experimental-features
```

    fetch-tree flakes nix-command

Note that the `fetch-tree` experimental feature is required and automatically enabled with the enablement of `flakes`.
