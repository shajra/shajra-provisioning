- [About this project](#sec-1)
- [Community-curated configuration](#sec-2)
- [Basic operation](#sec-3)
- [Understanding this project](#sec-4)
- [Similar projects](#sec-5)
- [Ideas for the future](#sec-6)
- [Release](#sec-7)
- [License](#sec-8)
- [Contribution](#sec-9)

[![img](https://github.com/shajra/shajra-provisioning/workflows/CI/badge.svg)](https://github.com/shajra/shajra-provisioning/actions)

# About this project<a id="sec-1"></a>

> This is either my love letter to Nix, or Nix's love letter to me, or both.

This project uses [Nix](https://nixos.org/nix) to provision three machines of mine. I may include more eventually.

The main idea of this project is to come into a vanilla OS installation and with only a few steps get a fully provisioned system including

-   all system-level and user-level packages and services
-   all system-level and user-level “dot-file” configurations.

This project can be at least partially useful on any operating system that Nix can be installed on. For example, one of my machines runs NixOS, a full Linux operating system built upon Nix. Another runs MacOS and has the Nix package manager installed on it.

Using Nix and the supporting Nix ecosystem gives us

-   strictly deterministic (reproducible) builds, package installation, and configuration
-   a concise and declarative configuration using community-curated modules
-   easy rollbacks of provisioning
-   easy updates
-   the ability to install multiple versions of software without worry of conflict.

A [provided introduction to Nix](doc/nix-introduction.md) explains motivations to use Nix more.

This repository is designed to be forked and modified, and not really to be used as is. The likelihood that anyone else will want the exact configuration I use is infinitesimal. Still, you can look at this project to see how code is laid out. For the most part, it just shows a recommended way to use a few tools in conjunction to get a fairly complete provisioning. If you're interested, I've documented [more about my personal configuration](doc/ux.md) in a separate document.

# Community-curated configuration<a id="sec-2"></a>

This project delegates heavily to the following tools:

-   [NixOS's `nixos-rebuild`](https://nixos.org/manual/nixos/stable/index.html#sec-changing-config), for NixOS system-level provisioning
-   [Nix-Darwin](https://daiderd.com/nix-darwin), for MacOS system-level provisioning with Nix
-   [The Home Manager project for Nix](https://github.com/nix-community/home-manager), to provision a home directory in either NixOS or MacOS.

Each of these projects give us a community-curated catalog of configuration modules that dramatically simplify the code we need on our end. Our configurations can end up extremely concise and declarative. Here's an example:

```text
{
    …
    hardware.pulse.audio.enable = true;
    …
}
```

You can see this configuration concretely in the [./home](./home) and [./doc](./doc) directories.

These configuration modules also provide a battery of checking to make sure settings are valid and congruent.

# Basic operation<a id="sec-3"></a>

The only prerequisite should be the installation of the Nix package manager, after which we can execute the following commands.

> **WARNING:** Don't actually clone this repository and run the commands below. You'd attempt to turn your computer into mine.

On a NixOS machine:

```sh
nix run github:shajra/shajra-provisioning/shajra-nixos-rebuild switch
nix run github:shajra/shajra-provisioning/shajra-home-manager switch
```

On MacOS with Nix installed:

```sh
nix run github:shajra/shajra-provisioning/shajra-darwin-rebuild switch
nix run github:shajra/shajra-provisioning/shajra-home-manager switch
```

Managing the home directory with a separate command from the command for system-level configuration is a design decision to help with system stability. We can experiment more with our user-level configuration without worrying about accidentally breaking the whole system. In fact, we don't even need to use the system-level provisioning if we don't want to, and can just use the home manager alone.

After we have Git on the system, we can also clone this repository to any target system for more convenient provisioning.

On a NixOS machine:

```sh
./nixos-rebuild switch
./home-manager switch
```

On MacOS with Nix installed:

```sh
./darwin-rebuild switch
./home-manager switch
```

You can also call each script with a `build` command, which builds out the configuration in `/nix/store` without provisioning and activating it. Instead, it will generate a symlink named "result" in the project root directory pointing back to the build. You can look within it and see that it looks right before switching to it.

Use `--help` with these scripts for more options.

Furthermore, updating supporting software to the latest released versions is as simple as a single command:

```sh
nix flake update
```

And because all of the code needed for provisioning is within this repository, rolling back to a previous version can be as simple as checking out an alternate commit in history, and provisioning from that.

# Understanding this project<a id="sec-4"></a>

A lot of third-party software and conventions comes together to enable all the features of this provisioning project. Hopefully, the code is well-factored and comprehensible, but it's still a lot to look at when coming in fresh.

To help, this project provides some standalone guides to help get started, linked below.

If you don't know much about Nix, consider reading the following provided guides:

-   [Introduction to Nix and motivations to use it](doc/nix-introduction.md)
-   [Nix installation and configuration guide](doc/nix-installation.md)
-   [Nix end user guide](doc/nix-usage-flakes.md)
-   [Introduction to the Nix programming language](doc/nix-language.md)

This project leans heavily on an experimental Nix feature called *flakes*. This project is built on top of another project called [Nix-project](https://github.com/shajra/nix-project), which factors out some common code useful for Nix projects in general. Nix-project doesn't have a lot of code, and largely builds on top of another project called [Flake-parts](https://github.com/hercules-ci/flake-parts). Flake parts helps making working with flakes a little easier.

After learning about the Nix and basic usage, the next two guides will help get up to speed with this project:

-   [Nix-project's development guide](https://github.com/shajra/nix-project/blob/main/doc/project-developing.md)
-   [A walk-through of this project](doc/provisioning-walkthrough.md)

The development guide introduces Nix flakes and Flake-parts in the context of Nix-project. The guide also covers some patterns of making packages common to the Nix ecosystem (`callPackage` and *overlays*).

Finally, the walk-through explains how all of these projects and patterns come together in this project provision my personal machines.

# Similar projects<a id="sec-5"></a>

This project wasn't made in isolation. It's just one of many other similar projects out there. I looked at all of these, some more than others:

-   [bkase/life](https://github.com/bkase/life)
-   [divnix/devos](https://github.com/divnix/devos)
-   [martinbaillie/dotfiles](https://github.com/martinbaillie/dotfiles)
-   [michaelpj/nixos-config](https://github.com/michaelpj/nixos-config)
-   [rossabaker/nix-config](https://gitlab.com/rossabaker/nix-config)

# Ideas for the future<a id="sec-6"></a>

When provisioning with Nix we try to do everything we possible can with files built out in `/nix/store` that we then symlink to.

However, there's some limitations to this approach. Fundamental to Nix, `/nix/store` has two very important properties:

-   it's read-only
-   it's publicly visible.

The fact that it's read-only provides some impedance when dealing with mutable configuration. Mutable configuration is generally something to avoid, but it's just hard to completely avoid when provisioning machines.

Home Manager has a system of activation scripts that are run after provisioning symlinks back to `/nix/store`. These side-effecting scripts enable us to do things like cycle services on/off for the new configuration to take place.

However, we can do more with these scripts provided we work hard to make sure they are robust and idemopotent. We absolutely don't want to abuse this escape hatch, but here's some things that would be possible:

-   We can pointing symlinks not only to `/nix/store` but also relatively within the home directory.

-   We can check out "live repositories" to a specified version. Sometimes these repositories by design are expected to have mutable state maintained within them during runtime. This is the case with Spacemacs and Spacevim, for instance. Both of these projects configure a `.gitignore` file to ignore this mutable runtime state.

-   Finally, we can do secrets management without worrying about having to encrypt secrets that might be visible from `/nix/store` or elsewhere. I should be able to transiently pull a secret from a trusted value, and prepare it appropriately on the target system. This might involve weaving the secret with a template built out in `/nix/store`. We just have to be careful that the secret stays out of `/nix/store`, only persisting in its target destination.

This last possibility is big for me. Even if modern encryption is highly unlikely to be cracked, keeping encrypted secrets in public spaces is strictly less secure than hiding them behind other barriers. Security is a complicated field, and we shouldn't compromise our expectations merely because our provisioning tools are missing a feature.

It's true that I could just call another script after the normal provisioning ones, but this just adds more steps. Also, there's benefits to configuring these activation scripts as a declaratively used NixOS-style module.

# Release<a id="sec-7"></a>

The "main" branch of the repository on GitHub has the latest released version of this code. There is currently no commitment to either forward or backward compatibility.

"user/shajra" branches are personal branches that may be force-pushed to. The "main" branch should not experience force-pushes and is recommended for general use.

# License<a id="sec-8"></a>

All files in this "shajra-provisioning" project are licensed under the terms of GPLv3 or (at your option) any later version.

Please see the [./COPYING.md](./COPYING.md) file for more details.

# Contribution<a id="sec-9"></a>

Feel free to file issues and submit pull requests with GitHub.

There is only one author to date, so the following copyright covers all files in this project:

Copyright © 2020 Sukant Hajra
