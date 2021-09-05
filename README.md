- [About this project](#sec-1)
- [Prerequisites](#sec-2)
  - [Installing your operating system](#sec-2-1)
  - [Nix package manager setup](#sec-2-2)
  - [Cache setup](#sec-2-3)
  - [Getting this project onto a new machine](#sec-2-4)
- [Using the project](#sec-3)
  - [Change the configuration to your liking](#sec-3-1)
    - [Project code layout](#sec-3-1-1)
    - [Configuration modules](#sec-3-1-2)
    - [This project's module convention](#sec-3-1-3)
    - [Package building and selection](#sec-3-1-4)
  - [Provisioning your machine](#sec-3-2)
  - [Updating dependencies](#sec-3-3)
- [Miscellany](#sec-4)
  - [Similar projects](#sec-4-1)
  - [Ideas for the future](#sec-4-2)
  - [Why use Niv and not Nix Flakes?](#sec-4-3)
- [Release](#sec-5)
- [License](#sec-6)
- [Contribution](#sec-7)

[![img](https://github.com/shajra/shajra-provisioning/workflows/CI/badge.svg)](https://github.com/shajra/shajra-provisioning/actions)

# About this project<a id="sec-1"></a>

> This is either my love letter to Nix, or Nix's love letter to me, or both.

This project uses [Nix](https://nixos.org/nix) to provision two machines of mine. I may include more eventually. One of these machines runs MacOS and has had the Nix package manager installed on it. The other runs NixOS, a full Linux operating system built upon Nix. This project can be at least partially useful on any OS Nix can be installed on.

This repository is designed to be forked and modified, and not really to be used as is. The likelihood that anyone else will want the exact configuration I use is infinitesimal. Still, you can look at this project to see how code is laid out. For the most part, it just shows a recommended way to use a few tools in conjunction to get a fairly complete provisioning.

If you've used other configuration management tools for this kind of task, hopefully this project can show you a much improved alternative. Because of Nix, we get a provisioning that's deep, precise, idempotent, and highly repeatable, much moreso than tools like Ansible. Additionally, almost the entire system is built by a continuous integration job, and published to a public cache for convenient redistribution. So we're getting benefits similar to those of Docker, but without the restriction to containers.

If you are new to Nix, this project provides [a small guide](./doc/nix.md) to get you started with it. Also, the [official Nix documentation](https://nixos.org/learn.html) is a great resource as well.

The main idea of this project is to come into a relatively vanilla OS installation and with one command completely configure system-level services and packages. Then with another command we can completely build out a home directory, including all the requisite dot-file configurations. So after just two commands, the system should be ready to use.

On a NixOS machine this will look like:

```sh
./nixos-rebuild switch
./home-manager switch
```

On MacOS with Nix installed, we can run:

```sh
./darwin-rebuild switch
./home-manager switch
```

Managing the home directory with a separate command from the command for system-level configuration is a design decision to help with system stability. We can experiment more with our user-level configuration without worrying about accidentally breaking the whole system. In fact, we don't even need to use the system-level provisioning if we don't want to, and can just use `home-manager` alone.

Ultimately, configuration files, both system-level as well as user-level, are built out under `/nix/store` and then symlinked from their respective targets. These commands generally will check for pre-existing files before attempting to provision anything. Your files should be protected from being overwritten. If there's a conflict, you'll get a message informing you how to change things to proceed.

Furthermore, updating supporting software to the latest released versions is as simple as a single command:

```sh
./support/dependencies-update
```

And because all of the code needed for provisioning is within this repository, rolling back to a previous version can be as simple as checking out an alternate commit in history, and provisioning from that.

This project delegates heavily to the following tools:

-   [NixOS's `nixos-rebuild`](https://nixos.org/manual/nixos/stable/index.html#sec-changing-config): for NixOS system-level provisioning
-   [Nix-Darwin](https://daiderd.com/nix-darwin): for MacOS system-level provisioning with Nix
-   [The Home Manager project for Nix](https://github.com/nix-community/home-manager): to provision a home directory in either NixOS or MacOS
-   [Niv](https://github.com/nmattia/niv): a tool to help manage versions of this project's external dependencies.

This delegation is done by the wrapper scripts in this project's root:

-   [./home-manager](./home-manager)
-   [./nixos-rebuild](./nixos-rebuild)
-   [./darwin-rebuild](./darwin-rebuild)

These wrapper scripts call upstream programs bearing the same names. These projects each give us a catalog of configuration modules that dramatically simplify the code we need on our end. Our configurations can end up extremely concise and declarative. Here's an example:

```text
{
    …
    hardware.pulse.audio.enable = true;
    …
}
```

Enabled by this approach, there's a battery of checking done by configuration modules to make sure settings are valid and congruent.

Beyond a few switches parsed by these wrapper scripts, the rest of the arguments are passed through to the respective upstream program. If you're already familiar with these tools there shouldn't be too many surprises.

Something important these wrapper scripts do is highly restrict sensitivity to environment variables on your path that might affect how tools provision. If you look within these scripts, you'll see this illustrated by the `--ignore-enviornment` switch, which clears out environment variables.

Because environment variables like `NIX_PATH` are ignored completely, and rebuilt internally, we can avoid all the extra steps otherwise needed to set them up. Additionally, independence from the caller's environment improves the repeatability and precision of our provisioning.

# Prerequisites<a id="sec-2"></a>

Though this project does a bulk of the provisioning, the system does require some minimal setup. Specifically, you need to

1.  install an operating system
2.  install Nix for non-NixOS operating systems
3.  optionally (though recommended) point to a cache of pre-built artifacts to save compilation time
4.  get this project's source code on the target machine.

Note that you don't have to follow the official installation for either Nix-Darwin or Home Manager. Both of these tools are bundled at pinned versions by this project and made available by the wrapper scripts.

## Installing your operating system<a id="sec-2-1"></a>

Instructions for installation of an operating system is beyond the scope of this document. MacOS is almost certainly preinstalled by Apple. For NixOS, see the [NixOS manual](https://nixos.org/manual/nixos/stable/index.html) for detailed installation instructions. Other operating systems should have their respective installation guides.

## Nix package manager setup<a id="sec-2-2"></a>

> **<span class="underline">NOTE:</span>** You don't need this step if you're running NixOS, which comes with Nix baked in.

If you don't already have Nix, [the official installation script](https://nixos.org/learn.html) should work on a variety of UNIX-like operating systems:

```bash
sh <(curl -L https://nixos.org/nix/install) --daemon
```

If you're on a recent release of MacOS, you will need an extra switch:

```bash
sh <(curl -L https://nixos.org/nix/install) --daemon \
    --darwin-use-unencrypted-nix-store-volume
```

After installation, you may have to exit your terminal session and log back in to have environment variables configured to put Nix executables on your `PATH`.

The `--daemon` switch installs Nix in the recommended multi-user mode. This requires the script to run commands with `sudo`. The script fairly verbosely reports everything it does and touches. If you later want to uninstall Nix, you can run the installation script again, and it will tell you what to do to get back to a clean state.

The Nix manual describes [other methods of installing Nix](https://nixos.org/nix/manual/#chap-installation) that may suit you more.

## Cache setup<a id="sec-2-3"></a>

It's recommended to configure Nix to use shajra.cachix.org as a Nix *substitutor*. This project pushes built Nix packages to [Cachix](https://cachix.org/) as part of its continuous integration. Once configured, Nix will pull down these pre-built packages instead of building them locally (potentially saving a lot of time). This augments the default substitutor that pulls from cache.nixos.org.

You can configure shajra.cachix.org as a substitutor with the following command:

```sh
nix run \
    --file https://cachix.org/api/v1/install \
    cachix \
    --command cachix use shajra
```

Cachix is a service that anyone can use. You can call this command later to add substitutors for someone else using Cachix, replacing "shajra" with their cache's name.

If you've just run a multi-user Nix installation and are not yet a trusted user in `/etc/nix/nix.conf`, this command may not work. But it will report back some options to proceed.

One option sets you up as a trusted user, and installs Cachix configuration for Nix locally at `~/.config/nix/nix.conf`. This configuration will be available immediately, and any subsequent invocation of Nix commands will take advantage of the Cachix cache.

You can alternatively configure Cachix as a substitutor globally by running the above command as a root user (say with `sudo`), which sets up Cachix directly in `/etc/nix/nix.conf`. The invocation may give further instructions upon completion.

## Getting this project onto a new machine<a id="sec-2-4"></a>

This project doesn't support remote provisioning. So we need a way to get this project on the system we want to provision. One way is to clone it with Git, assuming the machine we want to provision has network connectivity.

MacOS, fortunately, comes with Git preinstalled, so you can just use that.

If Git is not on your path, since you've now installed Nix, you can enter a Nix Shell providing Git on the path with the following command:

```sh
nix-shell -p git -I nixpkgs=channel:nixpkgs-unstable
```

Once you have access to Git, you can clone the project:

```sh
git clone git@github.com:shajra/shajra-provisioning.git
```

# Using the project<a id="sec-3"></a>

## Change the configuration to your liking<a id="sec-3-1"></a>

You *very certainly* do not want to try to configure your machines exactly like mine. This is my personal repository, and it has things like my name and emails hard-coded in.

### Project code layout<a id="sec-3-1-1"></a>

The following table gives an overview of the project's layout, and includes pointers of where to change code to suit your needs:

| File/Directory                | Change       | Description                                                |
|----------------------------- |------------ |---------------------------------------------------------- |
| `config.nix`                  | Eventually   | Top-level project configuration                            |
| `darwin-rebuild`              |              | Script to provision MacOS system-level                     |
| `default.nix`                 |              | Top-level Nix code to tie everything together              |
| `home/modules`                | Definitely   | Modules to assist building up home directory configuration |
| `home/target/$HOSTNAME`       | Definitely   | Home directory configuration by target machine hostname    |
| `home-manager`                |              | Script to configure a home directory                       |
| `infrastructure`              |              | Nix code to help build packages                            |
| `machines/$HOSTNAME`          | Definitely   | System-level configuration by target machine hostname      |
| `nixos-rebuild`               |              | Script to provision NixOS system-level                     |
| `packages.nix`                | Probably     | Packages to be used for provisioning                       |
| `external`                    | Not manually | Specification of external dependencies                     |
| `support/dependencies-update` |              | Script to update dependencies in `external`                |
| `support/docs-generate`       |              | Script to generate Markdown documentation                  |

Notably, system-level configuration are in directories under `machines`. And configuration of home directories is under `home`. If you use/reference this project, these directories will be your primary focus.

### Configuration modules<a id="sec-3-1-2"></a>

Whether setting system-level configuration with `nixos-rebuild` or `darwin-rebuild`, or user-level configuration with `home-manager`, all of these tools are configured with the NixOS-style configuration module infrastructure.

The goal of NixOS modules is to make it easy to toggle features on/off with lightweight declarative configuration options. The upstream projects (NixOS, Nix-Darwin, and Home Manager) maintain a huge number of modules for various tools and configuration. The code in these modules do all the complicated tasks of weaving everything together into configuration files. These modules also bake in a lot of useful verification of our configuration. We're just benefiting by reusing these upstream modules. Upstream modules define configurations options that can be set. Our modules then set these options.

The following is very helpful documentation on configuration for various NixOS-style modules:

-   [NixOS's configuration syntax](https://nixos.org/manual/nixos/stable/index.html#sec-configuration-file)
-   [NixOS's configuration options](https://nixos.org/manual/nixos/stable/index.html#ch-configuration)
-   [Nix-Darwin's configuration options](https://daiderd.com/nix-darwin/manual/index.html#sec-options)
-   [Home Manager's configuration options](https://nix-community.github.io/home-manager/options.html)

Read the NixOS documentation on configuration syntax first, even if you aren't running NixOS, because it gives details on the syntax of NixOS-style modules, which you'll find under both `machines` and `home` in this project.

Once you understand the format of NixOS-style modules and how they work, you can look at the rest of the documentation for all the options available to you by all the modules provided by upstream projects.

Also, once you've provisioned a system once, it will likely have some man pages you can call instead of looking up documentation online. To get details on system-level configuration for NixOS or MacOS/Nix-Darwin call:

```sh
man configuration.nix
```

For details on Home Manager configuration call:

```sh
man home-configuration.nix
```

### This project's module convention<a id="sec-3-1-3"></a>

Each wrapper script for `nixos-rebuild`, `darwin-rebuild`, and `home-manager` by default guesses what module to load by inspecting the hostname of the caller's computer. Each of these scripts provides a `--target` switch to override this inspection.

This is why the directories under `home/target` and `machines` have funny names (they are the hostnames of my machines).

It's unlikely that your machines have hostnames that match mine, so you can just create new configurations in sibling directories named after your machines.

As is common among Nix users, the system-level configuration is kept minimal, keeping closely to hardware configuration, and services that either can't or shouldn't be run as user-level. Everything else is managed by Home Manager in a home directory.

Because there's more user-level configuration, I take more advantage of the modularity of NixOS-style configuration modules to factor it into reusable units. Technically, `home/target` and `home/modules` both have NixOS-style modules. The ones under `home/target` are just entrypoints that then import modules from `home/modules`.

You may notice that this project uses a more inheritance-style approach for user-level modules, rather than a mixin-style approach. The inheritance style leads to less importing, but more coupling of configuration that might technically be more separable. This is just a personal choice. Creating quality mixins requires thinking more about coupling, and ultimately leads to more work than seemed worth it for my few computers. As I add more computers, I may use more a hybrid of inheritance- and mixin-style.

### Package building and selection<a id="sec-3-1-4"></a>

If you look at the [base module for all my home directory configuration](./home/modules/base/default.nix), you'll see the `home.packages` configuration is set to a list of packages that is ultimately specified by the top-level [`packages.nix`](./packages.nix) file. Some of these packages come directly from the standard Nix package repository (Nixpkgs), but other packages are custom-built from source (for example, some Haskell tooling and little scripts that seem too small to bother pulling out into another repository). The code under `infrastructure` provides an API that helps keep `packages.nix` more declarative.

However, you don't need to use any of this infrastructure, and can chose packages directly from the `pkgs` parameter available to every NixOS-style module.

Just be aware that the version of Nixpkgs you'll get will vary per operation:

| Operation        | Operating System | Module `pkgs` selected from `external/sources.json` |
|---------------- |---------------- |--------------------------------------------------- |
| `nixos-rebuild`  | NixOS            | `nixos-$RELEASE_VERSION`                            |
| `darwin-rebuild` | MacOS            | `nixpkgs-$RELEASE_VERSION-darwin`                   |
| `home-manager`   | not MacOS        | `nixpkgs-unstable`                                  |
| `home-manager`   | MacOS            | `nixpkgs-$RELEASE_VERSION-darwin`                   |

What this means is that I'm running my Linux machine's user-level packages a touch more experimental than my Mac's. You can change this in [`config.nix`](./config.nix) if you like. The code is not flexible enough to vary this selection by machine, just platform. I just don't have a need for anything fancier yet.

## Provisioning your machine<a id="sec-3-2"></a>

Once you finally have your configuration where you think you want it, as mentioned before, you can try to provision it. Go to the root of the project, and call the provided top-level wrapper scripts.

On NixOS, this looks like the following:

```sh
./nixos-rebuild switch
./home-manager switch
```

On MacOS with Nix installed, you run:

```sh
./darwin-rebuild switch
./home-manager switch
```

If you call these wrapper scripts with no arguments, that will be the equivalent of calling them with the `build` command. The `build` command will build out your configuration in `/nix/store` but not deploy it within your system. Instead, it will generate a symlink named "result" in the project root directory pointing back to the build. You can look within it and see that it looks right before switching to it.

Even if you make a mistake, it's generally pretty easy to check out a prior version from source control and rerun the provisioning wrapper scripts.

These wrapper scripts call upstream programs bearing the same names. Beyond a few switches, the rest of the arguments are passed through to the respective upstream program.

Each wrapper script provides a usage message you can access with the `--help` switch. To get the `--help` message of the upstream program, call the wrapper with `-- --help`. The `--` forces all arguments after to go to the upstream program.

This means that you have almost the full power of the upstream script. You can learn more about what's possible from the upstream documentation:

-   [NixOS's `nixos-rebuild`](https://nixos.org/manual/nixos/stable/index.html#sec-changing-config)
-   [Nix-Darwin](https://daiderd.com/nix-darwin)
-   [The Home Manager project for Nix](https://github.com/nix-community/home-manager)

Just remember that all Nix-oriented environment variables are cleared out, such as `NIX_PATH`, so options like `nixos-rebuild`'s `--upgrade` won't function as usual. The `--upgrade` option is convenient for updating packages provided by Nix channels found on `NIX_PATH`. However, channels are mutable, and therefore not reliably repeatable. This is why this project manages versions of dependencies with Niv instead of Nix channels, so that the exact versions of our dependencies are locked down and under source control.

## Updating dependencies<a id="sec-3-3"></a>

Versions of all external dependencies are specified in the [`external/sources.json`](external/sources.json) file. But we won't edit this file directly. Instead we manage it with the [`support/dependencies-update`](support/dependencies-update) script, which ultimately wraps an upstream program called [Niv](https://github.com/nmattia/niv).

Generally, I call this script with no arguments, and it goes online to find the latest commits for all the repositories in `sources.json`:

```sh
support/dependencies-update
```

Many dependencies managed by Niv may come from GitHub. GitHub will rate limit anonymous API calls to 60/hour, which is not a lot. To increase this limit, you can make a [personal access token](https://github.com/settings/tokens) with GitHub. Then write the generated token value in the file `~/.config/nix-project/github.token`. Make sure to restrict the permissions of this file appropriately.

Like the other wrapper scripts, you can call `dependencies-update` with a `--help` switch to get a usage message. You can also send a command directly to Niv by prefixing it with the `niv` subcommand.

For instance, to get `dependencies-update`'s usage message, we'd run the following:

```sh
support/dependencies-update --help
```

    USAGE:
    
        nix-project [OPTION]... scaffold
        nix-project [OPTION]... init-update [--] [NIV_UPDATE_ARGS]...
        nix-project [OPTION]... niv NIV_COMMAND...
    …

But to use `dependencies-update` to get Niv's usage message, we'd run the following:

```sh
support/dependencies-update niv --help
```

    niv - dependency manager for Nix projects
    
    version: 0.2.19
    
    Usage: niv [-s|--sources-file FILE] [--no-colors] COMMAND
    …

The `nix` subcommand gives us the full generality of what Niv can do. See [Niv's documentation](https://github.com/nmattia/niv) for details.

In case you're curious about the "nix-project" name, the wrapper scripts in `support` use another project called [Nix-project](https://github.com/shajra/nix-project).

# Miscellany<a id="sec-4"></a>

## Similar projects<a id="sec-4-1"></a>

This project wasn't made in isolation. It's just one of many other similar projects out there. I looked at all of these, some more than others:

-   [bkase/life](https://github.com/bkase/life)
-   [divnix/devos](https://github.com/divnix/devos)
-   [martinbaillie/dotfiles](https://github.com/martinbaillie/dotfiles)
-   [michaelpj/nixos-config](https://github.com/michaelpj/nixos-config)
-   [rossabaker/nix-config](https://gitlab.com/rossabaker/nix-config)

## Ideas for the future<a id="sec-4-2"></a>

We try to do everything we possible can with files built out in `/nix/store` that we then symlink to.

However, there's some limitations to this approach. `/nix/store` has two very important properties:

-   it's read-only
-   it's publicly visible.

The fact that it's read-only makes it a partial solution for dealing with mutable configuration. Mutable configuration is generally something to avoid, but it's just hard to completely avoid when provisioning machines.

Home Manager has a system of activation scripts that are run after provisioning symlinks back to `/nix/store`. These side-effecting scripts enable us to do things like cycle services on/off for the new configuration to take place.

However, we can do more with these scripts provided we work hard to make sure they are robust and idemopotent. We absolutely don't want to abuse this escape hatch, but here's some things that would be possible:

-   We can pointing symlinks not only to `/nix/store` but also relatively within the home directory.

-   We can check out "live repositories" to a specified version. Sometimes these repositories by design are expected to have mutable state maintained within them during runtime. This is the case with Spacemacs and Spacevim, for instance. Both of these projects configure a `.gitignore` file to ignore this mutable runtime state.

-   Finally, we can do secrets management without worrying about having to encrypt secrets that might be visible from `/nix/store` or elsewhere. I should be able to transiently pull a secret from a trusted value, and prepare it appropriately on the target system. This might involve weaving the secret with a template built out in `/nix/store`. We just have to be careful that the secret stays out of `/nix/store`, only persisting in its target destination.

This last possibility is big for me. Even if modern encryption is highly unlikely to be cracked, keeping encrypted secrets in public spaces is strictly less secure than hiding them behind other barriers. Security is a complicated field, and we shouldn't compromise our expectations merely because our provisioning tools are missing a feature.

It's true that I could just call another script after the normal provisioning ones, but this just adds more steps. Also, there's benefits to configuring these activation scripts as a declaratively used NixOS-style module.

## Why use Niv and not Nix Flakes?<a id="sec-4-3"></a>

I'm basically holding out for Nix Flakes to officially release. Otherwise, from what I understand flakes seems to do what Niv does, and more.

For instance, using Flakes finally gets us out of the world of worrying about environment variables wreaking such havoc on our Nix expressions. I don't think this means that all the wrapper scripts of this project go away, because the scripts we build with flakes might still themselves read a environment during runtime.

Also, flakes are said to evaluate very quickly, which is a common complaint about Nix expressions, mine included.

I'm sure Nix 2.4, which will officially include flakes, will release sometime soon enough (it better!), and I'll likely move all my projects to use Nix Flakes then.

# Release<a id="sec-5"></a>

The "main" branch of the repository on GitHub has the latest released version of this code. There is currently no commitment to either forward or backward compatibility.

"user/shajra" branches are personal branches that may be force-pushed to. The "main" branch should not experience force-pushes and is recommended for general use.

# License<a id="sec-6"></a>

All files in this "shajra-provisioning" project are licensed under the terms of GPLv3 or (at your option) any later version.

Please see the [./COPYING.md](./COPYING.md) file for more details.

# Contribution<a id="sec-7"></a>

Feel free to file issues and submit pull requests with GitHub.

There is only one author to date, so the following copyright covers all files in this project:

Copyright © 2020 Sukant Hajra
