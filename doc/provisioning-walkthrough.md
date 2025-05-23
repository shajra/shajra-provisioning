- [About this document](#sec-1)
- [Recommended prior reading](#sec-2)
- [High-level overview](#sec-3)
  - [Building packages](#sec-3-1)
  - [Configuration modules](#sec-3-2)
- [Following along in code](#sec-4)
- [This project's module convention](#sec-5)
- [Nixpkgs instances used for configuration](#sec-6)


# About this document<a id="sec-1"></a>

This document helps explain the code of this project for provisioning my personal machines, which rely on the convergence of a variety of third-party software and techniques.

You *very certainly* do not want to try to configure your machines precisely like mine. This is my personal repository, with hard-coded things like my name and emails.

This document should help explain what it takes to modify this project, or mimic its approach, to suit the provisioning of your own machines.

# Recommended prior reading<a id="sec-2"></a>

In case there are concepts you don't know, the following provided guides may be helpful to read before continuing with this walk-through.

-   [Introduction to Nix and motivations to use it](nix-introduction.md)
-   [Nix installation and configuration guide](nix-installation.md)
-   [Nix end user guide](nix-usage-flakes.md)
-   [Introduction to the Nix programming language](nix-language.md)
-   [Nix-project's development guide](https://github.com/shajra/nix-project/blob/main/doc/project-developing.md), which explains concepts and tools used by this project including
    -   Nix's experimental *flakes* feature
    -   [Flake-parts](https://github.com/hercules-ci/flake-parts), a library to ease flake authorship
    -   authoring packages using Nixpkgs's `callPackage` function
    -   assembling packages from one another with Nix *overlays*.

It's a lot of material, but it relieves the burden of explaining as much within this walk-through.

# High-level overview<a id="sec-3"></a>

## Building packages<a id="sec-3-1"></a>

A lot of configuring a system involves building and installing packages, whether at a system-level or user-level profile. In Nix, many packages are prebuilt and cached in a public substituter, so our build step becomes merely downloading the prebuilt package.

This project builds packages in two ways. The first way is with standard Nixpkgs infrastructure as documented in the [Nixpkgs manual](https://nixos.org/nixpkgs/manual). The second uses [Haskell.nix](https://github.com/input-output-hk/haskell.nix) to build software written in the Haskell programming language.

For the Nixpkgs infrastructure, packages are built against a specific version of Nixpkgs. Some versions of Nixpkgs are considered stable, others unstable. Which version we consider stable or unstable might vary with platform. We might want all the packages installed at the system-level to be from a stable set. But we might be willing to try out unstable packages at the user-level. Even then, we might want to make exceptions for specific packages.

To deal with this complexity, this project builds out multiple versions of Nixpkgs. Each instance is further extended by an overlay that augments them with custom packages. The Haskell.nix infrastructure independently uses its own instance of Nixpkgs.

Nixpkgs is gigantic, with many thousands of packages. On top of that, we now have multiple instances of Nixpkgs to choose packages from. Fortunately, because Nix is lazily evaluated, we only have to build what we need. So, our next step is to select packages.

The selection of packages is done within the top-level [`packages.nix`](../packages.nix) file. The Nix expression in this file is a function. The input of this function passes in a small API from both the Nixpkgs and Haskell.nix infrastructure to select out packages and group them into sets, later used in configuration modules.

By building selected packages separately from system-level or home directory configuration, we can easily integrate the build of these packages into CI. Some CI services like GitHub Actions have limits on both compute time and disk space per job, so it helps break up package sets into chunks.

## Configuration modules<a id="sec-3-2"></a>

Whether setting system-level configuration with `nixos-rebuild` or `darwin-rebuild`, or user-level configuration with `home-manager`, all of these tools are configured with the NixOS-style configuration module infrastructure.

Nix's configuration module infrastructure originally targeted the configuration of just NixOS. The goal of NixOS modules is to make it easy to toggle features on/off with lightweight declarative configuration options. However, the module infrastructure is general enough to be used as a configuration system for any project, not just NixOS, but Nix-Darawin and Home Manager as well.

These upstream projects all maintain many modules for various tools and configurations. The code in these modules does all the complicated tasks of installing the correct packages and weaving everything into configuration files. These modules also bake in a lot of helpful verification of our configuration. Using upstream modules simplifies our configuration considerably.

The following is useful documentation on configuration for various NixOS-style modules:

-   [NixOS's configuration syntax](https://nixos.org/manual/nixos/stable/index.html#sec-configuration-file)
-   [NixOS's configuration options](https://nixos.org/manual/nixos/stable/index.html#ch-configuration)
-   [Nix-Darwin's configuration options](https://daiderd.com/nix-darwin/manual/index.html#sec-options)
-   [Home Manager's configuration options](https://nix-community.github.io/home-manager/options.html)

Read the NixOS documentation on configuration syntax first, even if you aren't running NixOS. It gives details on the syntax of NixOS-style modules, which you'll find under both [`machines/`](../machines) and [`home/`](../home) in this project.

Once you understand the format of NixOS-style modules and how they work, you can look at the rest of the documentation for all the options available to you by all the modules provided by upstream projects.

Also, once you've provisioned a system for the first time, it will have some man pages you can call instead of looking up documentation online. To get details on system-level configuration for NixOS or MacOS/Nix-Darwin call

```sh
man configuration.nix
```

For details on Home Manager configuration call

```sh
man home-configuration.nix
```

# Following along in code<a id="sec-4"></a>

The following table gives an overview of this project's layout and includes pointers of where to change code to suit your needs:

| File/Directory              | Change    | Description                                                |
|--------------------------- |--------- |---------------------------------------------------------- |
| `build`                     |           | Nix code to build packages                                 |
| `build/nixpkgs/overlays`    | Yes       | Custom Nixpkgs overlays                                    |
| `build/nixpkgs/packages`    | Yes       | Custom packages                                            |
| `config.nix`                | Yes       | Top-level project configuration                            |
| `darwin-rebuild`            |           | Script to provision MacOS system-level                     |
| `flake.lock`                | Generated | Generated file locking dependencies                        |
| `flake.nix`                 | Yes       | Declares the OS and home directory configurations          |
| `home-manager`              |           | Script to configure a home directory                       |
| `home/modules`              | Yes       | Modules to assist building up home directory configuration |
| `home/target/$HOSTNAME`     | Yes       | Home directory configuration by target machine hostname    |
| `machines/modules`          | Yes       | Modules to assist building up system-level configuration   |
| `machines/target/$HOSTNAME` | Yes       | System-level configuration by target machine hostname      |
| `nixos-rebuild`             |           | Script to provision NixOS system-level                     |
| `packages.nix`              | Yes       | Selected packages for provisioning                         |

The entrypoint for all flake-enabled projects is a file called [flake.nix](../flake.nix) at the project's root.

The flake has an output attribute of `overlays.default` set to the overlay that starts with a bootstrap Nixpkgs instance and builds out an attribute tree of lots of packages, some configuration, and helpful functions. This “build” is output by the flake as `legacyPackages`.

The build attribute tree looks something like the following:

-   infra
    -   np: an API for building packages using Nixpkgs
        -   nixpkgs: versions of Nixpkgs with our custom overlay applied
            -   home: version chosen in `config.nix` for home directory packages
            -   system: version chosen in `config.nix` for system-level packages
            -   stable: version considered stable for the platform
            -   unstable: nixpkgs-unstable
    -   hn: an API for building packages using Haskell.nix
-   config: top-level configuration
-   pkgs: attribute tree of selected packages, organized by category
    -   sets: grouped in attribute sets
    -   lists: grouped in lists
-   ci: symlink trees of selected packages for CI

Notice that this build is different for each system platform we build for.

This build is then made available as a custom parameter `build` for all configuration modules.

In `flake.nix`, we see all the configurations for each of our machines under the following flake outputs attributes:

-   `nixosConfigurations.<hostname>`, for NixOS system-level configuration
-   `darwinConfigurations.<hostname>`, for Darwin system-level configuration
-   `homeConfigurations.<name>`, for home directory configuration

Each configuration specifies

-   the system architecture to target for building packages and configuration
-   a path to the top-level Nix module for configuration.

We use NixOS library calls from Nixpkgs to build `nixConfigurations`. Similarly, we use library calls from the `nix-darwin` and `home-manager` flake inputs to build out `darwinConfigurations` and `homeConfiguration` configurations respectively. To keep `flake.nix` clean, the complications of these library calls are factored out into [build/configurations.nix](../build/configurations.nix). This is also where the `build` parameter is set.

# This project's module convention<a id="sec-5"></a>

Each wrapper script for `nixos-rebuild`, `darwin-rebuild`, and `home-manager` by default guesses what module to load by inspecting the hostname of the caller's computer. Each script provides a `--flake` switch to override this inspection.

This is why the directories under `home/target` and `machines/target` have funny names (they are the hostnames of my machines).

It's unlikely that your machines have hostnames that match mine, so you can just create new configurations in sibling directories named after your machines.

As common among Nix users, the system-level configuration is kept minimal to the hardware configuration and services that either can't or shouldn't be run at the user level. Home Manager manages everything else in a home directory.

Because there's more user-level configuration, I take advantage of the modularity of NixOS-style configuration modules to factor it into reusable units. Technically, `home/target` and `home/modules` both have the same NixOS-style modules. The ones under `home/target` are just entry points that import modules from `home/modules`.

You may notice that this project uses a more inheritance-style approach for user-level modules, rather than a mixin-style approach. The inheritance style leads to less importing but more coupling of configuration that might technically be more separable. This is just a personal choice. Creating quality mixins requires thinking more about coupling, leading to more work than seemed worth it for my few computers. I may use a hybrid of inheritance- and mixin-style as I add more computers.

# Nixpkgs instances used for configuration<a id="sec-6"></a>

In all my configuration modules, you'll notice that packages are generally selected from the package lists in the custom `build` parameter. These are the packages selected in the top-level `packages.nix`.

However, all of the configuration modules also provide a `pkgs` parameter. This parameter is the default Nixpkgs instance used for configuration. Packages could also be selected from here, but the version of this Nixpkgs instance will vary by platform and whether the configuration is system-level or home-level.

Just be aware that the version of Nixpkgs you'll get will vary as follows:

| Configuration | Operating system | Nixpkgs version of `pkgs` |
|------------- |---------------- |------------------------- |
| NixOS         | NixOS            | `nixos-25.05`             |
| Darwin        | MacOS            | `nixpkgs-25.05-darwin`    |
| Home          | not MacOS        | `nixpkgs-unstable`        |
| Home          | MacOS            | `nixpkgs-unstable`        |

You can change this in [`config.nix`](../config.nix) if you like. The code doesn't vary this selection by machine, just platform. I just haven't needed anything fancier yet.

In case you noticed, all these versions of Nixpkgs are not inputs of the flake. The `nix-project` flake input locks them down. Using `nix-project` to specify my instances of Nixpkgs allows me to keep all projects based on Nix-project at the identical versions of Nixpkgs. The benefits of doing this are subtle (reduced disk consumption in `/nix/store`) and subjective.
