- [About this document](#sec-1)
- [Problems addressed by Nix](#sec-2)
  - [Complete build](#sec-2-1)
  - [Reliable build](#sec-2-2)
  - [Reliable deployment](#sec-2-3)
  - [Version conflicts](#sec-2-4)
  - [Polyglot programming](#sec-2-5)
  - [Distributed cache of builds](#sec-2-6)
- [Nix at a high level](#sec-3)
  - [Nix the package manager](#sec-3-1)
  - [Nix the build system](#sec-3-2)
  - [Nixpkgs](#sec-3-3)
- [Frustrations acknowledged](#sec-4)
  - [Nixpkgs takes time to learn](#sec-4-1)
  - [Confusion of stability](#sec-4-2)
    - [Nix 2.0 and the new `nix` command](#sec-4-2-1)
    - [Flakes as an experiment](#sec-4-2-2)
    - [Nix quick releases compete with stability](#sec-4-2-3)
  - [A few gaps in determinism](#sec-4-3)
- [Encouraging development with flakes](#sec-5)
  - [Limiting usage of experimental APIs](#sec-5-1)
  - [Keeping Nix version consistent](#sec-5-2)


# About this document<a id="sec-1"></a>

This document introduces the [Nix package manager](https://nixos.org/nix) and highlights some motivations to use Nix. It also covers the tradeoffs of using Nix and experimental features in Nix, such as *flakes*.

This document tries to capture enthusiasm while being honest about frustrations. Nix is a pioneer of an architectural approach that users will demand in the future. However, users need clear information up front where they are likely to face challenges.

# Problems addressed by Nix<a id="sec-2"></a>

The following sections cover various problems that Nix's architecture addresses.

## Complete build<a id="sec-2-1"></a>

When dealing with a new software project, wrangling dependencies can be a chore. Modern build systems for specific programming languages often don't manage system dependencies. For example, Python's `pip install` will download and install needed Python dependencies but may fail if the system doesn't provide shared libraries required for foreign function calls. Adding complexity, different operating systems have differing names for these system packages and install them with various commands (`apt`, `dnf`, etc.). This variation makes automation difficult. Consequently, many software projects only provide documentation as a surrogate for automation, which creates even more room for error.

## Reliable build<a id="sec-2-2"></a>

Some projects might have all the automation necessary for building, but due to subtle differences among systems, what builds on one system might not build on another.

For example, environment variables often can influence the behavior of the commands called by scripts. It's hard to lock down these variables on every system where something might be built.

## Reliable deployment<a id="sec-2-3"></a>

Once we've built some software and are ready to deploy it, it's not always obvious how to copy this built software to another system. For example, if the software dynamically links to system libraries, we need to know whether those libraries are on our target system.

## Version conflicts<a id="sec-2-4"></a>

Another complication we face is when an operating system only allows one installed version of a system library at a time. When this happens, we may be forced to make difficult choices if we need two programs requiring different system dependency versions.

## Polyglot programming<a id="sec-2-5"></a>

It can be tedious to synthesize libraries and programs from different language ecosystems to make a new program for a unified user experience. For example, the world of machine learning often requires the mixing of C/C++, Python, and even basic shell scripts. These hybrid applications tend to be fragile.

## Distributed cache of builds<a id="sec-2-6"></a>

Various build systems provide repositories for pre-built packages, which helps users save time by downloading packages instead of building them. We want this experience unified across all programming language ecosystems and system dependencies.

Note this is what traditional package managers like DNF and APT accomplish. However, there's an ergonomic difficulty in turning all software into standard Linux packages. Firstly, there are too many Linux distributions with too many package managers. Secondly, most package managers must adhere to policies for everything to work well together. For example, many distributions respect the [Filesystem Hierarchy Standard (FHS)](https://www.pathname.com/fhs/). Confusion around policies has led many developers to steer away from package managers and toward container-based technologies like Docker despite the overhead and drawbacks of containers.

# Nix at a high level<a id="sec-3"></a>

Nix addresses all the problems discussed above.

To build or install any project, we should be able to start with only the Nix package manager installed. No other library or system dependency should be required to be installed or configured.

Even if we have a library or system dependency installed, it shouldn't interfere with any build or installation we want to do. Nix builds and installs in its own meticulously sandboxed and controlled directories.

Our build should get everything we need, all the way down to the system-level dependencies, irrespective of which programming language the dependencies have been authored in. If anything has been pre-built, we should download a cached result.

Above and beyond the problems discussed above, Nix has a precisely deterministic build, generally guaranteeing reproducibility. If the package builds on one system, it should build on all systems, regardless of what's installed. Furthermore, multiple systems independently building the same package will often produce bit-for-bit identical builds.

Nix is also able to copy the transitive closure of a package's dependencies ergonomically from one system to another.

In broad strokes, Nix is a technology that falls into two categories:

-   package manager
-   build tool.

## Nix the package manager<a id="sec-3-1"></a>

As a package manager, Nix does what most package managers do. Nix provides a suite of command-line tools to search registries of known packages, as well as install and uninstall them.

Packages can provide both executables and plain files alike. Installation entails putting these files into a good location for the package manager and the user. Nix has an elegant way of storing everything under `/nix/store`, discussed more below.

Notably, the Nix package manager doesn't differentiate between system- and user-level installations. All packages end up in `/nix/store`. These packages are hermetic and can't conflict with one another. To save space, packages often share common elements via symlinks to other packages in `/nix/store`.

As a convenience, Nix has tools to help users put the executables provided by packages on their environment's `PATH`. This way, users don't have to find executables installed in `/nix/store`.

## Nix the build system<a id="sec-3-2"></a>

Nix combines the features of a package manager with those of a build tool. If a package or any of its dependent packages (including low-level system dependencies) aren't found in a *Nix substituter*, Nix builds them locally. Otherwise, Nix downloads pre-built packages cached in the substituter. We only need the Nix package manager and a network connection to build or download any package.

Every Nix package is specified by a *Nix expression*, written in a small programming language also called Nix. This expression specifies everything needed to build the package down to the system-level. These expressions are saved in files with a ".nix" extension.

Nix-friendly software will provide these Nix expressions as part of their source. If some software doesn't offer a Nix expression, you can always use an externally authored expression.

What makes Nix unique is that these expressions specify a way to build that's

-   precise
-   repeatable
-   guaranteed not to conflict with anything already installed

For some, it's easy to miss the degree to which Nix-built packages are precise and repeatable. Nix builds in highly controlled sandbox environments. If you build a package from a Nix expression on one system and then build the same expression on a system of the same architecture, you should get the same result. In many cases, the built artifacts will be identical bit-for-bit.

A system of thorough hashing accomplishes this degree of precision. In Nix, the dependencies needed to build packages are also themselves Nix packages. Every Nix expression has an associated hash calculated from the hashes of the package's dependencies and build instructions. When we change this dependency (even if only by a single bit), the hash for the Nix expression changes. This new hash cascades to a different calculated hash for any package relying on this dependency. But if nothing changes, all systems will calculate identical hashes.

The repeatability and precision of Nix form the basis of how substituters are trusted as caching services across the world. It also allows us to trust remote builds more easily without worrying about deviations in environment configuration.

Nix has a central substituter at <https://cache.nixos.org>, but there are third-party ones as well, like [Garnix](https://garnix.io) and [Cachix](https://cachix.org/). Before building a package, the hash for the package is calculated. If any configured substituter has a build for the hash, it's pulled down as a substitute. A certificate-based protocol is used to establish the trust of substituters. Between this protocol and the algorithm for calculating hashes in Nix, you can have confidence that a package pulled from a substituter will be equivalent to what you would have built locally.

Finally, all packages are stored in `/nix/store` by their hash. This simple scheme allows us to install multiple versions of the same package without conflicts. References to dependencies all point back to the desired version in `/nix/store` they need. Though Nix has not eliminated the risk of concurrently running different versions of the same program, at least the flexibility to do so is in the user's hands.

## Nixpkgs<a id="sec-3-3"></a>

Nix expressions help us create highly controlled environments to build packages precisely. However, Nix still calls the conventional build tools of various programming language ecosystems. Under the cover, Nix is ultimately a strictly controlled execution of Bash scripts orchestrating these tools.

The Nix community curates a [Git repository of Nix expressions called Nixpkgs](https://github.com/NixOS/nixpkgs). This repository has Nix expressions for all the packages provided by the [NixOS](https://nixos.org) operating system, as well as common Nix expressions used to build packages.

Most Nix expressions for packages will start with a snapshot of Nixpkgs as a dependency. This way, the complexity of shell scripting and calls to language-specific tooling can be kept mostly hidden away from Nix packaging expressions.

# Frustrations acknowledged<a id="sec-4"></a>

Having covered so many of Nix's strengths, it's good to be aware of some problems the Nix community is still working through.

## Nixpkgs takes time to learn<a id="sec-4-1"></a>

There are parts of Nix that are notably simple. For example, there's an elegance to the hashing calculation and how `/nix/store` is used. Furthermore, the Nix language has a small footprint, making learning Nix easier.

However, because of the complexity of all the programming language ecosystems, there are a *lot* of supporting libraries in Nixpkgs to understand. There are over two million lines of Nix in Nixpkgs, some auto-generated, increasing the odds of getting lost.

The [official Nixpkgs manual](https://nixos.org/nixpkgs/manual) only seems to cover a fraction of what package authors need to know. Invariably, people seem to master Nix by exploring the source code of Nixpkgs, supplemented by example projects for reference. You can get surprisingly far mimicking code you find in Nixpkgs that packages something similar to what you have in front of you. But understanding what's going on so you avoid simple mistakes can take some time.

Various people have attempted to fill the gap with documentation and tutorials. Even this document you're reading now is one such attempt. However, we're missing a searchable index of all the critical functions in Nixpkgs for people to explore. Something as simple as parsed [docstrings](https://en.wikipedia.org/wiki/Docstring) as an extension of the Nix language would go a long way, which would be far easier to implement than something more involved, like a type system for the Nix language.

## Confusion of stability<a id="sec-4-2"></a>

The Nix community seems divided into the following camps:

-   those who want new features and fixes to known grievances
-   those who want stable systems based on Nix in industrial settings.

These groups don't need to be at odds. Unfortunately, Nix has released experimental features in a way that has created confusion about how to build stable systems with Nix.

### Nix 2.0 and the new `nix` command<a id="sec-4-2-1"></a>

An early complaint of Nix was the non-intuitiveness of Nix's original assortment of command-line tools. To address this, Nix 2.0 introduced a unifying CLI tool called `nix`. Despite appreciable improvements in user experience, the newer `nix` command has taken some time to get enough functionality to replace the older tools (`nix-build`, `nix-shell`, `nix-store`, etc.). For a while, it's ended up yet another tool to learn.

If you look at the manpage for the latest release of `nix`, there's a clear warning at the top:

> Warning: This program is experimental, and its interface is subject to change.

This warning has been there since 2018, when Nix 2.0 was released.

However, `nix repl` is the only way to get to a [REPL session](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop) in Nix, which is an important tool for any programming language. The previous tool providing a REPL (`nix-repl`) has been removed from Nixpkgs.

Because something as basic as the REPL is only available with an experimental feature, the Nix community is confusing guidance on using Nix with some stability.

Eventually, with the release of Nix 2.4, experimental features were turned into flags that needed to be explicitly enabled by users. One of these flags was `nix-command`, which now gates users from any subcommand of `nix` beyond `nix repl`. However, because so many users already use the new `nix` command, the experimental `nix-command` feature is enabled by default if no experimental features have been configured explicitly.

In other words, Nix ships with an experimental feature enabled by default.

Enabling the new `nix` command by default almost indicates it isn't too unstable. However, Nix 2.4 did indeed change the API of `nix` subcommands. Industrial users scripting against `nix` had to figure out the appropriate changes.

In practice, the `nix` subcommands are relatively reliable. They are well-written and functionally robust. However, the core maintainers reserve the right to change input parameterization and output formatting without bumping a major version number.

They communicate this risk only with the warning atop the manpage, which most users have been training one another to ignore.

### Flakes as an experiment<a id="sec-4-2-2"></a>

Though Nix expressions have an incredible potential to be precise and reproducible, there have always been some backdoors to break the reliability of builds. For example, Nix expressions have the potential to evaluate differently depending on the setting of some environment variables like `NIX_PATH`.

The motivation for these relaxations of determinism has been a quick way to let personal computing users have a convenient way to manage their environments. Some people are careful to avoid accidentally having non-deterministic builds. Still, accidents have occurred frequently enough for the community to want better. It's frustrating to have a broken build because someone else set an environment variable incorrectly.

Nix 2.4 corrected this by introducing an experimental feature called *flakes*. Flakes provide an ergonomic way to manage build environments, with more guarantees of determinism. A nice benefit of strictly enforced determinism is the ability to cache evaluations of Nix expressions, which can be expensive to compute.

All this is generally good news. Flakes address problems that industrial users of Nix have long had to deal with.

However, flakes are an experimental feature that users need to enable explicitly. Similar to the `nix` command, across versions, the inputs and outputs of flake-related subcommands might change slightly. Furthermore, the hashes computed by flakes can change as well. Such changes have already happened.

On top of this, because flakes are experimental, documentation of flakes is fractured in the official documentation. It almost seems like the Nix developers are delaying proper documentation until there's a declaration of stability. A preferred alternative would be developing documentation concurrently with the implementation, using the documentation's comprehensibility to inform the software's design. Good opportunities for redesign can be found in features that prove difficult to explain.

All this puts industrial Nix users in an annoying place. Not using flakes and instead of coaching coworkers and customers on how to use Nix safely

-   increases the likelihood of defects as people make honest mistakes
-   reduces the likelihood of adoption because people get frustrated with poor ergonomics and difficulty understanding nuances and corner cases.

However, if industrial users move to flakes to address these problems, we have the following problems:

-   we have to be ready for the flakes API to change, as it's technically experimental
-   we have to accept some added training hurdles since the documentation of flakes is tucked behind documentation of non-flake usage.

### Nix quick releases compete with stability<a id="sec-4-2-3"></a>

The latest major version of the Nix package manager is currently Nix 2.25.3, but NixOS 24.11, the latest stable release of NixOS, uses Nix 2.24.11. NixOS is the primary way the Nix package manager gets used in the field. Far fewer users install Nix as a package manager atop another operating system. From a community perspective it makes sense to consider Nix 2.24.11 the stable release of the package manager. This version gets the most scrutiny and critical bug fixes.

As mentioned above, there are strong reasons to use still-experimental features, particularly flakes. However, APIs and calculated hashes change too frequently in experimental features from version-to-version. By sticking with the version used in NixOS, we get less breaking changes. For example, the [flake.lock](../flake.lock) file included with this project has calculated hashes for dependencies. These hashes were computed with Nix 2.24.11, and could change with later versions.

For these reasons, the [installation guide included with this project](nix-installation.md) recommends installing Nix 2.24.11, rather than the latest official release.

## A few gaps in determinism<a id="sec-4-3"></a>

Nix offers world-class build determinism, especially with flakes. But it's important to understand that this determinism is not infallible. To date, no build system can claim to provide flawless determinism.

Consider a hypothetical compiler that can auto-detect that a build machine has many cores, and enables an optimization upon detection incompatible with machines with fewer cores. While Nix will generate different hashes if the platform architecture changes, say from X86 to ARM, it will not consider a machine with many cores different from one with fewer. So our example optimizing compiler could cause a frustrating problem. A local build on a machine with few cores may work as expected. But if a cache had a optimized build from a machine with many cores, it would be pulled down for the same hash, as a substitute for a local build. This optimization would lead to defects running on the wrong machine.

Note that in general, we benefit from downloading and running packages built on more powerful machines, and in almost all cases, the clever optimizations of various compilers are portable.

Lapses in determinism caused by Nix expressions in Nixpkgs are generally considered defects and handled through GitHub issues. Some may argue that this is the best that we can do.

Most people will never encounter such corner cases in practice, but it's important to understand the limitations of an otherwise extremely strong guarantee of determinism.

# Encouraging development with flakes<a id="sec-5"></a>

This project encourages the development of Nix projects using flakes. The benefits seem to outweigh the risks of instability. This choice is not made lightly, and this document is an exercise of due diligence to inform users of compromises.

Flakes are the future in Nix. They significantly address prior pains. Furthermore, enough people worldwide are using them that we have some confidence that the Nix commands are robust.

Using Nix with flakes should lead to a mostly pleasant experience. There are some things to look out for, though.

## Limiting usage of experimental APIs<a id="sec-5-1"></a>

If you write scripts that call `nix` commands or use flakes, they may break slightly if you upgrade to a newer version of Nix. For example, the formatting of standard output for a command might change.

By calling `nix` with a few extra arguments `--extra-experimental-features 'nix-command flakes'` we can access flakes commands for single invocations without enabling flakes globally. You can even make an alias for your shell that might look like the following:

```sh
alias nix-flakes = nix --extra-experimental-features 'nix-command flakes'
```

This way, there's less to type interactively. Just don't script against this command, so there's no worry of scripts breaking due to experimental features.

## Keeping Nix version consistent<a id="sec-5-2"></a>

You may find that you need to pin the version of Nix to the same version for all your machines (because hashes could change between versions, which are saved in `flake.lock` files).
