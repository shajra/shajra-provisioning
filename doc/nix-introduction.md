- [About this document](#sec-1)
- [Problems addressed by Nix](#sec-2)
  - [Managed build](#sec-2-1)
  - [Reliable build](#sec-2-2)
  - [Reliable deployment](#sec-2-3)
  - [Version conflicts](#sec-2-4)
  - [Polyglot programming](#sec-2-5)
  - [Complete distributed cache of builds](#sec-2-6)
- [Nix at a high level](#sec-3)
  - [Nix the package mangager](#sec-3-1)
  - [Nix the build system](#sec-3-2)
  - [Nixpkgs](#sec-3-3)
- [Frustrations acknowledged](#sec-4)
  - [Nixpkgs takes time to learn](#sec-4-1)
  - [Confusion of stability](#sec-4-2)
    - [Nix 2.0 and the new `nix` command](#sec-4-2-1)
    - [Flakes as an experiment](#sec-4-2-2)
- [Encouraging development with flakes](#sec-5)
- [Helping non-flakes users](#sec-6)
- [Documenting an end user experience](#sec-7)


# About this document<a id="sec-1"></a>

This document introduces the [Nix package manager](https://nixos.org/nix) and highlights some motivations to use Nix. It also covers tradeoffs not only of using Nix, but experimental features in Nix such as one called *flakes*.

This document tries to capture enthusiasm while being honest about frustrations. Nix is amazing, and a clear pioneer of an architectural approach that users will come to demand in the future. However, users need clear information up front where they are likely to face challenges.

# Problems addressed by Nix<a id="sec-2"></a>

The following sections cover various problems that Nix's architecture addresses.

## Managed build<a id="sec-2-1"></a>

When dealing with a new software project, wrangling dependencies can be a chore. Modern build systems for specific programming languages often don't manage system dependencies. For example, Python's `pip install` will download and install needed Python dependencies, but may fail if the system doesn't provide C shared libraries needed for foreign function calls. Complicating matters, different operating systems have different names for these system packages and install them with different commands (`apt`, `dnf`, etc.). This makes automation difficult. Consequently, many software projects only provide documentation as a surrogate for automation, which creates even more room for error.

## Reliable build<a id="sec-2-2"></a>

Some projects might have all the automation necessary for building, but due to subtle differences among systems, what builds on one system might not build on another.

For example, environment variables often can influence the behavior of the commands called by scripts. It's hard to lock down these variables on every system where something might be built.

## Reliable deployment<a id="sec-2-3"></a>

Once we've built some software and are ready to deploy it, it's not always obvious how to copy this built software to another system. For example, if the software dynamically links to system libraries, we need to know whether those libraries are on the system we intend to copy to.

## Version conflicts<a id="sec-2-4"></a>

Another complication we face is when an operating system only allows one version of a system library to be installed at a time. When this happens, we have to make difficult choices if we need two programs that require different versions of a system dependency.

## Polyglot programming<a id="sec-2-5"></a>

It's also tedious to synthesize libraries and programs from different language ecosystems to make a new program for a unified user experience. For example, the world of machine learning programming often requires the mixing C/C++, Python, and even basic shell scripts. These hybrid applications have a tendency to be fragile.

## Complete distributed cache of builds<a id="sec-2-6"></a>

Various build systems provide repositories for pre-built packages, which helps users save time by downloading packages instead of building them. What we really want is this experience, but unified across all programming language ecosystems and system dependencies.

Note, this is what traditional package managers like DNF and APT accomplish. But there's an ergonomic difficulty to turning all software into standard Linux packages. To start, there are too many Linux distributions with too many package managers. Secondly, most of the package managers require adherence to a set of policies for everything to work well together. For example, many distributions respect the [Filesystem Hierarchy Standard (FHS)](https://www.pathname.com/fhs/). Confusion around policies have led many developers to steer away from package managers and towards container-based technologies like Docker, despite the overhead and drawbacks of containers.

# Nix at a high level<a id="sec-3"></a>

Nix addresses all the problems discussed above.

To build or install any project, we should be able to start with only the Nix package manager installed. No other library or system dependency should be required to be installed or configured.

Even if we have a library or system dependency installed, it shouldn't interfere with any build or installation we want to do.

Our build should get everything we need, all the way down to system-level dependencies, irrespective of which programming language the dependencies has been authored in. If anything has been pre-built, we should download a cached result.

Above and beyond the problems discussed above, Nix has a precisely deterministic build, broadly guaranteeing reproducibility. If the package builds on one system, it should build on all systems, irrespective of what's installed or not. Furthermore, multiple systems building the same package independently will often produce bit-for-bit identical builds.

Nix also is able to conveniently copy the transitive closure of a package all its dependencies ergonomically from one system to another.

In broad strokes, Nix is a technology that falls into two categories:

-   package manager
-   build tool.

## Nix the package mangager<a id="sec-3-1"></a>

As a package manager, Nix does what most package managers do. Nix provides a suite of command-line tools to search registries of known packages, as well as install and uninstall them.

Packages can provide both executables and plain files alike. Installation just entails putting these files into a good location for both the package manager and the user. Nix has an elegant way of storing everything under `/nix/store`, discussed more below.

Importantly, the Nix package manager doesn't differentiate between system-level installations and user-level installations. All builds and installations are by nature hermetic and can't conflict with one another.

As a convenience, Nix has tools to help users put the executables provided by packages provided on their environment's `PATH`. This way, users don't have to deal with finding executables to call installed in `/nix/store`.

## Nix the build system<a id="sec-3-2"></a>

Nix conjoins the features of a package manager with those of a build tool. If a package or any of its dependencies (including low-level system dependencies) aren't found in a *Nix substituter*, they are built locally. Otherwise, the pre-built package and dependencies cached in the Nix substituter are downloaded rather than built. All we need to build or download any package is the Nix package manager and a network connection.

Every Nix package is specified by a *Nix expression*, written in a small programming language also called Nix. This expression specifies everything needed to build the package down to the system-level. These expressions are saved in files with a ".nix" extension.

Some software provides these Nix expressions alongside as part of their source. If some software doesn't provide a Nix expression, you can always use an externally authored expression.

What makes Nix special is that these expressions specify a way to build that's

-   precise
-   repeatable
-   guaranteed not to conflict with anything already installed

For some, it's easy to miss the degree to which Nix-built packages are precise and repeatable. If you build a package from a Nix expression on one system, and then build the same expression on a system of the same architecture, you should get the same result. In most cases, the built artifacts will be identical bit-for-bit.

This degree of precision is accomplished by a system of thorough hashing. In Nix, the dependencies needed to build packages are also themselves Nix packages. Every Nix expression has an associated hash that is calculated from the hashes of package's dependencies and build instructions. When we change this dependency (even if only by a single bit), the hash for the Nix expression changes. This cascades to a different calculated hash for any package relying on this dependency. But if nothing changes, the same hashes will be calculated on all systems.

The repeatability and precision of Nix forms the basis of how substituters are trusted as caching services across the world. It also allows us to trust remote builds more easily, without worrying about deviations of environment configuration.

Nix has central a substituter at <https://cache.nixos.org>, but there are third-party ones as well, like [Cachix](https://cachix.org/). Before building a package, the hash for the package is calculated. If any configured substituter has a build for the hash, it's pulled down as a substitute. A certificate-based protocol is used to establish trust of substituters. Between this protocol, and the algorithm for calculating hashes in Nix, you can have confidence that a package pulled from a substituter will be identical to what you would have built locally.

Finally, all packages are stored in `/nix/store` by their hash. This simple scheme allows us to have multiple versions of the same package installed with no conflicts. References to dependencies all point back to the desired version in `/nix/store` they need. This is not to say that running multiple programs concurrently based on different versions can't cause problems, but at least the flexibility to do so is in the user's hands.

## Nixpkgs<a id="sec-3-3"></a>

Nix expressions help us create extremely controlled environments within which we can build packages precisely. However, Nix still calls the conventional build tools of various programming language ecosystems. Under the cover, Nix is ultimately an extremely controlled execution of Bash scripts orchestrating these tools.

To keep the Nix expressions for each package concise, the Nix community curates a [Git repository of Nix expressions called Nixpkgs](https://github.com/NixOS/nixpkgs). Most Nix expressions for packages will start with a snapshot of Nixpkgs as a dependency, which provides library support to help keep Nix expressions compact.

This way, the complexity of shell scripting and calls to language-specific tooling can be kept largely hidden away from general Nix authors. The Nix language lets us work with packages from any language ecosystem in a uniform way.

# Frustrations acknowledged<a id="sec-4"></a>

Having covered so many of Nix's strengths, it's important to be aware of some problems the Nix community is still working through.

## Nixpkgs takes time to learn<a id="sec-4-1"></a>

There are parts of Nix that are extremely simple. For example, there's an elegance to the hashing calculation and how `/nix/store` is used. Furthermore the Nix language itself has a small footprint, which eases learning it.

However, because of the complexity of all the programming language ecosystems out there, there are a *lot* of supporting libraries in Nixpkgs to understand. There's over two million lines of Nix in Nixpkgs, some auto-generated, increasing the odds of getting lost in it.

The [official Nixpkgs manual](https://nixos.org/nixpkgs/manual) only seems to cover a fraction of what package authors really need to know. Invariably, people seem to master Nix by exploring the source code of Nixpkgs, supplemented by example projects for reference. You can get surprisingly far mimicking code you find in Nixpkgs that packages something similar to what you have in front of you. But understanding what's actually going on so you avoid simple mistakes can take some time.

Various people have attempted to fill the gap with documentation and tutorials. Even this document you're reading now is one such attempt. However, we're missing searchable index of all the critical functions in Nixpkgs for people to explore. Something as simple as parsed [docstrings](https://en.wikipedia.org/wiki/Docstring) as an extension of the Nix language would go a long way, which would be far easier to implement than something more involved like a type system for the Nix language.

## Confusion of stability<a id="sec-4-2"></a>

The Nix community seems divided into the following camps:

-   those who want new features and fixes to known grievances
-   those who want stable systems based on Nix in industrial settings.

It's not necessary for these groups to be at odds. Unfortunately, Nix has released new experimental features in a way that has created confusion of how to build stable systems with Nix.

### Nix 2.0 and the new `nix` command<a id="sec-4-2-1"></a>

An early complaint of Nix was the non-intuitiveness of Nix's original assortment of command-line tools. To address this, Nix 2.0 introduced a unifying tool called `nix`. Despite appreciable improvements in user experience, the newer `nix` command has taken some time for it to get enough functionality to actually replace the older tools (`nix-build`, `nix-shell`, `nix-store`, etc.). For a while, it's ended up yet another tool to learn.

If you look at the manpage for `nix` there's a clear warning at the top:

> Warning: This program is experimental and its interface is subject to change.

This warning has been there since 2018 when Nix 2.0 released.

However, `nix repl` is the only way to get to a [REPL session](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop) in Nix, which is an important tool for any programming language. The previous tool providing a REPL (`nix-repl`) has been removed from Nixpkgs.

This means that technically, the community is strongly encouraging, if not forcing, users to use an experimental tool and providing little guidance on how to use Nix with some assurance of stability. This is important for industrial users who script solutions against the `nix` command-line tools.

Eventually, with the release of Nix 2.4, experimental features were turned into flags that needed to be explicitly enabled by users. One of these flags was `nix-command`, which now gates users from any subcommand of `nix` beyond `nix repl`. However, because so many users were already using the new `nix` command, the experimental `nix-command` feature is enabled by default if no experimental features have been configured explicitly.

In other words, Nix ships with an experimental feature enabled by default.

This almost indicates that the new `nix` command isn't too unstable. Except, Nix 2.4 did indeed change the API of `nix` subcommands.

In practice, the `nix` subcommands are relatively reliable. They are well-written and functionally robust. But the core maintainers are reserving the right to change input parameterization and output formatting.

They communicate this risk only with the warning atop the manpage, which most users have been training one another to ignore.

### Flakes as an experiment<a id="sec-4-2-2"></a>

Though Nix expressions have an incredible potential to be precise and reproducible, there has always been some backdoors to break the reliability of builds. For example, Nix expressions have the potential to evaluate differently depending on the setting of some environment variables like `NIX_PATH`.

The motivation for these relaxations of determinism has been to have a quick way to let personal computing users have a convenient way to manage their environments. Some people are careful to avoid accidentally having non-deterministic builds. Still, accidents have occurred frequently enough for the community to want better. It's frustrating to have a broken build because someone else set an environment variable incorrectly.

Nix 2.4 corrected for this by introducing an experimental feature called *flakes*. Users still have a largely ergonomic way to manage their environments, but builds are more strictly deterministic. Determinism is a large reason many turn to Nix in the first place. A nice benefit of strictly enforced determinism is the ability to cache evaluations of Nix expressions, which can be expensive to compute.

All this is generally good news. Flakes address problems that industrial users of Nix have long had to deal with.

However, flakes are an experimental feature that users need to explicitly enable. Similar to the `nix` command, the inputs and outputs of flake-related subcommands might change slightly. Such changes have already happened.

On top of this, because flakes are experimental, documentation of flakes is extremely fractured in the official documentation. It almost seems like the Nix developers are delaying proper documentation until there's a declaration of stability. A preferred alternative would be developing documentation more concurrently with the implementation, using the comprehensibility of the documentation to inform the design of the software. Features that are too hard to explain expose good opportunities for redesign.

All this puts industrial Nix users in an annoying place. Not using flakes and instead coaching coworkers and customers on how to use Nix safely

-   increases the likelihood of defects as people make honest mistakes
-   reduces the likelihood of adoption, because people get frustrated with poor ergonomics and difficulty understanding nuances and corner cases.

However, if industrial users move to flakes to address these problems we have the following problems:

-   we have to be ready for the flakes API to change as it's technically experimental
-   we have to accept some added training hurdles since documentation of flakes is tucked behind documentation of non-flakes usage.

# Encouraging development with flakes<a id="sec-5"></a>

This project encourages the development of Nix projects using flakes. The benefits seem to outweigh the risks of instability. This is not a choice made lightly, and this document is an exercise of due diligence to inform users of compromises.

Flakes are absolutely the future in Nix. They significantly address prior pains. Furthermore, enough people across the world are using them that we have some confidence that the Nix commands are reliable as implemented. The core contributors have just been very slow to commit to the user interfaces and experience.

There might be documentation and training hurdles with flakes, but it's not actually much better not using flakes. This is why this project includes documentation and guides on Nix itself.

It's also important to keep the risks of using experimental features in perspective. Industrial users are more likely to script heavily against `nix` commands than personal users. Upgrading anything risks small breaks to address. For some industrial users, such breaks are insufferable in aggregate, even if manageable individually. This leads to a desire to only use software that has been officially released as stable and supported. Still, if you've read documents like this one, and feel the history and risks have been well explained, using flakes might be the best option, even with industrial scripting that might break.

Usage of flakes outside scripting has almost no risk at all. By calling `nix` with a few extra arguments `--extra-experimental-features 'nix-command flakes'` we can access flakes commands for single invocations, without needing to enable flakes globally. You can even make an alias for your shell that might look like the following:

```sh
alias nix-flakes = nix --extra-experimental-features 'nix-command flakes'
```

This way there's less to type interactively. Just don't script against this command, and there's no worry of scripts breaking if the flakes API changes.

# Helping non-flakes users<a id="sec-6"></a>

A few users make work in organizations or contribute to projects that disallow experimental features such as flakes.

To buffer this compromise, this project uses and encourages the use of the [flake-compat](https://github.com/edolstra/flake-compat) project, which enables an end user who has opted not to enable flakes to at least access the flake's contents, packages or otherwise.

With flake-compat, end users will have a normal (non-flake) Nix expression they can evaluate. However, since dependencies are managed with flakes, the project maintainer must have flakes enabled to manage dependencies (for example, updating to the latest dependencies with `nix flake update`).

# Documenting an end user experience<a id="sec-7"></a>

To deal with the transition of the Nix community to flake, this project provides two user guides:

-   [Nix Usage with Flakes (Recommended) ](nix-usage-flakes.md)
-   [Nix Usage without Flakes](nix-usage-noflakes.md)

Links generally steer users to the recommended guide, which then links users to the non-flakes guide if they have the interest or need.

The non-flakes guide intentionally avoids commands like `nix-shell` and `nix-channel`. These commands lead users to setting the `NIX_PATH` environment variable, which can lead to unreliable builds.

Though this guide avoid the `flakes` experimental feature, it still invites end users to use the experimental `nix-command` to get the following subcommands:

-   `nix search`
-   `nix shell`
-   `nix run`

In general, the non-flakes guide only explains usage of experimental `nix` subcommands when there exist no other alternatives, or when the alternatives are considered worse for new users.

`nix search` simply has no good alternative within the set of non-experimental Nix tools, but it's too useful to not tell users about. Again, this is an example of the Nix community leading users to experimental features.

`nix shell` and `nix run` are shown as improved alternatives to `nix-shell`. `nix-shell` is a complicated tool that has been historically used for a lot of different purposes:

-   debugging the build environments of packages
-   creating a developer environment for a package (`nix develop` does this better, but for only for flakes)
-   entering a shell with Nix-build executables on the path (`nix shell` does this better)
-   running arbitrary commands with Nix-build executables on the path (`nix run` does this better)

To cover all of these scenarios, `nix-shell` became so complex it is hard to explain to new users. `nix-shell` is really only best for debugging builds, which is beyond the scope of the documentation provided by this project.
