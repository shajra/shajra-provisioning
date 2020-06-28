- [How this project uses Nix](#sec-1)
- [Motivation to use Nix](#sec-2)
- [Installation and Setup](#sec-3)
  - [Nix package manager setup](#sec-3-1)
  - [Cache setup](#sec-3-2)
- [Working with Nix](#sec-4)
  - [Searching Nix files](#sec-4-1)
  - [Building Nix expressions](#sec-4-2)
  - [Running commands](#sec-4-3)
  - [Installing and uninstalling programs](#sec-4-4)
  - [Garbage collection](#sec-4-5)
  - [Understanding derivations](#nix-drv)
  - [Lazy evaluation](#sec-4-7)
- [Next Steps](#sec-5)


# How this project uses Nix<a id="sec-1"></a>

This project uses the [Nix package manager](https://nixos.org/nix) to download all necessary dependencies and build everything from source.

Because Nix is more than a build system, notably a full package manager, the final build is actually a Nix package that you can install with the Nix package manager if you like.

Builds from this project are cached at [Cachix](https://cachix.org), a service that caches pre-built Nix packages. If you don't want to wait for a full local build, setting up Cachix is recommended.

The various files with a ".nix" extension are Nix files, each of which contains an expression written in the [Nix expression language](https://nixos.org/nix/manual/#ch-expression-language) used by the Nix package manager. If you get proficient with this language, you can compose expressions together to make your own packages from others (if that's useful to you).

# Motivation to use Nix<a id="sec-2"></a>

When making a new software project, wrangling dependencies can be a chore. For instance, GNU Make's makefiles often depend on executables and libraries that may or may not be on a system. The makefiles in most projects don't assist with getting these dependencies at usable versions. And many projects just provide error-prone instructions for how to get and install these dependencies manually.

Nix can build and install projects in a way that's precise, repeatable, and guaranteed not to conflict with anything already installed. Nix can even concurrently provide multiple versions of any package without conflicts.

Furthermore, Nix supports building for a variety of languages. In many cases, Nix picks up where language-specific tooling stop, layering on top of the tools and techniques native to those ecosystems. Nix expressions are designed for composition, which helps integrate packages from dependencies that may not all come from the same language ecosystem. These dependencies in Nix are themselves Nix packages.

To underscore how repeatable and precise Nix builds are, it helps to know that Nix uniquely identifies packages by a hash derived from the hashes of requisite dependencies and configuration. This is a recursive hash calculation that assures that the smallest change to even a distant transitive dependency changes the hash. When dependencies are downloaded, they are checked against the expected hash. Most Nix projects (this one included) are careful to pin dependencies to specific versions/hashes. Because of this, when building the same project with Nix on two different systems, we get an extremely high confidence we will get the same output, often bit-for-bit. This is a profound degree of precision relative to other popular package managers.

The repeatability and precision of Nix enables caching services, which for Nix are called *substituters*. Cachix is one such substituter. Before building a package, the hash for the package is calculated. If any configured substituter has a build for the hash, it's pulled down as a substitute. A certificate-based protocol is used to establish trust of substituters. Between this protocol, and the algorithm for calculating hashes in Nix, you can have confidence that a package pulled from a substituter will be identical to what you would have built locally.

All of this makes Nix an attractive tool for managing almost any software project.

# Installation and Setup<a id="sec-3"></a>

## Nix package manager setup<a id="sec-3-1"></a>

> **<span class="underline">NOTE:</span>** You don't need this step if you're running NixOS, which comes with Nix baked in.

If you don't already have Nix, the official installation script should work on a variety of UNIX-like operating systems. The easiest way to run this installation script is to execute the following shell command as a user other than root:

```shell
curl https://nixos.org/nix/install | sh
```

This script will download a distribution-independent binary tarball containing Nix and its dependencies, and unpack it in `/nix`.

The Nix manual describes [other methods of installing Nix](https://nixos.org/nix/manual/#chap-installation) that may suit you more.

## Cache setup<a id="sec-3-2"></a>

It's recommended to configure Nix to use shajra.cachix.org as a Nix *substituter*. This project pushes built Nix packages to [Cachix](https://cachix.org) as part of its continuous integration. Once configured, Nix will pull down these pre-built packages instead of building them locally.

You can configure shajra.cachix.org as a substituter with the following command:

```shell
nix run \
    --file https://cachix.org/api/v1/install \
    cachix \
    --command cachix use shajra
```

This will perform user-local configuration of Nix at `~/.config/nix/nix.conf`. This configuration will be available immediately, and any subsequent invocation of Nix commands will take advantage of the Cachix cache.

If you're running NixOS, you can configure Cachix globally by running the above command as a root user. The command will then configure `/etc/nixos/cachix/shajra.nix`, and the output will explain how to tie this configuration into your normal NixOS configuration.

# Working with Nix<a id="sec-4"></a>

Though covering Nix comprehensively is beyond the scope of this document, we'll go over a few commands illustrating using Nix with this project.

## Searching Nix files<a id="sec-4-1"></a>

Each of the Nix files in this project (ones with a ".nix" extension) contains exactly one Nix expression. This expression evaluates to one of the following values:

-   simple primitives and functions
-   derivations of packages that can be built and installed with Nix
-   containers of values, allowing a single value to provide more than one thing (these containers can nest).

Once you learn the Nix language, you can read these files to see what kind of values they build. We can use the `nix search` command to see what package derivations a Nix expression contains. For example from the top-level of this project, we can execute:

```shell
nix search --file default.nix --no-cache
```

    * autojump (autojump)
      A `cd' command that learns
    
    * binutils (binutils-wrapper)
      Tools for manipulating binaries (linker, assembler, etc.) (wrapper script)
    
    * bzip2 (bzip2)
      High-quality data compression program
    
    …

If you don't get the results above, see the [section on understanding derivations](#nix-drv) for an explanation of the likely problem and a workaround.

Note that because for extremely large Nix expressions, searching can be slow, `nix search` by defaults uses an indexed cache. This cache can be explicitly updated. However, because small local projects rarely have that many package derivations, the `--no-cache` switch is used above to bypass the cache. This guarantees accurate results that are fast enough. Otherwise, you will only get hits for the last Nix expression cached, which may be surprising.

The output of `nix search` is formatted as

    * attribute-name (name-of-package)
      Short description of package

*Attribute names* are used to select values from a Nix set containing multiple package derivations. If the Nix expression evaluates to a single derivation (not in a container), the attribute name will be missing from the `nix search` result.

Many Nix commands evaluate Nix files. If you specify a directory instead, the command will look for a `default.nix` file within to evaluate. So from the top-level of this project, we could use `.` instead of `default.nix`:

```shell
nix search --file . --no-cache
```

In the remainder of this document, we'll use `.` instead of `default.nix` since this is conventional for Nix.

## Building Nix expressions<a id="sec-4-2"></a>

From our execution of `nix search` we can see that a package named "binutils-wrapper" can be accessed with the "binutils" attribute name in the Nix expression in the top-level `default.nix`.

We can build this package with `nix build` from the top-level:

```shell
nix build --file . binutils
```

The positional arguments to `nix build` are attribute names. If you supply none then all attributes are built by default.

All packages built by Nix are stored in `/nix/store`. Nix won't rebuild packages found there. Once a package is built, its directory in `/nix/store` is read-only (until the package is deleted).

After a successful call of `nix build`, you'll see some symlinks for each package requested in the current working directory. These symlinks by default have a name prefixed with "result" and point back to the respective build in `/nix/store`:

```shell
readlink result*
```

    /nix/store/czlk0dv83mmcpjbsm3vgnpqqfac17dvh-binutils-wrapper-2.31.1

Following these symlinks, we can see the files the project provides:

```shell
tree -l result*
```

    result
    ├── bin
    │   ├── as -> /nix/store/bh3r88sv8wckwmfyhjxbqmxcha0hrm8h-binutils-2.31.1/bin/as
    │   ├── ld
    │   ├── ld.bfd
    │   └── ld.gold
    └── nix-support
        ├── add-flags.sh
        ├── add-hardening.sh
        ├── dynamic-linker
    …

It's common to configure these "result" symlinks as ignored in source control tools (for instance, within a Git `.gitignore` file).

`nix build` has a `--no-link` switch in case you want to build packages without creating "result" symlinks. To get the paths where your packages are located, you can use `nix path-info` after a successful build:

```shell
nix path-info --file . binutils
```

    /nix/store/czlk0dv83mmcpjbsm3vgnpqqfac17dvh-binutils-wrapper-2.31.1

## Running commands<a id="sec-4-3"></a>

You can run a command from a package in a Nix expression with `nix run`. For instance, to get the help message for the `ld` executable provided by the "binutils-wrapper" package selected by the "binutils" attribute name, we can call the following:

```shell
nix run \
    --file . \
    binutils \
    --command ld --help
```

    Usage: ld [options] file...
    Options:
      -a KEYWORD                  Shared library control for HP/UX compatibility
      -A ARCH, --architecture ARCH
                                  Set architecture
    …

You don't even have to build the package first with `nix build` or mess around with the "result" symlinks. `nix run` will build the project if it's not yet been built.

Again, as with `nix build` attribute names are specified as positional arguments to select packages.

The command to run is specified after the `--command` switch. `nix run` runs the command in a shell set up with a `PATH` environment variable including all the `bin` directories provided by the selected packages.

`nix run` also supports an `--ignore-environment` flag that restricts `PATH` to only packages selected, rather than extending the `PATH` of the outside environment. With `--ignore-environment`, the invocation is more sandboxed.

## Installing and uninstalling programs<a id="sec-4-4"></a>

We've seen that we can build programs with `nix build` and then execute programs using the "result" symlink (`result/bin/*`). Additionally, we've seen that you can run programs with `nix run`. But these additional steps and switches/arguments can feel extraneous. It would be nice if we could just have the programs on our `PATH`. This is what `nix-env` is for.

`nix-env` maintains a symlink tree, called a *profile*, of installed programs. The active profile is pointed to by a symlink at `~/.nix-profile`. By default, this profile points to `/nix/var/nix/profiles/per-user/$USER/profile`. But you can point your `~/.nix-profile` to any writable location with the `--switch-profile` switch:

```shell
nix-env --switch-profile /nix/var/nix/profiles/per-user/$USER/another-profile
```

This way, you can just put `~/.nix-profile/bin` on your `PATH`, and any programs installed in your currently active profile will be available for interactive use or scripts.

We can query what's installed in the active profile with the `--query` switch:

```shell
nix-env --query
```

To install the `ld` executable, which is accessed by the "binutils" in our top-level `default.nix` file, we'd run the following:

```shell
nix-env --install --file . --attr binutils 2>&1
```

    …
    installing 'binutils-wrapper-2.31.1'

We can see this installation by querying what's been installed:

```shell
nix-env --query
```

    binutils-wrapper-2.31.1

And if we want to uninstall a program from our active profile, we do so by its name, in this case "binutils-wrapper":

```shell
nix-env --uninstall binutils-wrapper 2>&1
```

    uninstalling 'binutils-wrapper-2.31.1'

Note that we've installed our package using its attribute name ("binutils") within the referenced Nix expression. But we uninstall it using the package name ("binutils-wrapper"), which may or may not be the same as the attribute name. When a package is installed, Nix keeps no reference to the expression that evaluated to the derivation of the installed package. The attribute name is only relevant to this expression. In fact, two different expressions could evaluate to the exact same derivation, but use different attribute names. This is why we uninstall packages by their package name.

See the [documentation for `nix-env`](https://nixos.org/nix/manual/#sec-nix-env) for more details.

## Garbage collection<a id="sec-4-5"></a>

Old versions of packages stick around in `/nix/store`. We can clean this up with garbage collection by calling `nix-collect-garbage`.

For each package, Nix is aware of all references back to `/nix/store` for other packages, whether in text files or binaries. This allows Nix to prevent the deletion of a runtime dependency required by another package.

Symlinks pointing to packages to exclude from garbage collection are maintained by Nix in `/nix/var/nix/gcroots`. Looking closer, you'll see that for each "result" symlink created by a `nix build` invocation, there's symlinks in `/nix/var/nix/gcroots/auto` pointing back it. So we've got symlinks in `/nix/var/nix/gcroots/auto` pointing to "result" symlinks in our projects, which then reference the actual built project in `/nix/store`.

These symlinks prevent packages built by `nix build` from being garbage collected. If you want a package you've built with `nix build` to be garbage collected, delete the "result" symlink created before calling `nix-collect-garbage`. Breaking symlinks under `/nix/var/nix/gcroots` removes protection from garbage collection. `nix-collect-garbage` will cleans up broken symlinks when it runs.

Also, it's good to know that `nix-collect-garbage` won't delete packages referenced by any running processes. In the case of `nix run` no garbage collection root symlink is created under `/nix/var/nix/gcroots`, but while `nix run` is running a `nix-collect-garbage` won't delete packages needed by the invocation. However, once the `nix run` call exits, any packages pulled from a substituter or built locally are candidates for deletion by `nix-collect-garbage`. If you called `nix run` again after garbage collecting, those packages might be pulled or built again.

## Understanding derivations<a id="nix-drv"></a>

We haven't detailed what happens when we build a Nix expression that evaluates to a package derivation. There are two important steps:

1.  *instantiating* the derivation
2.  *realizing* the instantiated derivation, which builds the final package.

An instantiated derivation is effectively a script stored in `/nix/store` that Nix can run to build the final package (which also ends up in `/nix/store`). These instantiated derivations have a ".drv" extension, and if you look in `/nix/store` you may find some. Instantiated derivations have references to all necessary build dependencies, also in `/nix/store`, which means that everything is physically in place in `/nix/store` to build the package. No further evaluation of a Nix expression is needed once we have an instantiated derivation to build the final package. Note that both `nix build` and `nix run` perform both instantiation and realization of a derivation, so for the most part, we don't have to worry about the difference between instantiating and realizing a derivation.

However, you may encounter a Nix expression where `nix search` returns nothing, though you're sure that there are derivations to select out. In this case, the Nix expression is using an advanced technique that unfortunately hides attributes from `nix search` until some derivations are instantiated into `/nix/store`. We can force the instantiation of these derivations without realizing their packages with the following command:

```shell
nix show-derivation --file default.nix
```

Once these derivations are instantiated, you may get more results with `nix search` for the occasional Nix expression that uses some advanced techniques.

## Lazy evaluation<a id="sec-4-7"></a>

We haven't made a big deal of it, but the Nix language is *lazily evaluated*. This allows a single Nix expression to refer to several thousand packages, but without requiring us to evaluate everything when selecting out packages by attribute names. In fact, the entire NixOS operating system is based heavily on a single single expression managed in a Git repository called [Nixpkgs](https://github.com/NixOS/nixpkgs).

# Next Steps<a id="sec-5"></a>

This document has covered a fraction of Nix usage, hopefully enough to introduce Nix in the context of [this project](../README.md).

An obvious place to start learn more about Nix is [the official documentation](https://nixos.org/learn.html). The author of this project also maintains another project with [a small tutorial on Nix](https://github.com/shajra/example-nix/tree/master/tutorials/0-nix-intro). This tutorial covers the Nix expression language in more detail.

All the commands we've covered have more switches and options. See the respective man-pages for more. Also, we didn't cover `nix-shell`, which can be used for setting up development environments. And we didn't cover much of [Nixpkgs](https://github.com/NixOS/nixpkgs), the gigantic repository of community-curated Nix expressions.

The Nix ecosystem is vast. This project and documentation illustrates just a small example of what Nix can do.
