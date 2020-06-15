- [How this project uses Nix](#sec-1)
- [Motivation to use Nix](#sec-2)
- [Installation and Setup](#sec-3)
  - [Nix package manager setup](#sec-3-1)
  - [Cache setup](#sec-3-2)
- [Working with Nix](#sec-4)
  - [Understanding Nix files](#sec-4-1)
  - [Building Nix expressions](#sec-4-2)
  - [Running commands](#sec-4-3)
  - [Installing and uninstalling programs](#sec-4-4)
  - [Garbage collection](#sec-4-5)
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

## Understanding Nix files<a id="sec-4-1"></a>

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
    
    * binutils-unwrapped (binutils)
      Tools for manipulating binaries (linker, assembler, etc.)
    
    * cabal-install (cabal-install)
      The command-line interface for Cabal and Hackage
    
    …

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

From our execution of `nix search` we can see that a package named "binutils" can be accessed with the "binutils-unwrapped" attribute name in the Nix expression in the top-level `default.nix`.

We can build this package with `nix build` from the top-level:

```shell
nix build --file . binutils-unwrapped
```

Because `nix build` by default builds `default.nix`, you don't need the `--file .` argument. So our invocation could be even more simple:

```shell
nix build binutils-unwrapped
```

The positional arguments to `nix build` are attribute names. If you supply none then all attributes are built by default.

All packages built by Nix are stored in `/nix/store`. Nix won't rebuild packages found there. Once a package is built, its directory in `/nix/store` is read-only (until the package is deleted).

After a successful call of `nix build`, you'll see some symlinks for each package requested in the current working directory. These symlinks by default have a name prefixed with "result" and point back to the respective build in `/nix/store`:

```shell
readlink result*
```

    /nix/store/ivmds12g88jbjf8famgwkj36a4rrfcy6-binutils-2.31.1

Following these symlinks, we can see the files the project provides:

```shell
tree -l result*
```

    result
    ├── bin
    │   ├── addr2line
    │   ├── ar
    │   ├── as
    │   ├── c++filt
    │   ├── dwp
    │   ├── elfedit
    │   ├── gprof
    │   ├── ld -> ld.bfd
    …

It's common to configure these "result" symlinks as ignored in source control tools (for instance, within a Git `.gitignore` file).

## Running commands<a id="sec-4-3"></a>

You can run a command from a package in a Nix expression with `nix run`. For instance, to get the help message for the `addr2line` executable provided by the "binutils" package selected by the "binutils-unwrapped" attribute name, we can call the following:

```shell
nix run \
    --file . \
    binutils-unwrapped \
    --command addr2line --help
```

    Usage: addr2line [option(s)] [addr(s)]
     Convert addresses into line number/file name pairs.
     If no addresses are specified on the command line, they will be read from stdin
     The options are:
      @<file>                Read options from <file>
    …

You don't even have to build the package first with `nix build` or mess around with the "result" symlinks. `nix run` will build the project if it's not yet been built.

Note that unlike `nix build` the `--file .` argument is needed in this case. The default expression for `nix run` is different than `default.nix` in the current working directory.

Again, as with `nix build` attribute names are specified as positional argument to select packages.

The command to run is specified after the `--command` switch. `nix run` runs the command in a shell set up with a `PATH` environment variable including all the `bin` directories provided by the selected packages.

`nux run` also supports an `--ignore-environment` flag that restricts `PATH` to only packages selected, rather than extending the `PATH` of the outside environment. With `--ignore-environment`, the invocation is more sandboxed.

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

To install the `addr2line` executable, which is accessed by the "binutils-unwrapped" in our top-level `default.nix` file, we'd run the following:

```shell
nix-env --install --file . --attr binutils-unwrapped
```

We can see this installation by querying what's been installed:

```shell
nix-env --query
```

    binutils-2.31.1

And if we want to uninstall a program from our active profile, we do so by its name, in this case "binutils":

```shell
nix-env --uninstall binutils
```

Note that we've installed our package using its attribute name ("binutils-unwrapped") within the referenced Nix expression. But we uninstall it using the package name ("binutils"), which may or may not be the same as the attribute name. When a package is installed, Nix keeps no reference to the expression that evaluated to the derivation of the installed package. The attribute name is only relevant to this expression. In fact, two different expressions could evaluate to the exact same derivation, but use different attribute names. This is why we uninstall packages by their package name.

See the [documentation for `nix-env`](https://nixos.org/nix/manual/#sec-nix-env) for more details.

## Garbage collection<a id="sec-4-5"></a>

Old versions of packages stick around in `/nix/store`. We can clean this up with garbage collection by calling `nix-collect-garbage`.

For each package, Nix is aware of all references back to `/nix/store` for other packages, whether in text files or binaries. This allows Nix to prevent the deletion of a runtime dependency required by another package.

Symlinks pointing to packages to exclude from garbage collection are maintained by Nix in `/nix/var/nix/gcroots`. Looking closer, you'll see that for each "result" symlink created by a `nix build` invocation, there's symlinks in `/nix/var/nix/gcroots/auto` pointing back it. So we've got symlinks in `/nix/var/nix/gcroots/auto` pointing to "result" symlinks in our projects, which then reference the actual built project in `/nix/store`.

These symlinks prevent packages built by `nix build` from being garbage collected. If you want a package you've built with `nix build` to be garbage collected, delete the "result" symlink created before calling `nix-collect-garbage`. Breaking symlinks under `/nix/var/nix/gcroots` removes protection from garbage collection. `nix-collect-garbage` will cleans up broken symlinks when it runs.

Also, it's good to know that `nix-collect-garbage` won't delete packages referenced by any running processes. In the case of `nix run` no garbage collection root symlink is created under `/nix/var/nix/gcroots`, but while `nix run` is running a `nix-collect-garbage` won't delete packages needed by the invocation. However, once the `nix run` call exits, any packages pulled from a substituter or built locally are candidates for deletion by `nix-collect-garbage`. If you called `nix run` again after garbage collecting, those packages might be pulled or built again.

# Next Steps<a id="sec-5"></a>

This document has covered a fraction of Nix usage, hopefully enough to introduce Nix in the context of [this project](../README.md).

An obvious place to start learn more about Nix is [the official documentation](https://nixos.org/learn.html). The author of this project also maintains another project with [a small tutorial on Nix](https://github.com/shajra/example-nix/tree/master/tutorials/0-nix-intro). This tutorial covers the Nix expression language in more detail.

All the commands we've covered have more switches and options. See the respective man-pages for more. Also, we didn't cover `nix-shell`, which can be used for setting up development environments. And we didn't cover [Nixpkgs](https://github.com/NixOS/nixpkgs), a gigantic repository of community-curated Nix expressions.

The Nix ecosystem is vast. This project and documentation illustrates just a small example of what Nix can do.
