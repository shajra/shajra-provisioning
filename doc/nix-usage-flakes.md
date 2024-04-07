- [About this document](#sec-1)
- [How this project uses Nix](#sec-2)
- [Prerequisites](#sec-3)
- [Working with Nix](#sec-4)
  - [Referencing flake projects](#sec-4-1)
  - [Inspecting flake outputs](#sec-4-2)
  - [Referencing flake outputs](#sec-4-3)
  - [Searching flakes for packages](#sec-4-4)
  - [Building installables](#sec-4-5)
  - [Running commands in a shell](#sec-4-6)
  - [Running installables](#sec-4-7)
  - [`nix run` and `nix shell` with remote flakes](#sec-4-8)
  - [Installing and uninstalling programs](#sec-4-9)
  - [Garbage collection](#sec-4-10)
- [Next steps](#sec-5)


# About this document<a id="sec-1"></a>

This document explains how to take advantage of software provided by Nix for people new to [the Nix package manager](https://nixos.org/nix). This guide uses this project for examples but focuses on introducing general Nix usage, which also applies to other projects using Nix.

This project requires an experimental feature of Nix called *flakes*. To understand more about what flakes are and the consequences of using a still-experimental feature of Nix, please see the provided [introduction to Nix](nix-introduction.md).

# How this project uses Nix<a id="sec-2"></a>

This project uses Nix to download all necessary dependencies and build everything from source. In this regard, Nix is helpful as not just a package manager but also a build tool. Nix helps us get from raw source files to built executables in a package we can install with Nix.

Within this project, the various files with a `.nix` extension are Nix files, each of which contains an expression written in the [Nix expression language](https://nixos.org/manual/nix/stable/language/index.html) used by the Nix package manager to specify packages. If you get proficient with this language, you can use these expressions as a starting point to compose your own packages beyond what's provided in this project.

# Prerequisites<a id="sec-3"></a>

If you're new to Nix consider reading the provided [introduction](nix-introduction.md).

This project supports

-   Linux on x86-64 machines
-   MacOS on x86-64 machines
-   MacOS on ARM64 machines (M1 or M2).

That may affect your ability to follow along with examples.

Otherwise, see the provided [Nix installation and configuration guide](nix-installation.md) if you have not yet set Nix up.

To continue following this usage guide, you will need Nix's experimental flakes feature. You can enable this globally or use an alias such as the following:

```sh
alias nix-flakes = nix --extra-experimental-features 'nix-command flakes'
```

# Working with Nix<a id="sec-4"></a>

Though comprehensively covering Nix is beyond the scope of this document, we'll go over a few commands illustrating some usage of Nix with this project.

## Referencing flake projects<a id="sec-4-1"></a>

Most of this document illustrates usage of the `nix` command, which provides a number of subcommands and centralizes Nix usage.

Many of the `nix` subcommands accept references to flake-enabled projects. A flake is specified with a Nix expression saved in a file named `flake.nix`. This file should be at the root of a project. We can reference both local and remote flake projects.

Here are some common forms we can use to reference flake projects:

| Syntax                           | Location                                                       |
|-------------------------------- |-------------------------------------------------------------- |
| `.`                              | flake in the current directory                                 |
| `<path>`                         | flake in some other file path (must have a slash)              |
| `<registry>`                     | reference to flake from the registry (see `nix registry list`) |
| `git+<url>`                      | latest flake in the default branch of a Git repository         |
| `git+<url>?ref=<branch>`         | latest flake in a branch of a Git repository                   |
| `git+<url>?rev=<commit>`         | flake in a specific commit of a Git repository                 |
| `github:<owner>/<repo>`          | latest flake in the default branch of a GitHub repository      |
| `github:<owner>/<repo>/<branch>` | latest flake in a branch of a GitHub repository                |
| `github:<owner>/<repo>/<commit>` | flake in a specific commit of a GitHub repository              |

This table introduces an angle-bracket notation for syntactic forms with components that change with context. This notation is used throughout this document.

Referencing local flake projects is easy enough with file paths. But the URL-like notation for remote flake projects can get verbose. Furthermore, some of these references are not fixed. For example, Git branches point to different commits over time.

To manage flake references, Nix provides a flakes registry. Upon installation this registry is prepopulated with some global entries:

```sh
nix registry list
```

    …
    global flake:nixos-homepage github:NixOS/nixos-homepage
    global flake:nixos-search github:NixOS/nixos-search
    global flake:nixpkgs github:NixOS/nixpkgs/nixpkgs-unstable
    global flake:nur github:nix-community/NUR
    global flake:patchelf github:NixOS/patchelf
    global flake:poetry2nix github:nix-community/poetry2nix
    global flake:pridefetch github:SpyHoodle/pridefetch
    global flake:sops-nix github:Mic92/sops-nix
    global flake:systems github:nix-systems/default
    global flake:templates github:NixOS/templates

For example, rather than referencing the flake on the `nixpkgs-unstable` branch of the Nixpkgs GitHub repository with `github:NixOS/nixpkgs/nixpkgs-unstable`, we can use the simpler identifier `nixpkgs`.

If we want to point to a different branch but still use an identifier from the registry, we can do so by extending it with the branch. For example, the flakes identifier `nixpkgs` is the same as `nixpkgs/nixpkgs-ustable`, but we can also use `nixpkgs/nixos-23.11` to override the branch and point to the NixOS 23.11 release branch.

Note that registries have mutable references, but Nix knows how to rebuild the snapshot referenced for some of these references deterministically. For example, when referencing a GitHub repository via a registry reference, Nix will take note of the commit ID of the snapshot retrieved. Nix typically stores this information required for reproducibility in a *lock file* called `flake.lock` adjacent to `flake.nix`.

## Inspecting flake outputs<a id="sec-4-2"></a>

A flake can provide a variety of outputs that can be used in different contexts. A few of these outputs include the packages we can build and install.

We can use `nix flake show` to see the outputs provided by any flake, local or remote, by providing a flake reference discussed in the previous section. Here's an example of inspecting the flake of this project locally:

```sh
nix flake show .
```

    git+file:///home/shajra/src/shajra-provisioning
    ├───apps
    │   ├───aarch64-darwin
    …
        │   ├───ci-build-nixpkgs omitted (use '--all-systems' to show)
        │   ├───ci-build-shajra omitted (use '--all-systems' to show)
        │   ├───ci-prebuilt-haskellnix omitted (use '--all-systems' to show)
        │   ├───ci-prebuilt-nixpkgs omitted (use '--all-systems' to show)
        │   ├───ci-prebuilt-shajra omitted (use '--all-systems' to show)
        │   ├───home-manager omitted (use '--all-systems' to show)
        │   ├───shajra-darwin-rebuild omitted (use '--all-systems' to show)
        │   ├───shajra-home-manager omitted (use '--all-systems' to show)
        │   └───shajra-nixos-rebuild omitted (use '--all-systems' to show)
        └───x86_64-linux
            ├───ci-build-haskellnix: package 'shajra-provision-ci-build-haskellnix'
            ├───ci-build-nixpkgs: package 'shajra-provision-ci-build-nixpkgs'
            ├───ci-build-shajra: package 'shajra-provision-ci-build-shajra'
            ├───ci-prebuilt-haskellnix: package 'shajra-provision-ci-prebuilt-haskellnix'
            ├───ci-prebuilt-nixpkgs: package 'shajra-provision-ci-prebuilt-nixpkgs'
            ├───ci-prebuilt-shajra: package 'shajra-provision-ci-prebuilt-shajra'
            ├───home-manager: package 'home-manager'
            ├───shajra-darwin-rebuild: package 'shajra-darwin-rebuild'
            ├───shajra-home-manager: package 'shajra-home-manager'
            └───shajra-nixos-rebuild: package 'shajra-nixos-rebuild'

Flake outputs are organized in a tree of *attributes*. References to paths of attributes are dot-delimited. There is a standard schema for the output attribute tree of a flake. Nix permits outputs outside this schema.

This document focuses on packages provided by the `packages` output attribute. Notice that a flake offers packages for different (but rarely all) system architectures.

For commands like `nix flake show` that expect a flake reference as an argument, `.` is assumed as default if an argument isn't provided. So `nix flake show` is equivalent to `nix flake show .`.

## Referencing flake outputs<a id="sec-4-3"></a>

Many of the `nix` subcommands work with references to flakes outputs. These references are called *installables*. There are many types of installables (hence the general name). In this document, we'll hone in on the following forms:

-   `<flake>` to select a default output from
-   `<flake>#<output reference>` for a reference to packages(s) within a flake.

In this notation, `<flake>` is the same reference to a local or remote flake project discussed in the prior section.

When the installable is just a flake reference, the called `nix` subcommand will generally look for the `packages.<system>.default` attribute path within the flake. So `<flake>` will generally be the same as `<flake>#packages.<system>.default`. One exception to this rule, `nix run` will first look for `<flake>#apps.<system>.default`, and then for `<flake>#packages.<system>.default`.

Installables can also reference an output of a flake (`<output reference>` above) directly in a couple of ways:

| Output reference                     | Example installable                    |
|------------------------------------ |-------------------------------------- |
| `<output attribute>.<system>.<name>` | `.#packages.x86_64-linux.home-manager` |
| `<name>`                             | `.#home-manager`                       |

The first way is the most explicit, by providing the full attribute path we can see with `nix flake show`. But this requires specifying the package's system architecture.

With the second form, with the shorter `<name>` package reference, Nix will detect the system we're on and also search some attributes in a precedence order for the provided name:

1.  `apps.<system>.<name>` (`nix run` only)
2.  `packages.<system>.<name>` (all subcommands accepting installables)
3.  `legacyPackages.<system>.<name>` (all subcommands accepting installables)

Which attributes are searched depends on the `nix` subcommand called.

For commands accepting installables as an argument, if none are provided, then `.` is assumed. Nix will attempt to read a `flake.nix` file in the current directory. If not found, Nix will recursively search parent directories to find a `flake.nix` file.

## Searching flakes for packages<a id="sec-4-4"></a>

We can use the `nix search` command to see what package derivations a flake contains. For example, from the root directory of this project, we can execute:

```sh
nix search .
```

    * packages.x86_64-linux.ci-build-haskellnix
    
    * packages.x86_64-linux.ci-build-nixpkgs
    
    * packages.x86_64-linux.ci-build-shajra
    
    * packages.x86_64-linux.ci-prebuilt-haskellnix
    
    * packages.x86_64-linux.ci-prebuilt-nixpkgs
    …

If a flake has a lot of packages, you can pass regexes to prune down the search. Returned values will match all the regexes provided.

We can also search a remote repository for packages to install. For example, Nixpkgs is a central repository for Nix, providing several thousand packages. We can search the “nixpkgs-unstable” branch of [Nixpkgs' GitHub repository](https://github.com/NixOS/nixpkgs) for packages that match both “gpu|opengl|accel” and “terminal” as follows:

```sh
nix search github:NixOS/nixpkgs/nixpkgs-unstable 'gpu|opengl|accel' terminal
```

As discussed in a previous section, we can use the flakes registry identifier of `nixpkgs` instead of the longer `github:NixOS/nixpkgs/nixpkgs-unstable` to save some typing:

```sh
nix search nixpkgs 'gpu|opengl|accel' terminal
+end_src

#+name: nix-search-remote-concise
#+begin_src sh :dir .. :results output :exports results
nix search nixpkgs 'gpu|opengl|accel' terminal | ansifilter
```

    * legacyPackages.x86_64-linux.alacritty (0.13.1)
      A cross-platform, GPU-accelerated terminal emulator
    
    * legacyPackages.x86_64-linux.darktile (0.0.11)
      A GPU rendered terminal emulator designed for tiling window managers
    
    * legacyPackages.x86_64-linux.kitty (0.32.0)
      A modern, hackable, featureful, OpenGL based terminal emulator
    
    * legacyPackages.x86_64-linux.rio (0.0.34)
      A hardware-accelerated GPU terminal emulator powered by WebGPU
    
    * legacyPackages.x86_64-linux.wezterm (20230712-072601-f4abf8fd)
      GPU-accelerated cross-platform terminal emulator and multiplexer written by @wez and implemented in Rust

If we're curious about what version of WezTerm is available in NixOS's latest release, we can specialize the installable we're searching as follows:

```sh
nix search nixpkgs/nixos-23.11#wezterm
```

    * legacyPackages.x86_64-linux.wezterm (20230712-072601-f4abf8fd)
      GPU-accelerated cross-platform terminal emulator and multiplexer written by @wez and implemented in Rust

Here `/nixos-23.11` overrides the default `nixpkgs-unstable` branch of the registry entry, and the `#wezterm` suffix searches not just the flake, but a specific package named `wezterm`, which will either be found or not (there's no need for regexes to filter further).

You may also notice that the Nixpkgs flake outputs packages under the `legacyPackages` attribute instead of the `packages`. The primary difference is that packages are flatly organized under `packages`, while `legacyPackages` can be an arbitrary tree. `legacyPackages` exists specifically for the Nixpkgs project, a central project to the Nix ecosystem that has existed long before flakes. Beyond Nixpkgs, you don't have to think much about `legacyPackages`. Packages from all other flakes should generally be found under `packages`.

## Building installables<a id="sec-4-5"></a>

The following result is one returned by our prior execution of `nix search .`:

    * packages.x86_64-linux.home-manager
      A user environment configurator
    
    * packages.x86_64-linux.shajra-home-manager
      Controlled home directory management with Nix

We can see that a package can be accessed with the `packages.x86_64-linux.home-manager` output attribute path of the project's flake. Not shown in the search results above, this package happens to provide the executable `bin/home-manager`.

We can build this package with `nix build` from the project root:

```sh
nix build .#home-manager
```

As discussed in prior sections, the positional arguments to `nix build` are *installables*. Here, the `.` indicates that our flake should be found from the current directory. Within this flake, we look for a package with an attribute name of `home-manager`. We didn't have to use the full attribute path `packages.x86_64-linux.home-manager` because `nix build` will automatically look in the `packages` attribute for the system it detects we're on.

If we omit the attribute path of our installable, Nix tries to build a default package, which it expects to find under the flake's `packages.<system>.default`. For example, if we ran just `nix build .`, Nix would expect to find a `flake.nix` in the current directory with an output providing a `packages.<system>.default` attribute with a package to build.

Furthermore, if we didn't specify an installable at all, Nix would assume we're trying to build the default package of the flake found from the current directory. So, the following invocations are all equivalent:

-   `nix build`
-   `nix build .`
-   `nix build .#packages.<system>.default`

All packages built by Nix are stored in `/nix/store`. Nix won't rebuild packages found there. Once a package is built, its content in `/nix/store` is read-only (until the package is garbage collected, discussed later).

After a successful call of `nix build`, you'll see one or more symlinks for each package requested in the current working directory. These symlinks, by default, have a name prefixed with “result” and point back to the respective build in `/nix/store`:

```sh
readlink result*
```

    /nix/store/nnhp4y9f0z4lzlfsghvdqi9czc5q70xn-home-manager

Following these symlinks, we can see the files the project provides:

```sh
tree -l result*
```

    result
    ├── bin
    │   └── home-manager
    └── share
        ├── bash
        │   └── home-manager.sh
        ├── bash-completion
        │   └── completions
        │       └── home-manager
        ├── fish
    …

It's common to configure these “result” symlinks as ignored in source control tools (for instance, for Git within a `.gitignore` file).

`nix build` has a `--no-link` switch in case you want to build packages without creating “result” symlinks. To get the paths where your packages are located, you can use `nix path-info` after a successful build:

```sh
nix path-info .#home-manager
```

    /nix/store/nnhp4y9f0z4lzlfsghvdqi9czc5q70xn-home-manager

## Running commands in a shell<a id="sec-4-6"></a>

We can run commands in Nix-curated environments with `nix shell`. Nix will take executables found in packages, put them in an environment's `PATH`, and then execute a user-specified command.

With `nix shell`, you don't have to build the package first with `nix build` or mess around with “result” symlinks. `nix shell` will build any necessary packages required.

For example, to get the help message for the `home-manager` executable provided by the package selected by the `home-manager` attribute path output by this project's flake, we can call the following:

```sh
nix shell \
    .#home-manager \
    --command home-manager --help
```

    Usage: /nix/store/nnhp4y9f0z4lzlfsghvdqi9czc5q70xn-home-manager/bin/home-manager [OPTION] COMMAND
    
    Options
    
      -f FILE           The home configuration file.
    …

Similarly to `nix build`, `nix shell` accepts installables as positional arguments to select packages to put on the `PATH`.

The command to run within the shell is specified after the `--command` switch. `nix shell` runs the command in a shell set up with a `PATH` environment variable that includes all the `bin` directories provided by the selected packages.

If you only want to enter an interactive shell with the configured `PATH`, you can drop the `--command` switch and following arguments.

`nix shell` also supports an `--ignore-environment` flag that restricts `PATH` to only packages selected, rather than extending the `PATH` of the caller's environment. With `--ignore-environment`, the invocation is more sandboxed.

As with `nix build`, `nix shell` will select default packages for any installable that is only a flake reference. If no installable is provided to `nix shell`, the invocation will look for the default package under the `packages.<system>.default` attribute output by a flake assumed to be in the current directory. So, the following invocations are all equivalent:

-   `nix shell`
-   `nix shell .`
-   `nix shell .#packages.<system>.default`

## Running installables<a id="sec-4-7"></a>

The `nix run` command allows us to run executables from packages with a more concise syntax than `nix shell` with a `--command` switch.

The main difference from `nix shell` is that `nix run` detects which executable to run from a package. If we want something other than what can be detected, we must continue using `nix shell` with `--command`.

As with other `nix` subcommands, `nix run` accepts an installable as an argument (but only one). If none is provided, then `.` is assumed.

If the provided installable is only a flake reference with no package selected, then `nix run` searches the following flake output attribute paths, in order, for something to run:

-   `apps.<system>.default`
-   `packages.<system>.default`

In flakes, applications are just packages that have been expanded with metadata. This metadata includes explicitly specifying which executable to use with `nix run`. Otherwise, for plain packages, `nix run` looks at metadata on the package to guess the binary to execute within the package.

If only a name is provided, then `nix run` searches in order through the following output attribute paths:

-   `apps.<system>.<name>`
-   `packages.<system>.<name>`
-   `legacyPackages.<system>.<name>`

And as always, we can specify a full output attribute path explicitly if `nix run`'s search doesn't find what we want to run.

Here's the `nix run` equivalent of the `nix shell` invocation from the previous section:

```sh
nix run .#home-manager  -- --help
```

    Usage: /nix/store/nnhp4y9f0z4lzlfsghvdqi9czc5q70xn-home-manager/bin/home-manager [OPTION] COMMAND
    
    Options
    
      -f FILE           The home configuration file.
    …

We can see some of the metadata of this package with the `--json` switch of `nix search`:

```sh
nix search --json .#home-manager | jq .
```

    {
      "packages.x86_64-linux.home-manager": {
        "description": "A user environment configurator",
        "pname": "home-manager",
        "version": ""
    …

The “pname” field in the JSON above indicates the package's name. In practice, the package's name may or may not differ from flake output name of the installable.

`nix run` works because the package selected by the output attribute name `home-manager` selects a package with a package name “home-manager” that is the same as the executable provided at `bin/home-manager`.

If we want something other than what can be detected, then we have to continue using `nix shell` with `--command`.

## `nix run` and `nix shell` with remote flakes<a id="sec-4-8"></a>

In the examples above, we've used selected packages from this project's flake, like `.#home-manager`. But one benefit of flakes is that we can refer to remote flakes just as easily, like `nixpkgs#hello`. Referencing remote flakes helps us quickly build environments with `nix shell` or run commands with `nix run` without committing to install software.

Here's a small example.

```sh
nix run nixpkgs#hello
```

    Hello, world!

When using `nix shell`, we can even mix local flake references with remote ones, all in the same invocation:

```sh
nix shell --ignore-environment \
    nixpkgs#which \
    .#home-manager \
    --command which home-manager
```

    /nix/store/nnhp4y9f0z4lzlfsghvdqi9czc5q70xn-home-manager/bin/home-manager

What we do with local flake references can work just as well with remote flake references.

## Installing and uninstalling programs<a id="sec-4-9"></a>

We've seen that we can build programs with `nix build` and execute them using the “result” symlink (`result/bin/*`). Additionally, we've seen that you can run programs with `nix shell` and `nix run`. But these additional steps and switches/arguments can feel extraneous. It would be nice to have the programs on our `PATH`. This is what `nix profile` is for.

`nix profile` maintains a symlink tree of installed programs called a *profile*. The default profile is at `~/.nix-profile`. For non-root users, if this doesn't exist, `nix profile` will create it as a symlink pointing to `/nix/var/nix/profiles/per-user/$USER/profile`. But you can point `nix profile` to another profile at any writable location with the `--profile` switch.

This way, you can just put `~/.nix-profile/bin` on your `PATH`, and any programs installed in your default profile will be available for interactive use or scripts.

To install the `home-manager` executable, which is provided by the `home-manager` output of our flake, we'd run the following:

```sh
nix profile install .#home-manager
```

We can see this installation by querying what's been installed:

```sh
nix profile list
```

    Index:              0
    Flake attribute:    packages.x86_64-linux.home-manager
    Original flake URL: git+file:///home/shajra/src/shajra-provisioning
    Locked flake URL:   git+file:///home/shajra/src/shajra-provisioning
    Store paths:        /nix/store/nnhp4y9f0z4lzlfsghvdqi9czc5q70xn-home-manager

If we want to uninstall a program from our profile, we do so by the index from this list:

```sh
nix profile remove 0
```

We can also provide a regex matching the full attribute path of the flake:

```sh
nix profile remove '.*home-manager'
```

Also, if you look at the symlink-resolved location for your profile, you'll see that Nix retains the symlink trees of previous generations of your profile. You can even roll back to an earlier profile with the `nix profile rollback` subcommand. You can delete old generations of your profile with the `nix profile wipe-history` subcommand.

## Garbage collection<a id="sec-4-10"></a>

Every time you build a new version of your code, it's stored in `/nix/store`. You can call `nix store gc` to purge unneeded packages. Programs that should not be removed by `nix store gc` can be found by starting with symlinks stored as *garbage collection (GC) roots* under three locations:

-   `/nix/var/nix/gcroots`
-   `/nix/var/nix/profiles`
-   `/nix/var/nix/manifests`.

For each package, Nix is aware of all files that reference back to other packages in `/nix/store`, whether in text files or binaries. This dependency tracking helps Nix ensure that dependencies of packages reachable from GC roots won't be deleted by garbage collection.

Each “result” symlink created by a `nix build` invocation has a symlink in `/nix/var/nix/gcroots/auto` pointing back to it. So we've got symlinks in `/nix/var/nix/gcroots/auto` pointing to “result” symlinks in our projects, which then reference the actual built project in `/nix/store`. These chains of symlinks prevent packages built by `nix build` from being garbage collected.

If you want a package you've built with `nix build` to be garbage collected, delete the “result” symlink created before calling `nix store gc`. Breaking symlink chains under `/nix/var/nix/gcroots` removes protection from garbage collection. `nix store gc` will clean up broken symlinks when it runs.

Everything under `/nix/var/nix/profiles` is also considered a GC root. This is why users, by convention, use this location to store their Nix profiles with `nix profile`.

Also, note if you delete a “result\*” link and call `nix store gc`, though some garbage may be reclaimed, you may find that an old profile is keeping the program alive. Use the `nix profile wipe-history` command to delete old profiles before calling `nix store gc`.

It's also good to know that `nix store gc` won't delete packages referenced by any running processes. In the case of `nix run` no garbage collection root symlink is created under `/nix/var/nix/gcroots`, but while `nix run` is running `nix store gc` won't delete packages needed by the running command. However, once the `nix run` call exits, any packages pulled from a substituter or built locally are candidates for deletion by `nix store gc`. If you called `nix run` again after garbage collecting, those packages may be pulled or built again.

# Next steps<a id="sec-5"></a>

This document has covered a fraction of Nix usage, hopefully enough to introduce Nix in the context of [this project](../README.md).

An obvious place to start learning more about Nix is [the official documentation](https://nixos.org/learn.html).

Bundled with this project is [a small tutorial on the Nix language](nix-language.md).

All the commands we've covered have more switches and options. See the respective man pages for more.

We didn't cover much of [Nixpkgs](https://github.com/NixOS/nixpkgs), the gigantic repository of community-curated Nix expressions.

The Nix ecosystem is vast. This project and documentation illustrate only a small sample of what Nix can do.
