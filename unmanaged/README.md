- [Prerequisites beyond installing Nix itself](#sec-1)
  - [Access to private repository](#sec-1-1)
  - [Git repository ownership](#sec-1-2)
  - [Homebrew installation](#sec-1-3)
- [Other little things](#sec-2)

I'm grateful that Nix is able to configure so much of my systems, both Linux and macOS included. However, projects like [Nix-darwin](https://daiderd.com/nix-darwin) can only go so far. The rest requires manual configuration. This folder contains what little is left for me.

# Prerequisites beyond installing Nix itself<a id="sec-1"></a>

## Access to private repository<a id="sec-1-1"></a>

The top-level flake pulls in a `shajra-private` repository that is by default set to an empty flake. When using `shajra-provisioning` with real machines, I override this input with a repository that provides modules for each of my hosts containing more sensitive configuration. You can see this overriding for various development shell commands defined in the top-level `flake.nix` file.

Note, I access this repository via SSH within a private network. Since system-level provisioning is done via `sudo`, SSH needs to be configured for both root as well as my normal user.

It would be nice to configure keys and SSH configuration with Nix, but that would just lead to a bootstrapping paradox.

If you're referencing my configuration to make your own, it's possible you may encounter a few missing settings provided by my private repository. There is no requirement to use private repositories. You can put all your configuration in one project.

## Git repository ownership<a id="sec-1-2"></a>

Git now protects commands from executing on repositories not owned by the user. This can cause problems when running commands with `sudo`, which is a requirement for system-level configuration with Nix-darwin.

The development shell environment provides a `project-bootstrap` command to configure `/var/root/.gitconfig` (only for macOS) to consider the cloned repository of `shajra-provisioning` safe to run as the `root` user.

## Homebrew installation<a id="sec-1-3"></a>

For macOS, Nixpkgs provides every terminal application I want, but there are a few GUI applications we have to get elsewhere. For this reason, Home Manager suppports orchestrating [Homebrew](https://brew.sh/) package installation, which this project takes advantage of (see [../machines/modules/darwin](../machines/modules/darwin)).

However, this means I need to install Homebrew as well as Nix as a prerequisite for provisioning with this project. I just use the simple terminal command to install it as recommended from the Homebrew site.

# Other little things<a id="sec-2"></a>

I don't want to make this document a giant list of things all the settings I set manually. Nix manages most things that matter. Everything else is either frivolous, or changes too dynamic to care to capture.

Here's some remaining miscellany:

-   The "notify-time" script won't work until "terminal-notifier" is enabled in "System Preferences -> Notifications."
-   See subdirectories for more notes on manual configuration.
