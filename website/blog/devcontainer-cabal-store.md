---
title: Persistent cabal store in Haskell devcontainers
date: 2023-01-14
---

This is going to be a short one. I recently added a [development container](https://code.visualstudio.com/docs/devcontainers/containers) definition to the repository that hosts this very website. The definition I'm using is taken from [Microsoft's devcontainer repository](https://github.com/microsoft/vscode-dev-containers/tree/main/containers/haskell) via the option integrated in Visual Studio Code[^1].

For the most part, this development container works flawelessly. Except for this: the user's cabal folder (located at `~/.cabal` by default) is not persisted across container runs. This means I need to rebuild from scratch most of the dependencies that cabal saves in its user-wide package store (`~/.cabal/store`) every time I reopen the project (notably annoying with Hakyll's dependency of big-chungus `pandoc`).

There are a couple of ways to solve this:

* By configuring cabal to use a package store located inside the repository's folder (which is bind-mounted to the host's file system).

  _Problem_: this would duplicate the work for any dependency shared across Haskell projects.

* Creating a `~/.cabal` folder in my host computer and share it among devcontainers.

  _Problem_: this folder would be continuously modified by `root`, a permission hell waiting to happen.

* Mounting the package store to a [Docker named volume](https://code.visualstudio.com/remote/advancedcontainers/improve-performance#_use-a-targeted-named-volume).

  This is what I ended up doing, as it takes the least work and doesn't clutter my host's `$HOME`.

  The only changes needed are adding the following properties to `.devcontainer/devcontainer.json`:

  ```json
  "mounts": [
      "source=cabal_store,target=/home/vscode/.cabal/store,type=volume"
    ],
  "postCreateCommand": "sudo chown vscode /home/vscode/.cabal/store"
  ```

  Where `cabal_store` is the name of the Docker volume to be created and `vscode` is the Linux user of this devcontainer. The `postCreateCommand` is there to make sure our container can access and modify the package store (only needed in case your devcontainer logs in to a user other than `root`). Repeat this for every Haskell devcontainer you want to share the cabal store with.

That's it! Signing off.

[^1]: Sadly, I found today this repo is getting deprecated in favor of [containers.dev](https://containers.dev/), and the latter doesn't yet host any devcontainer definition for Haskell.