---
title: Creating CI jobs dynamically in GitHub
date: 2023-01-15
---

This a neat trick I learnt the other day while I was writing some automation for my [out-of-tree QMK builder](https://github.com/agustinmista/qmk_playground) project. There, I have firmware files for different keyboards in a folder called `keyboards`:

```
Makefile
keyboards
  |-- preonic
  |     |-- keymap.c
  |     |-- config.h
  |     |-- rules.mk
  |     \-- env
  \-- thekey_v2
        |-- keymap.c
        |-- config.h
        |-- rules.mk
        \-- env
```

The content of these folders is not important today (but maybe soon). What's relevant here is that I have a Makefile that builds a given firmware by passing the `KBD` variable with the folder where it's defined:

```bash
$ make KBD=preonic
$ make KBD=thekey_v2
```

Now, if I want to build and publish these firmares in CI, I could simply do one after another, something like:

```yaml
on: [push]

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - name: Checkout repository
        uses: actions/checkout@v3

      - name: Build preonic firmware
        run: make KBD=preonic

      - name: Build thekey_v2 firmware
        run: make KBD=thekey_v2

      - name: Create artifact
        uses: actions/upload-artifact@v2
        with:
          name: firmwares
          path: |
            build/*.bin
            build/*.hex
```

The problem with this approach is that I need to remember to go and change the CI workflow everytime keyboard N+1 suddenly appears. What we want instead is to run something like:

```bash
for kbd in $(ls keyboards); make KBD=$kbd; done
```

One interesting way to do this is to split the workflow into:

1. A `find-targets` job that "discovers" which jobs to run and saves them in an output named `targets`.
2. A `build` job that reads these `targets` and uses the [`matrix` strategy](https://docs.github.com/en/actions/using-jobs/using-a-matrix-for-your-jobs) to run them in parallel.

This looks like:

```yaml
on: [push]

jobs:

  find-targets:
    runs-on: ubuntu-latest
    outputs:
      targets: ${{ steps.set-targets.outputs.targets }}
    steps:
      - uses: actions/checkout@v2

      - id: set-targets
        run: echo "targets=$(ls keyboards | jq -R '[.]' | jq -s -c 'add')" >> $GITHUB_OUTPUT

  build:
    needs: find-targets
    runs-on: ubuntu-latest
    strategy:
      matrix:
        KBD: ${{ fromJson(needs.find-targets.outputs.targets) }}
    steps:
      - name: Checkout repository
        uses: actions/checkout@v3

      - name: Build firmware
        run: make KBD=${{ matrix.KBD }}

      - name: Create artifact
        uses: actions/upload-artifact@v2
        with:
          name: ${{ matrix.KBD }}
          path: |
            build/*.bin
            build/*.hex
```

There are a couple of things worth mentioning here:

1. The actual build step is more complicated as it uses the official `qmkfm/qmk_cli` Docker image to build the firmwares. The code I show here is deliberately simpler to show the idea.

2. We need the `build` job to depend on `find-targets` so they run in the correct order. This is easy to enforce by adding `needs: find-targets` as shown above.

3. In `find-targets`, we need to create a JSON array of targets, e.g. `["preonic","thekey_v2"]`. For this, the `set-targets` step first lists the files under `keyboards` and progressively add them to an empty array using good 'ol `jq`. This array is then saved to the `GITHUB_OUTPUT` environment variable associated with this step. Finally, this job retrieves the targets from the output of the `set-targets` and assigns it to the job's output `targets`. For reference, this is the new and cool way to do this now that [`save-output` is getting deprecated](https://github.blog/changelog/2022-10-11-github-actions-deprecating-save-state-and-set-output-commands/).

4. In `build`, we define the build matrix by retrieving the `targets` variable from output of `find-targets`. GitHub will then run the `build` action once per target, instantiating the `matrix.KBD` variable with the current target name, which we use later to call `make` accordingly.


With this in place, GitHub will create `build` jobs dinamycally on push, and there's no need to hardcode build targets anywhere :)