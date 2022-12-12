---
title: Works on my machine™
date: 2022-12-12
---

I've been having issues compiling LaTeX projects that use Lhs2TeX+pdfTeX+BibTeX in a reproductible manner between my workstation and GitHub's CI, and while there are a bunch of solutions for building LaTeX in CI and VSCode without much hassle, I couldn't quite find the way to make them work with Lhs2Tex in the pipeline. To solve this, I decided to homogenize the build environment using a Docker image, the same one everywhere. This works pretty well but turned out to be a bit nuanced, so these are the instructions for future me.

The setup consists of three parts:

1. A [Docker image](https://hub.docker.com/r/agustinmista/latex-lhs2tex) with `texlive-full` and `lhs2tex` preinstalled. It gets built and deployed automatically to DockerHub using a [GitHub action](https://github.com/agustinmista/latex-lhs2tex-docker/blob/main/.github/workflows/docker.yml).
2. A GitHub action inside my project that pulls the Docker image (1) and uses it to build the LaTeX documents (via `make`), exporting all the generated PDFs as an artifact.
3. A couple of config files to instruct VSCode to open my project inside the same Docker image (1) and rebuild the documents after saving a modified source file.

Let's take a deeper look at each part now.

## Build environment

The LaTeX pipeline I use is relatively simple: a simple Makefile preprocesses the root `.lhs.tex` files of my project, producing some gigantic `.tex` files that get compiled with the usual `pdflatex`+`bibtex`+`pdflatex`+`pdflatex` nonsense. I wrote [this Dockerfile](https://github.com/agustinmista/latex-lhs2tex-docker/blob/main/Dockerfile) to automate the process of installing all these dependencies.

For simplicity, I use a GitHub action to build and [deploy this image to DockerHub](https://hub.docker.com/r/agustinmista/latex-lhs2tex) remotely on every new push. This is quite straightforward to set up using some existing actions:

**.github/workflows/docker.yml**:
```yaml
name: Docker Image CI

env:
  IMAGE_NAME: latex-lhs2tex

on:
  push:
    branches: [ main ]

jobs:
  build:

    runs-on: ubuntu-latest

    steps:
    - uses: actions/checkout@v3

    - name: Login to Docker Hub
      uses: docker/login-action@v1
      with:
        username: ${{ secrets.DOCKERHUB_USER }}
        password: ${{ secrets.DOCKERHUB_TOKEN }}

    - name: Set up Docker Buildx
      uses: docker/setup-buildx-action@v1
      with:
        driver: docker-container
        driver-opts: |
          image=moby/buildkit:master
          network=host

    - name: Build and push
      uses: docker/build-push-action@v2
      with:
        context: ./
        file: ./Dockerfile
        builder: ${{ steps.buildx.outputs.name }}
        push: true
        tags: ${{ secrets.DOCKERHUB_USER }}/${{ env.IMAGE_NAME }}:latest
        cache-from: type=registry,ref=${{ secrets.DOCKERHUB_USER }}/${{ env.IMAGE_NAME }}:buildcache
        cache-to: type=registry,ref=${{ secrets.DOCKERHUB_USER }}/${{ env.IMAGE_NAME }}:buildcache,mode=max
```

These definitions live in a [separate repository](https://github.com/agustinmista/latex-lhs2tex-docker) so I can reuse it for different LaTeX projects. The `secrets.DOCKERHUB_USER` and `secrets.DOCKERHUB_TOKEN` are the credentials used to login into DockerHub and need to be set using the web UI. Finally, in the build step it's important to setup the `cache-from` and `cache-to` so we can additionally cache the intermediate image layers in DockerHub, and our future commits don't take an eternity to rebuild.

## GitHub CI action

With the build environment already deployed to DockerHub, the next step is to create a CI action in the project's repo that pulls the image and runs `make` inside the build environment:

**.github/workflows/latex.yml**:
```yaml
name: Build LaTeX documents

env:
  BUILD_ENV_IMAGE: agustinmista/latex-lhs2tex

on: [push]

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - name: Set up Git repository
        uses: actions/checkout@v3
        with:
          path: repo

      - name: Fix file permissions in repo
        run: |
          chmod -R 777 repo
          sudo chown -R 1000:1000 repo

      - name: Pull custom Docker image
        run: docker pull ${{ env.BUILD_ENV_IMAGE }}

      - name: Compile LaTeX documents using custom Docker image
        run: |
          docker run \
            -v ${{ github.workspace }}/repo:/home/docker/workdir \
            ${{ env.BUILD_ENV_IMAGE }} \
            -c "make all"

      - name: Upload PDF file
        uses: actions/upload-artifact@v3
        with:
          name: artifact
          path: repo/*.pdf
```

A little nuance here is that I'm building the build enviroment image using the usual `uid:gid=1000:1000` so build containers can modify and create files in the host files without screwing up their permissions. To my surprise, GitHub does **not** follow this, and the action `runner` user has instead `uid:gid=1001:121`. I've seen that some projects solve this by building Docker images to be used by GitHub runners using those specific `uid:gid` values, but this approach would probably not work if we want to also use the image later to build the project locally. The easiest way to solve it is to `chown`+`chmod` the cloned project folder `repo` so they belong to the build container user, but everyone can access them (this is needed by the last step to find and export the generated PDFs as an artifact).

## VSCode setup

Now it's time to bring the same build environment we used in CI to my local machine, so I don't have to worry anymore about reproductibility. The easiest way to do it in VSCode is by using a [development container](https://code.visualstudio.com/docs/devcontainers/containers). We do this by creating the following file in our project's repository:

**.devcontainer/devcontainer.json**:
```json
{
  "image": "agustinmista/latex-lhs2tex",
  "customizations": {
    "vscode": {
      "extensions": [
        "mathematic.vscode-pdf",
        "Gruntfuggly.triggertaskonsave"
      ]
    }
  }
}
```

The important bit here is that we're telling VSCode that this project is meant to be developed inside the [agustinmista/latex-lhs2tex](https://hub.docker.com/r/agustinmista/latex-lhs2tex) Docker image.


Additionally, we can customize VSCode with specific extensions to be enabled for this particular project. Here I'm including the "Trigger Task on Save" extension that let us run VSCode tasks when we save specific files. These tasks are specified under `.vscode/tasks.json`. In our case, we can define a `make` task that simply runs `make all`:

**.vscode/tasks.json**:
```json
{
  "version": "2.0.0",
  "tasks": [
    {
      "label": "make",
      "type": "shell",
      "command": "make all",
      "problemMatcher": [],
      "group": {
        "kind": "build",
        "isDefault": true
      }
    }
  ]
}
```

To make this work, we now need to configure the extension to run the `make` task when we save some modified source file. This can be done by adding the following to the repo's local `.vscode/settings.json`:

**.vscode/settings.json**:
```json
{
  "triggerTaskOnSave.tasks": {
    "make": [
      "*.lhs.tex",
      "notation.fmt",
      "references.bib",
      "path/to/other/source/files/*.tex"
    ],
  },
  "triggerTaskOnSave.showNotifications": true,
  "triggerTaskOnSave.on": true,
  "triggerTaskOnSave.restart": true
}
```

And that's it. Next time we open the project, VSCode should ask us if we want to reopen it inside our build environment. Click "Yes" and carry on looking for those `missing $ inserted`.
