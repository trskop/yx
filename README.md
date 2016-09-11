Y repeat X?
===========

Environment isolator and workflow simplification tool.

* Isolate your development/build/etc. environment, and switch between different
  environments easily.

* Simplify your workflow by providing intuitive commands that do the boring
  parts for you.


Terminology
-----------

* **Project** --- is a unit identified by a name and its root directory.
  Anything that is inside that directory is considered a project. For example
  application source code.

* **Environment** --- is a set of tasks necessary for setting up tooling for
  development/building/etc. One **Project** can have multiple **Environments**.
  For example when the same project is built with multiple versions of of a
  compiler, then one environment is a configuration for a specifc compiler
  version.


What YX Is Not
--------------

* **YX is not a build system.** Its purpose is to just simplify build system
  invocation without the need to always create a specialized build script that
  handles all the build system options. If you know Travis CI then `yx.yaml`
  will remind you of its configuration file, but `yx.yaml` is not a
  "executable" script.

* **YX is not a package system.**

* **YX is not a scriptiong language.**


How Does YX Work
----------------

When `yx cd PATTERN` is executed, then it will lookup `PATTERN` in its internal
database. If it succeeded in find one project then it will do the following:

1. Generate a shell `*rc` script(s) that will be used when invoking a shell
   within an isolated environment.

2. Create a bunch of symlinks to its self. These behave as an advanced shell
   `alias`.

3. It will cleanup environment variables, and also add following variables:

    - `YX_ENVIRONMENT` --- Contains the name of project environment.

    - `YX_ENVIRONMENT_DIR` --- Path to `${YX_STUFF}/${YX_ENVIRONMENT}`.

    - `YX_EXE` --- Absolute file path of YX executable that is being used.

    - `YX_INVOKED_AS` --- Alias (symbolic link file name) used for executing
      YX executable.

    - `YX_PROJECT` --- Name of the project for which the isolated environment
      is being created.

    - `YX_PROJECT_ROOT` --- Absolute path to project directory.

    - `YX_STUFF` --- Path to a directory where YX stuff for current project is
      stored.

    - `YX_VERSION` --- Version of YX that is being used.

4. When all the above is done, YX will invoke a shell executable with the
   modified environment, and it will pass generated `*rc` file to it as a
   provile script.

If `PATTERN` is not found in the YX database and it is an existing directory,
YX will assume that user is adding new project in to its known list of
projects. This will trigger YX project initialization.

**TODO:**

* Plan is to make shell selection fully customizable which will include
  template files for `*rc` scripts.

* Support for shell completion of YX commands.

* Modification of environment variables will be configurable from `yx.yaml`
  file.

* Fully customizable command aliasing. Meaning, all the symbolic links created
  to YX, in the `${YX_ENVIRONMENT_DIR}/bin` directory, will be defined in
  `yx.yaml`. This feature should include customization of shell completion.

* Provide customized environment for editor/IDE. For example customized lint,
  coding style formatting, preformatted Git commit messages, etc.

  This is just an idea that should work, but no investigation in this area was
  done, so far.

* Simplified release process and project version management. Another idea that
  migt be useful to explore.

* Support for running isolated environment inside a container (Docker or rkt).
  This shouldn't be so hard.

* Support for storing output of "wrapped" commands for further analysis. For
  example, one may want to use build error to try automatically fix the issue.

* Detect presence of right development tools, e.g. `cmake >=3.0.2`.

* Command hooks. For "binaries" with `type: command` we can provide "pre" and
  "post" hooks.


Usage
-----

```
yx cd [PATTERN] [-e ENVIRONMENT]    -- Jump in to an isolated environment.

yx ls [PATTERN]
```

**TODO:**

Create new project from a template:

```
yx init [TEMPLATE]
```

Create a new module/file/code-snippet:

```
yx new {module|file|snippet} [PATH]
```


Technologies
------------

Notes on what things would be good to use:

* optparse-applicative
* shake
* overloaded-records


User Configuration and Data Files
---------------------------------

**NOT YET FULLY IMPLEMENTED.**

```
${HOME}/
|-- .bash_yx
`-- .config/yx/
    |-- global.yaml
    `-- data.db
```


YX Project Artifacts
--------------------

**NOT YET FULLY IMPLEMENTED.**

```
${YX_PROJECT_ROOT}/
|-- yx.yaml
`-- .yx-stuff/
     |-- global/
     |   |-- bash/
     |   |   `-- bashrc
     |   |
     |   `-- git/
     |       `-- git.config
     |
     |-- env/
     |   `-- ${YX_ENVIRONMENT}/
     |       |-- bash/
     |       |   |-- bashrc
     |       |   `-- completion
     |       |
     |       |-- bin/
     |       |   |-- yx --> ${YX_EXE}
     |       |   `-- ...
     |       |
     |       `-- git/
     |           `-- git.config
     |
     `-- cache/
         `-- config.bin
```

When YX is invoked using `yx cd [PATTERN] [-e ENVIRONMENT]` then `PATH`
environment variable is modified, and
`"${YX_PROJECT_ROOT}/.yx-stuff/env/${YX_ENVIRONMENT}/bin"` is as the first
entry. When no specific environment is specified then `"_default"` is used.

Command `yx cd` invokes shell. Currently only Bash is supported, and it's
invoked as:

```
bash --rcfile "${YX_PROJECT_ROOT}/.yx-stuff/bash/bashrc"
```

Script `"${YX_PROJECT_ROOT}/.yx-stuff/bash/bashrc"` does:

* Loads user `.bashrc` or other appropriate profile script. I.e. all the user
  settings will be preserved.
* Adds YX specific functions in to your interactive environment.


YX Project Configuration File
-----------------------------

**NOT YET FULLY IMPLEMENTED.**

```yaml
# Source Code Management (SCM) tool used by the project.
# Currently only 'Git' is recognized automatically.
scm: "Git"

# Build tool used by the project.
# Currently only 'Cabal' and 'Stack' are recognized automatically.
build-tool: Stack

global:
  env:
    # User can override these variables in "${HOME}/.bash_yx", that is usually
    # necessary on systems with different installation paths for these tools.
    YX_STACK_EXE: /usr/bin/stack
    YX_GIT_EXE: /usr/bin/git

  bin:
    stack:
      type: command
      command: ${YX_STACK_EXE}

    git:
      type: command
      command: ${YX_GIT_EXE}
      env:
        #GIT_MERGE_VERBOSITY: 5

  build-tool:
    stack:
      version: >=1.1

  scm:
    git:
      version: >=1.9

      config:
        #core.editor: vim
        #commit.template: ${YX_PROJECT_ROOT}/dev-tools/git-commit.template
        #core.pager: less
        #merge.ff: only
        #pull.rebase: true
        #color.ui: true

      hooks:
        # Committing-Workflow Hooks
        #pre-commit:
        #prepare-commit-msg:
        #commit-msg:
        #post-commit:

        # Email Workflow Hooks
        #applypatch-msg:
        #pre-applypatch:
        #post-applypatch:

        # Other Client Hooks
        #pre-rebase:
        #post-rewrite:
        #post-checkout:
        #post-merge:
        #pre-push:
        #pre-auto-gc:

# Environments for this project.
environment:
  # Execution environment named "default". It is used when there is no
  # environment specified on the command line, due to "is-default: true".
  default:
    is-default: true

    # Add or modify environment variables of the isolated execution
    # environment.
    env:
    #  PATH: "/some/path/bin:${PATH}"

    # Add following commands/executables in to the isolated execution
    # environment.
    #
    # When creating symlink, the command has always have to be absolute path to
    # the executable.
    bin:
      #stack:
      #  type: command
      #  command: stack
      #  env:
      #    STACK_YAML: ${YX_PROJECT_ROOT}/stack-production.yaml
      #
      #build:
      #  type: command
      #  command: ${YX_ENVIRONMENT_DIR}/bin/stack build
      #  pre-hook:
      #    command: ${YX_PROJECT_ROOT}/dev-tools/pre-build-hook
      #    env: {}
      #  post-hook:
      #    command: ${YX_PROJECT_ROOT}/dev-tools/post-build-hook
      #    env: {}
      #
      #alex:
      #  type: symlink
      #  command: /opt/alex/3.1.7/bin/alex
      #
      #happy:
      #  type: symlink
      #  command: /opt/happy/1.19.5/bin/happy
      #
      #hoogle:
      #  type: alias
      #  command: "stack exec hoogle --"
```
