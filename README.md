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

* **Environment** --- is a set of task necessary for setting up tooling for
  development/building/etc. One **Project** can have multiple **Environments**.
  For example when the same project is built with multiple versions of
  of a compiler then environment is a configuration for just one of them.


What YX Is Not
--------------

* **YX is not a build system.** Its purpose is to just simplify build system
  invocation without the need to always create a specialized build script that
  handles all the build system options. If you know Travis CI then `yx.yaml`
  will remind you of its configuration file.

* **YX is not a package system.**


How Does YX Work
----------------

When `yx cd PATTERN` is executed, then it will lookup `PATTERN` in its internal
database. If it succeeded in find one project then it will do the following:

1. Generate a shell `*rc` script(s) that will be used when invoking a shell
   with an isolated environment.

2. Create a bunch of symlinks to its self. These behave as an advanced shell
   `alias`.

3. It will cleanup environment variables, and it will followint variables:

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

**NOT YET IMPLEMENTED.**

```
${HOME}/.config/yx/global.yaml
${HOME}/.config/yx/data.db
```


YX Project Artifacts
--------------------

```
${YX_PROJECT_ROOT}/
|-- yx.yaml
`-- .yx-stuff/
     |-- env/
     |   `-- ${YX_ENVIRONMENT}/
     |       |-- bash/
     |       |   |-- bashrc
     |       |   `-- completion
     |       |
     |       `-- bin/
     |           |-- build --> ${YX_EXE}
     |           |-- ghci --> ${YX_EXE}
     |           |-- repl --> ${YX_EXE}
     |           |-- run --> ${YX_EXE}
     |           `-- stack --> ${YX_EXE}
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

**NOT YET IMPLEMENTED.**

```yaml
# Recognized value of scm field is currently just "git".
scm: Git

# Recognized values of build-tool are "stack", and "cabal".
build-tool: Stack

# #############################################################################

environment:
  - ghc8:
      env:
        PATH: "/opt/ghc/8.0.1/bin:${PATH}"
        STACK_YAML: "${XY_PROJECT_ROOT}/stack-nightly.yaml"```
```
