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


Usage
-----

```
yx cd [PATTERN] [-e ENVIRONMENT]    -- Jump in to an isolated environment.

yx ls [PATTERN]
```

TODO:

```
yx init [TEMPLATE]              -- Create directory structure for a new project.

yx new module [MODULE_NAME]     -- Create new module.
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
```

```
${HOME}/.cache/yx/data.db
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
