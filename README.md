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
yx cd [PATTERN]                 -- Jump in to an isolated environment.

yx ls [PATTERN]
```

TODO:

```
yx init [TEMPLATE]              -- Create directory structure for a new project.

yx new module [MODULE_NAME]     -- Create new module.
```


Technologies
------------

* optparse-applicative
* shake
* overloaded-records


Configuration Files
-------------------

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

Bash is invoked as:

```
bash --rcfile "${YX_PROJECT_ROOT}/.yx-stuff/bash/bashrc"
```
