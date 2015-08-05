

# Synopsis #

epm-mode is a tool for project managers, based on [EmacsMuse](http://mwolson.org/projects/EmacsMuse.html), which implements the [Scrum](http://www.scrumalliance.org/) methodology.

For more information see [Manual](Manual.md).

# Directory contents #

The directory structure is similar to [Maven](http://maven.apache.org/) projects:

  * `src/main/lisp` epm source code.
  * `src/main/config` configuration files examples
  * `src/test/lisp` epm tests code
  * `src/test/resources` resources used in tests
  * `src/site/muse` epm documentation


# Getting started #

## Prerequisites ##

You need Emacs (21.1 or greater), muse, and gnuplot. For tests you
need also ert.


## Installation ##

Copy the source code directory into your file hierarchy. I use
/local/share/emacs/epm  directory for this project. Optionally you can
compile the sources.



## Configuration ##

Create a directory for yours projects and a sub-directory named day for
days pages. See src/main/config/epm-config.el for suggestions to what
you must add to .emacs

epm-mode is also a Muse project and if you want to publish your plans
you must add it to muse-project-alist:

```
("WorkPlan" ("~/work/plan" "~/work/plan/day"
             :default "index"
             :major-mode epm-mode)
 (:base "html" :path "/home/www/work/plan"))
```



# License #

epm-mode is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

epm-mode is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.


