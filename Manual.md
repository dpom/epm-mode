

This manual documents the epm-mode package, version 1.0

Copyright (C) 2008-2009 Dan Pomohaci

Permission is granted to copy, distribute and/or modify this document
under the terms of the GNU Free Documentation License, Version 3 or
any later version published by the Free Software Foundation; with no
Invariant Sections, no Front-Cover Texts, and no Back-Cover Texts.


# Introduction #

I am an old emacs user and I try to use it for (almost)
everything. Muse-mode is excellent for writing documentation and
http://www.emacswiki.org/cgi-bin/wiki/PlannerMode for personal projects, but I needed a tool for managing team
projects. First I tried to bend planner to my needs but it didn't work
and I took an other approach by using plane muse page for
planning. Gradually I created commands for my most used tasks and
added support for scrum methodology. Now it became a complete package
used daily.


# Obtaining epm-mode #

You find the latest stable version at http://epm-mode.googlecode.com/svn/trunk/
(I use branches for development).

For svn users:

```
svn checkout http://epm-mode.googlecode.com/svn/trunk/ epm-mode
```

For git users:

```
git svn clone http://epm-mode.googlecode.com/svn/trunk/ epm-mode
```



# Getting Started #

## Prerequisites ##

You need Emacs (21.1 or greater), muse, and gnuplot. For tests you
need also ert.


## Installation ##

Copy the source code directory into your file hierarchy. I use
`/local/share/emacs/epm`  directory for this project. Optionally you can
compile the sources.



## Configuration ##

Create a directory for yours projects and a sub-directory named day for
days pages. See src/main/config/epm-config.el for suggestions to what
you must add to .emacs

epm-mode is also a Muse project and if you want to publish your projects
you must add it to muse-project-alist:

```
     ("WorkPlan" ("~/work/plan" "~/work/plan/day"
                  :default "index"
                  :major-mode epm-mode)
      (:base "html" :path "/home/www/work/plan"))
```




# Concepts #

The majority of epm concepts are borrowed from Scrum. For more
information on scrum terms see http://www.scrumalliance.org/articles/39-glossary-of-scrum-terms and http://www.scrumalliance.org/resources/339

## Feature ##

I use the generic name _feature_ for all the backlog items (also named
_stories_ or _use-cases_ in scrum). A feature has an id and a short
description (name). In epm its syntax is:

```
** 3. Add a global action delete in back-office
```

where:

  * `**`  feature identifier,
  * `3` the id and the text after
  * `.` the feature description/name.


## Resource ##

In epm-mode the term `resource` is restraint to human resources.


## Task ##

Tasks are specific actions which must be done to develop a feature. In
epm has the following syntax:

```
!3.2 "Add a delete icon in back-office" {1} (0.25) |0.75| bill 2009-01-29
```

where:

  * `!`  task indicator
  * `3`  parent feature id
  * `2`  task id
  * `text between "`  task description
  * `{1}`  initial task estimation in scrum points (ideal man-days)
  * `(0.25)`  remaining work to do estimation
  * `|0.75|` work done
  * `bill`  resource allocated
  * `2009-01-29` day(s) in which resouces worked at this task


## Product Backlog Page ##

The Product Backlog page contains all features not planned yet.


## Sprint Page ##

The sprint page contains all information needed to manage a sprint. It
is a standard muse page (see [SprintExample](SprintExample.md)) with an header and four
sections: _Resources_, _Planned_, _Working_, and, _Done_.

In header I put sprint useful information as: `startdate`, deadline,
`ework` (total estimate work), `etime` (sprint duration in days), `date` (a
comma separated list with sprint dates), `work` (a comma separated list
with work done in each day of the sprint), `burndown` (a comma separated
list with estimated remaining work for each day). These lists are used
for drawing the sprint burndown chart.

_Resources_ contains a list of resources used in this sprint. _Planned_
stores tasks planned for sprint but not started, _Working_ tasks started
but not finished, and _Done_ tasks finished. In all three section the
tasks are grouped by feature.


## Day Page ##

The day page is also a muse page (see [2009-01-08](20090108.md)) and contains tasks executed in this day. The tasks order is
random because I use `epm-copy-task-to-day` to fill day pages. The work-done
task field value indicates the work done only in this day and not the
entire work done as it is in sprint page.



# How I use epm-mode #

## Starting Scrum ##

I use a single directory for all the projects I manage:
`~/work/plan`. In this directory I create a subdirectory `day` for day
pages.



## Sprint Planning Meeting ##

First I create (using `sprintx` template) an empty sprint page for the
next sprint.  With the product owner we establish what features must
be achivied in this sprint. I move these features from product backlog
page in sprint page. With the dev team we split each feature in tasks
(create new task with `taskx` template) and put them in _Planned_
section. Using `epm-display-todo` we find the initial estimate in scrum
points (ideal man-days). Divided with team focus and team length we
obtain an estimate sprint duration. Usually the product owner is not
happy with this duration and I must intermediate negotiations between
dev team and product owner but epm-mode doesn't help me in these :(.


## Daily Scrum ##

In daily scrum meeting I use the sprint page for:

  * assigning tasks to developers - `epm-add-resource`
  * create new day page (template `dayx`), assigning day to tasks -
`epm-add-day`, and, copy tasks to day (`epm-copy-task-to-day`)
  * move tasks from planned to working (`epm-move-task-to-working`) or
from working to done (`epm-move-task-to-done`)
  * update tasks (`epm-update-task`)

Using gnuplot and `epm-display-burndown-chart` it is possible to see
graphical how the sprint evolve:

<a href='http://picasaweb.google.ro/lh/photo/7Rva2USryUjFRvti2V5PCw?feat=embedwebsite'><img src='http://lh6.ggpht.com/_WTY0XTodjjg/SYxREf0wzxI/AAAAAAAAAOM/9jAJT_JEV34/s800/sprintburndownchart.png' /></a>


# Conversions #

## From Rally ##

TODO


## To taskjuggler ##

TODO



# Keystroke Summary #

You can set the epm prefix map key with:

```
(define-key global-map (kbd "\C-c\C-p") 'epm-prefix-map)
```

Epm defines the following commands keys:

  * a  epm-move-task-to-working
  * b  epm-rally-print-project
  * c  epm-copy-task-to-day
  * d  epm-add-day
  * e  epm-rally-print-features
  * f  epm-display-focus
  * g  epm-display-burndown-chart
  * h  epm-help
  * i  epm-display-user-done
  * k  epm-rally-print-features2
  * l  epm-epm-print-all-tasks-between
  * m  epm
  * n  epm-add-nonworking-period
  * p  epm-move-task-to-planned
  * r  epm-add-resource
  * s  epm-display-remaining-estimate
  * t  epm-display-task-total-done
  * u  epm-update-task
  * v  epm-display-todo
  * w  epm-display-done
  * x  epm-move-task-to-done
  * z  epm-display-estimate