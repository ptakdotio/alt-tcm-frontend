# Gimme TCM Movies

## What is this?

This is a simple program for checking what movies are available to watch on
TCM. It uses TCM's API, but adds filters so you can pare down the list.

## Why did you make this?

I find TCM's web interface a little bit cumbersome to use. You can't filter on
every available genre or by release year, and clicking on a movie takes you to
a new webpage. I would rather just see a filterable, sortable list of what is
available, which is what this script does.

## How do I use it?

The simplest invocation is simply `python gimme-tcm-movies.py`. This will print
out a summary of all movies available to watch, including the title,
description, and year they were released.

You can filter the list based on genre and year of release. For example, I like
to watch Crime, Mystery, and Thriller movies from the 1930s and 40s. I invoke
the script like this:

```
$ python gimme-tcm-movies --after-year=1930 --before-year=1950 --genre=Crime,Thriller,Suspense/Mystery
```

Use `--list-genres` to see all the valid genre names.

You can also use the option `--html` to generate an HTML table summary.

## Why isn't there an option to filter by such-and-such?

Go ahead and open an issue! I plan on adding lots more options to this program
in case anyone finds them useful.

