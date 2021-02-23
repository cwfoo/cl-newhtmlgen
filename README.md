# cl-newhtmlgen
A Common Lisp library for generating HTML.

This is a fork of htmlgen from
[Portable AllegroServe](https://sourceforge.net/projects/portableaserve/).
Reason for fork: to add support for HTML5 tags.


## Improvements over Portable AllegroServe's htmlgen
Changes made by cl-newhtmlgen:
* Add support for HTML5 tags.
* Replace Allegro Common Lisp's `if*` macro with `if`, `cond`, and `when`.
  This removes any dependency on other libraries.
* Allow the unit tests to be run using ASDF.
* Rename `*.cl` files to `*.lisp`, so that the ASDF `defpackage` `:components`
  field can find the source file.
* Reindent the source code; replace tab indentation with space-only indentation.


## Documentation
The documentation is in the `doc/` directory.

Important note: the package name for the functions provided by cl-newhtmlgen is
`newhtmlgen`. The examples in the documentation use the package name
`net.html.generator`. When using cl-newhtmlgen, use the package name
`newhtmlgen` instead of `net.html.generator`.

### Tips
To always output tags and tag attributes in lowercase, set `*print-case*` to
`:downcase`. For example:

```lisp
(let ((*print-case* :downcase))
  (with-output-to-string (port)
    (newhtmlgen:html-print-list
      '(((:p :class "summary") "It was a " (:em "great")  " day."))
      port)))
;; Returns: "<p class=\"summary\">It was a <em>great</em> day.</p>"
```


## Development
To run the unit tests: `(asdf:test-system :newhtmlgen)`.

The library has been tested using SBCL 2.0.1 on Ubuntu 20.04.1.

Project started on 2021-02-06.


## Credits
This is a fork of htmlgen from
[Portable AllegroServe](https://sourceforge.net/projects/portableaserve/).
In turn, Portable AllegroServe is a fork of Franz Inc.'s
[AllegroServe](https://github.com/franzinc/aserve).

This project based on htmlgen from Portable AllegroServe's commit cac1d6920998
(2019-07-20), and includes several patches obtained from Franz Inc.'s
AllegroServe..


## License
The source code is licensed under the terms of the Lisp Lesser GNU Public
License, known as the LLGPL. The LLGPL consists of a preamble
(see LICENSE-allegroserve) and the LGPL (see LICENSE-lgpl).
Where these conflict, the preamble takes precedence.


## Contributing
Bug reports, suggestions, and patches should be submitted on GitHub:
https://github.com/cwfoo/cl-newhtmlgen
