# herbalizer

A HAML to ERB translator, written in Haskell.

This program converts [HAML](http://haml.info/) templates to [ERB](http://www.stuartellis.eu/articles/erb/).

## Why

HAML syntax may look more compact, but on a mnemonic level, HAML is
not very [compact](http://www.faqs.org/docs/artu/ch04s02.html). You will often require 
the syntax reference on hand to write or decipher it.

ERB is simpler, easier to remember the rules for, and more accessible to anyone
familiar with HTML.  ERB templates are easier to teach to beginners and easier
to collaborate on with a wider range of people, like designers. And for some people ERB is 
easier on the eyes.

## Requirements

The GHC Platform, unless you're using a precompiled binary.

## Install

From the git project directory,

    cabal install 

## Quick setup for Mac OS X Users

I've precompiled the executable for OS X (10.8). You can download it and `chmod +x` it:

    wget http://openmbta.org/herbalizer
    chmod +x herbalizer

And then put it on your PATH.

## Usage

    herbalizer < test.haml 

You can also pass filenames as arguments:

    herbalizer test.haml

## Caveats

**herbalizer** can deal with the most commonly used features of HAML's
syntax.  Mainly, it will handle the widely used HAML syntax features for tags,
classes, ids, attributes, Ruby blocks and expressions, and inline `<script>`
content.

I wrote it so that I could automate at least 90% of work I need to do to
translate HAML templates to ERB. YMMV.

**herbalizer** can't yet recognize some less commonly used [HAML
constructs](http://haml.info/docs/yardoc/file.REFERENCE.html) such
as 

1. HTML-style () attributes e.g. `%a(title=@title href=href)`
1. :class and :id attributes specified as a Ruby array
1. Filters besides `:javascript` and `:erb`; these are just rendered inside pseudo-tags named after the filter
1. Whitespace removal sytnax
1. Object reference syntax
1. Escaping HTML &= syntax
1. Unescaping HTML != syntax
1. Multiline | syntax
1. Ruby Interpolation #{} within plain body text; passed through
1. Conditional /[] comments; passed through; normal / comments OK
1. HAML Comments (#-) not processed
1. XML Doctype (!!! XML) declarations; HTML Doctype declarations are supported
1. Single-line Ruby expressions that are purely for side effect and not for output; an extra <% end %> will be created. (This is bad practice anyway)

Eventually I hope to cover more of these edge cases. 


