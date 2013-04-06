# herbalizer

A HAML to ERB translator, written in Haskell.

This program converts [HAML](http://haml.info/) templates to [ERB](http://www.stuartellis.eu/articles/erb/).

## Requirements

The GHC Platform

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

    or

    herbalizer test.haml


## Caveats

herbalizer can deal with the most commonly used features of HAML's
syntax.  Mainly, it will handle the widely used HAML syntax features for tags,
classes, ids, attributes, Ruby blocks and expressions, and inline `<script>`
content.

I wrote it so that I could automate at least 90% of work I need to do to
translate HAML templates to ERB. YMMV.

`herbalizer` can't yet recognize some less commonly used HAML contructs such as 

1. HTML-style () Attributes
1. :class and :id attributes specified as a Ruby array
1. Filters besides `:javascript` and `:erb`; these are just rendered inside pseudo-tags named after the filter
1. Whitespace preservation sytnax
1. Escaping HTML &= syntax
1. Unescaping HTML != syntax
1. Multiline | syntax
1. Ruby Interpolation #{} within plain body text; passed through
1. Conditional /[] comments; passed through; normal / comments OK
1. HAML Comments (#-) not processed
1. XML Doctype (!!! XML) declarations; HTML Doctype declarations are supported
1. Single-line Ruby expressions that are purely for side effect and not for output; an extra <% end %> will be created. (This is bad practice anyway)

Eventually I hope to cover all the edge cases. As you can see, HAML syntax is
not compact. It's getting more baroque over time, and so is the Ruby parser
that powers it. I like ERB for this reason. It's simple, classic, and won't
change. I like angle brackets.


