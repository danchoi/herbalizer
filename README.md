# herbalizer

A HAML to ERB translator, written in Haskell.

This program converts HAML templates to ERB. 


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


## Caveats

herbalizer can deal with many of the most commonly used features of HAML's
syntax.  Mainly, it will handle the widely used HAML syntax features for tags,
classes, ids, attributes, Ruby blocks and expressions, and inline `<script>`
content.

I wrote it so that I could process the sorts of HAML templates that I
personally deal with.  YMMV.

But as this is an alpha release, `herbalizer` can't yet recognize some less
commonly used HAML contructs such as 

1. HTML-style () Attributes
1. :class and :id attributes specified as a Ruby array
1. Filters besides `:javascript`; these are just rendered inside HTML tags named after the filter
1. Whitespace preservation sytnax
1. Escaping HTML &= syntax
1. Unescaping HTML != syntax
1. Multiline | syntax
1. Ruby Interpolation #{} within plain text; pass through
1. Conditional /[] comments; passed through
1. Doctype !!! directives; passed through



