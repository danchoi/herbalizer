# herbalizer

A Haml to Erb translator, written in Haskell.

This program converts HAML templates to ERB. 


## Requirements

The GHC Platform

## Install

    cabal install 

## Usage

    herbalizer < test.haml 


## Caveats

herbalizer can deal with many of the most commonly used features of HAML's
syntax.  Mainly, it will handle the widely used HAML syntax features for tags,
classes, ids, attributes, Ruby blocks and expressions, and inline <script>
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
1. Ruby Interpolation #{} within plain text
1. Conditional /[] comments
1. Haml Comments
1. Doctype !!! directives

