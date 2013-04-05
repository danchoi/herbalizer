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

herbalizer can deal with many of the most commonly used features of HAML's syntax. 

But it can't yet recognize some less commonly used HAML contructs such as 

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

