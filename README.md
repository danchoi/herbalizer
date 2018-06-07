# herbalizer

A HAML to ERB translator, written in Haskell.

This program converts [HAML](http://haml.info/) templates to [ERB](http://www.stuartellis.eu/articles/erb/).

## Why

A lot of people prefer HAML to ERB. But not everyone, and not for every
circumstance. ERB is simpler, easier to remember the rules for, and more
accessible to anyone familiar with HTML.  ERB templates are easier to teach to
beginners and easier to collaborate on with a wider range of people, like
designers. And for some people ERB is easier on the eyes.

As far as I know, there is no other good tool to convert HAML to ERB. To
quote HAML maintainer Nathan Weizenbaum:

>  No, no such tool exists. In fact, it's not possible to make without
>  parsing Ruby code - Haml supports several constructs, such as = with a
>  block and comments within Ruby code, that ERB doesn't. It might be
>  possible to modify the Haml engine to convert a fairly large subset of
>  Haml to ERB, but it would take considerable work to even get to an
>  imperfect implementation.
>  [source](https://groups.google.com/d/msg/haml/rx6T5eLnPN0/Dr7ckyoLK5gJ)  

However, with Haskell, it is possible to write a pretty good parser-translator
for HAML markup without incorporating a full-blown Ruby parser.

## Requirements

The GHC Platform, unless you're using a precompiled binary.

* [How to install GHC on OS X](http://justtesting.org/using-the-glasgow-haskell-compiler-ghc-on-os)

## Install

    cabal update
    cabal install herbalizer

## Quick setup for Mac OS X Users

I've precompiled the executable for OS X (10.8). You can download it and `chmod +x` it:

    wget http://openmbta.org/herbalizer
    chmod +x herbalizer

And then put it on your PATH.

## OSX - Add to Path

```sudo /etc/paths```

You will need to reload your terminal for path to update. Verify the new path with...

```echo $PATH | grep Haskell```

You can also compile programs using cabal. If you are programming in Haskell with your development system setup with cabal....

Add this to your ~/.bash_profile

export PATH="$HOME/Library/Haskell/bin:$PATH"

```source ~/.bash_profile```

## Usage

    herbalizer < test.haml 

## Simple way to save file

    herbalizer < test.haml > outputfile.html.erb

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

1. HTML-style () attributes e.g. `%a(title=@title href=ahref)`
1. :class and :id attributes specified as a Ruby array, e.g. `%div{:id => [@item.type, @item.number], :class => [@item.type, @item.urgency]}`
1. Filters besides `:javascript` and `:erb`; these are just rendered inside pseudo-tags named after the filter
1. Whitespace removal sytnax
1. Object reference syntax
1. Escaping HTML &= syntax
1. Multiline | syntax
1. Ruby Interpolation #{} within plain body text; passed through
1. Conditional /[] comments; passed through; normal / comments OK
1. HAML Comments (#-) not processed
1. XML Doctype (!!! XML) declarations; HTML Doctype declarations are supported
1. Ternary Ruby expressions in attribute values are not handled robustly; surround these with parentheses
1. Nested attribute hash expressions e.g. `{:data => {:send => false, :width => 100}}` are not processable

Maybe **herbalizer** will cover more of these edge cases over time.  Let me
know in a GitHub issue if you have a particular need. I'll try to write a
patch. 


## Related reading

* [Fashion Runway: ERb vs. Haml](http://robots.thoughtbot.com/post/159805300/fashion-runway-erb-vs-haml)
* [HAML: the unforgivable sin](http://opensoul.org/blog/archives/2011/11/30/haml-the-unforgivable-sin/)
* [Why I don't like HAML](http://blog.getify.com/why-i-dont-like-haml/)
* [My thoughts on Haml and ERB](https://speakerdeck.com/klaustopher/erb)


