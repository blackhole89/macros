A more powerful C/C++ macro preprocessor
========================================

The venerable C preprocessor (cpp) – the part of the compilation process that interprets hash-prefixed directives such as `#include` and `#define`, and substitutes for the macros defined by the latter – is undoubtedly one of the backbones of the C/C++ ecosystem. However, its macro functionality suffers from [known limitations](https://gcc.gnu.org/onlinedocs/cpp/Self-Referential-Macros.html): macros may not call themselves recursively, meaningfully work with mutable state or introduce syntax that does not obey the shape of either single keywords or function calls. This severely limits its utility for metaprogramming, necessitating the proliferation of idiosyncratic boilerplate-generation tools of high complexity but limited scope such as [Yacc](https://en.wikipedia.org/wiki/Yacc) or Qt's [moc](http://doc.qt.io/archives/qt-4.8/moc.html).

This project is an attempt to create a Turing-complete general-purpose preprocessor for C and C++ that is powerful enough to subsume all of the above and more: indeed, we shall aim to be able to build on top of either C or C++ in the way the latter was originally build upon the former, while seamlessly blending in with existing code as the C preprocessor does. 

We draw significant inspiration from [Rust's macro system](https://doc.rust-lang.org/nightly/book/second-edition/appendix-04-macros.html), which appears to be the most ambitious such effort this side of LISP, without binding ourselves to its sometimes curious choice of syntax or its demand of [hygiene](https://en.wikipedia.org/wiki/Hygienic_macro) (this is C, after all!). Since all respectable programming language projects are self-hosting and this is not one of them respectable programming language projects, the preprocessor itself is written in [Haskell](https://en.wikipedia.org/wiki/Haskell).

Example
-------

```c++
struct {
    int value;
    LinkedList *next;
} LinkedList;

// define recursive macro to create a linked list
@define MakeList {
    ( {@^[,]$head, @^$tail} ) => (
        new LinkedList( {$head, MakeList {$tail}} )
    )
    ( {@^[,]$singleton} ) => (
        new LinkedList( {$singleton, NULL} )
    )
    ( {} ) => ( NULL )
}

// create a linked list with 5 elements
LinkedList *l = MakeList {1,2,3,4,5};
```

Introduction
------------

The fundamental principle of <macros> is **keyword-triggered pattern-matching and substitution on token streams**. A typical macro definition takes the following form:
```c++
@define macroname {
    ( pattern one ) => ( printf("first pattern encountered") )
    ( pattern two ) => ( printf("second pattern encountered") )
    ( pattern three) => ( printf("third pattern encountered") )
    // ...
}
```
This defines a macro that is triggered by encountering the keyword `macroname` *anywhere in the program text following the definition*. 

