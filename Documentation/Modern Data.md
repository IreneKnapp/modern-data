Title: Modern Data
Author: Irene Knapp
Base Header Level: 1

# Introduction #

Modern Data is a compact, binary, dependently-typed, self-describing data format for object graphs, and also a library.

A self-describing data format is a format like XML, JSON, or Common Lisp’s s-expressions, which can be processed at a superficial level without knowledge of the specific type of data it contains.  The typical purpose of such formats is interchange across diverse software tools; for example, many tools for working with databases can produce output or accept input in XML.  They can also be useful intermediate representations for transformations on data structures, as with XSLT.  They can even be used for network protocols, as with XMPP/Jabber.  Modern Data is at least as suitable for these tasks as any existing format.

Unlike the above-mentioned alternatives, Modern Data is primarily a binary format, not a textual one.  This is because any textual format based on it would necessarily be either very verbose and hard to write, or else would give up many important features.  There is a textual format for documentation and debugging, but users are urged never to use it for interchange.

Although not the primary motivation for the choice, a benefit of being a binary format is a certain compactness.  Care has been taken to ensure that the format can in principle be used by database engines and the like as a native format - perhaps slightly larger than non-self-describing forms, but not enormously so as a textual format would be, and without needing a parse phase to perform simple actions such as scanning through it.  Specifically, the format does not use offset-based pointers, so it is suitable for embedding in other formats that do their own space allocation.

It would be difficult to discuss the specifics of Modern Data with no notation for it at all; therefore, there are in fact textual notations for it.  But parsers for these notations are not part of the library - only generators.  This should hopefully remove the temptation to use them for any purpose but documentation and standardization.

The above benefits are substantial, but the most distinctive attribute of Modern Data compared to existing formats is that it is dependently typed.  In a formal sense, this means that types and values are intermixable.  Thus the schema for a Modern Data document is itself potentially a Modern Data document, using a type as a value.  Furthermore, bounds-checked arrays, dimensioned quantities, and the like can all be expressed through values used within types, in ways that will be familiar to programmers who have used theorem-proving languages such as Agda, Coq, Epigram, and Idris.

Another benefit that is a prerequisite of dependent typing but will not be obvious to programmers who are unfamiliar with Hindley-Milner-style type systems is that types are parametric; this means, for example, that it is possible to write the incomplete type “balanced binary tree containing leaf nodes of some particular type”, without specifying the content type at the time that the tree type is defined; the type parameter can be filled in to instantiate that tree for any desired content type, thus obtaining a complete type, which can actually be instantiated.  This will be familiar to Haskell and ML programmers; it is also reminiscent of C++ and Java templates, but the meaning of it has been more rigorously formalized.

Furthermore, code can be written using the library which operates on a parametric type such as our binary tree example, generically to its specific parameters - thus code can be written which rebalances a tree after an update, without knowing the type of the contents.

Modern Data, unlike other self-describing formats, is capable of representing an arbitrary object graph - not merely one that has obvious linear structure.  This is accomplished without pointers or globally-scoped identifiers of any kind, and therefore does not impede use embedded within other formats, as mentioned above.  The benefit is that arbitrary data structures can be represented directly, including ones which would have cyclic pointers in their traditional in-memory representations.

The Modern Data base library is written in C.  Not because it’s more suitable for the task than other languages would be - it’s not.  Rather, functional languages tend to have runtime systems (largely consisting of garbage collectors) which make the scenario of deploying a library to be linked against from other languages more difficult.  Obviously there will be bindings written in functional languages, as functional programming is the paradigm best equipped to take advantage of Modern Data’s features, but the library will always be in a system language, so that it can easily be linked against from as many other languages as possible.  Care has been taken to play nicely with foreign-function interfaces, which cannot always use the full capabilities of the C ABI.

The library presents two basic ways of interacting with data.  The node-based object model is analogous to the Document Object Model (DOM) for HTML and XML processing, and is simpler to invoke.  The streaming model is analogous to SAX for XML processing, and is more complicated but dramatically more efficient in some situations.  If you are not sure which to use, you should use the node-based object model.

The streaming model’s primary purpose is to allow you to substitute your own data structures in place of the ones the library provides.  The streaming model requires more work of you, because validation is the client code’s responsibility rather than the library’s.  This lack of validation is a feature; it enables the efficiency gains, and also is the mechanism for constructing novel file formats and network protocols which use Modern Data in some places but have their own “skeletons”.

Please, come on in - and enjoy the type system!