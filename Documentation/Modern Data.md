Title: Modern Data
Author: Irene Knapp
Base Header Level: 1

# Introduction #

Modern Data is a compact, binary, dependently-typed, self-describing data format for object graphs, and also a library.

A self-describing data format is a format like XML, JSON, or Common Lisp’s s-expressions, which can be processed at a superficial level without knowledge of the specific type of data it contains.  The typical purpose of such formats is interchange across diverse software tools; for example, many tools for working with databases can produce output or accept input in XML.  They can also be useful intermediate representations for transformations on data structures, as with XSLT.  They can even be used for network protocols, as with XMPP/Jabber.  Modern Data is at least as suitable for these tasks as any existing format.

Unlike the above-mentioned alternatives, Modern Data is exclusively a binary format, not a textual one.  This is because textual formats can be written directly by humans, who can make mistakes, which software tools must then cope with, which can be quite difficult and lead to unexpected behavior and, when processing is done inconsistently by different implementations, even to security holes.  A binary format can be written only by the use of assistive software which understands the format, so it is not subject to this problem.

Although not the primary motivation for the choice, a benefit of being a binary format is a certain compactness.  Care has been taken to ensure that the format can in principle be used by database engines and the like as a native format - perhaps slightly larger than non-self-describing forms, but not enormously so as a textual format would be, and without needing a parse phase to perform simple actions such as scanning through it.  Specifically, the format does not use offset-based pointers, so it is suitable for embedding in other formats that do their own space allocation.

It would be difficult to discuss the specifics of Modern Data with no notation for it at all; therefore, there are in fact textual notations for it.  But parsers for these notations are not part of the library - only generators.  This should hopefully remove the temptation to use them for any purpose but documentation and standardization.

The above benefits are substantial, but the most distinctive attribute of Modern Data compared to existing formats is that it is dependently typed.  In a formal sense, this means that types and values are intermixable.  Thus the schema for a Modern Data document is itself potentially a Modern Data document, using a type as a value.  Furthermore, bounds-checked arrays, dimensioned quantities, and the like can all be expressed through values used within types, in ways that will be familiar to programmers who have used theorem-proving languages such as Agda, Coq, Epigram, and Idris.

Another benefit that is a prerequisite of dependent typing but will not be obvious to programmers who are unfamiliar with Hindley-Milner-style type systems is that types are parametric; this means, for example, that it is possible to write the incomplete type “balanced binary tree containing leaf nodes of some particular type”, without specifying the content type at the time that the tree type is defined; the type parameter can be filled in to instantiate that tree for any desired content type, thus obtaining a complete type, which can actually be instantiated.  This will be familiar to Haskell and ML programmers; it is also reminiscent of C++ and Java templates, but the meaning of it has been more rigorously formalized.

Furthermore, code can be written using the library which operates on a parametric type such as our binary tree example, generically to its specific parameters - thus code can be written which rebalances a tree after an update, without knowing the type of the contents.  The library is written such that these operations can be efficiently performed without the need to convert Modern Data to a separate in-memory representation.

Modern Data, unlike other self-describing formats, is capable of representing an arbitrary object graph - not merely one that has obvious linear structure.  This is accomplished without pointers or globally-scoped identifiers of any kind, and therefore does not impede use embedded within other formats, as mentioned above.  The benefit is that arbitrary data structures can be represented directly, including ones which would have cyclic pointers in their traditional in-memory representations.

The Modern Data base library is written in C.  Not because it’s more suitable for the task than other languages would be - it’s not.  Rather, functional languages tend to have runtime systems (largely consisting of garbage collectors) which make the scenario of deploying a library to be linked against from other languages more difficult.  Obviously there will be bindings written in functional languages, as functional programming is the paradigm best equipped to take advantage of Modern Data’s features, but the library will always be in a system language, so that it can easily be linked against from as many other languages as possible.  Care has been taken to play nicely with foreign-function interfaces, which cannot always use the full capabilities of the C ABI.

Please, come on in - and enjoy the type system!

# Semantics #

In Modern Data, the fundamental in-memory datatype is the “modern node”, which is a representation of, loosely, either a value or a type.  If we wish to specify further, we may occasionally refer to a “modern type” or “modern value”, but these concepts do not map to actual host-language datatypes; only “modern node” does.

The operations on a modern node are serialization and deserialization.  Serialization is performed on a stream of modern nodes in some “modern context”, and produces a stream of bytes.  Deserialization is performed on a stream of bytes in some modern context, and produces a stream of modern nodes.

Although simple applications which merely wish to exchange and act on data need never use anything but the default context, it is nonetheless a concept worth understanding.  The context is internally a hash table of modern types (and occasionally modern values, used as parts of dependent types).  The hash key is determined in a way that captures the notion of type equality, by placing the modern node in a canonical form and computing a hash function of that.  Any modern node which is in the hash table is “in scope” in that context.

## Caveats ##

Importantly, a modern type must be in scope in the given context before a value of that type can be serialized or deserialized. The high-level “node-based” portion of the library will check whether a value’s type is in context when serializing, and emit additional instructions which bring it into context if it is not, mutating the context object which it is passed to make note of the fact.  Similarly, when deserializing, if a value’s type is not in context, an error is produced and processing halts.  (To avoid security holes caused by different implementations responding to errors in different ways, processing is specified to always halt immediately upon the first error encountered.)

The low-level “stream-based” portion of the library does no such checking, as it tracks very little internal state, leaving that task to the client code.  The client code is thus responsible for ensuring that types are in context before they are used, both when serializing and when deserializing.

Cyclic structures, both at the type level and at the value level, can be created with inductive families and recursive lets - these two mechanisms are similar to each other, with the one being for types and the other for values.  The details of how this is done are left to a later chapter.  It is enough to know that the details of how these mechanisms are used do in fact become part of a modern node’s identity.  That is, they affect how types are canonicalized, which means that you have a different type if you insert the inductive family into the structure at a different place.  Canonicalization does not apply to values (except for values which are part of types), so the fact that the details of a recursive let also affect the node’s identity is not important in most situations.

As with the scope issue, the node-based portion of the library hides the issue of flattening cyclic structures from the client code, while the stream-based portion exposes it and places the burden on the client.  However, this hiding cannot be complete, so it is important to be aware of its semantics.

Cycles will be automatically broken by introducing the appropriate constructs at the time that they need to be serialized, if they have not already been; cycle-breaking can also be introduced manually.  Thus users can build up cyclic structures through mutation and expect serialization and deserialization of them to “just work”, or they can use inductive families and recursive lets directly, and that will also work.

When inspecting a node-based structure in memory, there are two versions of each library call which returns a related node.  The version for general use processes indirect references transparently, following them repeatedly until arriving at a node which is not an indirection.  The version for special-purpose use does no such processing, instead returning the indirection node itself.  This is analogous to the way that Unix-like operating systems handle symbolic links in their filesystems.

It’s worth noting in passing that it is bad style but technically possible in some situations to use an inductive family in defining values, and to use a recursive let in defining types.  This is not actually a necessary consequence of the ability to use types and values interchangeably in other situations, but it is certainly consistent with that design and makes more sense in that light.

Although many functional languages are additionally "purely functional", meaning that their variables are immutable, nodes can be mutated in memory in some languages.  This is to make it easier to create cyclic structures, which could not otherwise be done in languages without recursive binding.  The semantics of mutation are that all references to the old node become references to the new node.  Mutation is possible on all nodes, with the provision that nodes being modified have never been in scope in any context.  In other words, once a node has become part of a schema of types and and thus potentially has extant nodes which are values of it, it can no longer be modified.  This is, of course, to avoid the scenario of a value node being inconsistent in structure with the type node that corresponds to it.

Of course, it is expected that sets of types for some particular purpose will be constructed in most situations not through imperative code but through the use of an interactive editor having Modern Data as its native format.  The ease with which what would be called a schema in other systems can be serialized and deserialized obviates the need to construct one by hand, most of the time.  As such, the details discussed above should seldom be an issue.

# Scrawlings #

For C, everything is effectively like Data.Dynamic.  For Haskell there will be a GADT and an existential.

Node types “lambda”, “apply”, “index” (as in De Bruijn), and “family” (as in inductive type family).

Things like void (*modern_stream_type_definition_int8)(void *processor, void *context) are callbacks invoked when type IDs are read.  All the functions in that struct are callbacks that are invoked on things that happen during streaming parsing.  The API for streaming generation is the same - except the client code is the consumer of the struct and the caller of these functions, rather than the provider of them.  Note that using the streaming API it is possible to construct invalid output; if this is a concern the object-model API should be used instead.

To make things with cycles, what happens behind the scenes is…  Well, it’s not behind the scenes when you’re doing streaming mode, but in object-model mode, it uses lambdas and applies.  The “prelude” has the S and K combinators already defined, which isn’t strictly necessary because you could define them yourself, but it’s a convenience.

The Lisp-like syntax is the debug format, for use in, say, standards documents that describe APIs in which the units of data interchange are values in Modern Data.

So a point I’m not totally sure on is that I think I just need one version of lambda and one version of apply - as opposed to one of each for values and another of each for types.

Recursive types, such as the simple tree type (in Haskell notation) data Tree = Internal (Tree a) (Tree a) | Leaf a, are defined as cyclic structures.  This avoids the need to refer to anything by name, ever, although the system does in fact support names.

This is on the type level but it’s dependent, so the machinery is the same for types as for values.

I am unclear on whether to implement the set hierarchy, or type-in-type.  I know that I want to call it “universe”, because “set” is a poor name suggestive of set theory rather than type theory.  If I do use the set hierarchy, I expect to have a finite limit, probably 64 bits, on how high one can go.  This is not really any more of a wart than a limit placed on how large an array can be.

With both lambdas and applies present, in principle full computation is possible.

Some parts of this are simply matters of good C practice than of dependent types.  For example, the use of vfile structures for when you want to read a Modern Data value that’s in a zipfile.

You can basically use Modern Data for anything you presently do with XML or HTML or JSON or whatnot, except you can also communicate types across it.  So schemas are built in.  XML schema description is a bugbear that they’ve had a lot of failed attempts at.  You can “teach” the other end how to at least process your thing, even if it doesn’t know your types in advance, by sending a type-as-data across the wire.  You can write helper utilities that only understand some of the types and not the rest, to make “map” and other higher-order functions.

The original motivator was because I hate using text as the format for shell commands.

There should be all sorts of applications.  I think it applies to the functional-database problem, too, even.

In principle you could write webpages in it.

You could implement a programming “language” in it.  The source code would be simply a data structure, instead of being text.  If you like text, you can treat the textual presentation as simply a formatting layer.

Once the basic library is working I’m going to make a generic graphical editor for this, for the Mac.

You can use it for network protocols, too; XMPP/Jabber is similar but using XML as the self-describing data format.

The type system exposes a lot of very low-level types.  I intend to build more higher-level types on top, but the goal at this stage is to provide all the primitives.  After that, I can play around and stuff.  I don’t plan for the higher-level types to be part of the “standard”, because that wouldn’t buy anything - they don’t need to be agreed upon in advance to use them, since the schema is expressible in the system itself.

For aggregate types, I originally broke it down to array (one-dimensional, nothing fancy with the indices; anything more complicated can be built on top); union, which assigns a name to each field it may contain, and exactly one of those fields is present in an actual value; structure, which also assigns a name to each field, but all the fields are present.  Structures, notably, did not have field orders; that is, two structure types with the same field names and types were equivalent regardless of the order the fields appeared in.  And the last aggregate type is “named”.  Notice that in array, union, and structure, the aggregate doesn’t have its own name, which makes anonymous ones possible.  The capability is separated out.

For text the type is UTF-8.  There are no types for other character sets or encodings, because that’s the right one 99% of the time.

There’s blob, too, so if you need to embed non-Modern data in some known format, you can just make a blob out of it.

However, the type system has been simplified now - instead of array, union, and structure, there’s just sigma.

# Canonical form #

Modern Data contexts are essentially hash tables containing modern nodes as their elements.  The hash key used for indexing them is based on the “canonical form” of the modern node.  This section discusses what that canonical form means and how it is computed.  Though client code need never be aware of what are essentially implementation details such as these, independent implementations of the library do need to know them.

It is important to note that each node, independently, has its own canonical form.  It is not meaningful to speak of the canonical form of a document as a whole.

The canonical form is computed in the following steps, performed sequentially:

1) Compute the flattened representation of the document as viewed from the node being considered, as described below.

2) Do a pre-order traversal of the reachable parts of the document starting from the node being considered, outputting modern-stream events constructing each traversed node in turn to an “imagined” accumulator of bytes.

3) Compute the hash of all the bytes that were output to the accumulator.

For ease of implementation and on the general principle of consistency, the format of the accumulator is the same as the format of a modern stream.  However, to compute the accumulator, it is not sufficient simply to output a document and record what has been output; the flattening stage must be done first, to break cycles.  Furthermore, because the exact sequence of bytes changes with the exact sequence of events generated, the events must come from a pre-order traversal as described above.  The flattening stage will have ensured that the pre-order traversal only needs to output value and type events, as well as a single family event at the top-level.  The pre-order traversal will not output any lambda or apply events, nor will it output family events other than the single top-level one.

## Flattening ##

----

# Primitive values #

## Integers ##

## Natural numbers ##

## Floating-point numbers ##

The floating-point datatypes used by Modern Data are a subset of the “interchange formats” defined by IEEE 754-2008.

### 80-bit and 128-bit values ###

It is worth noting that, while many platforms support 128-bit float values, many others do not.  Even when the compiler does, often the system library does not.  This is, not least, because there are no standardized names for the functions - though GNU libquadmath uses the “q” suffix on the ISO C99 base names.  The C type which would be the most likely candidate to be a 128-bit float, long double, is actually an 80-bit float on most Intel-related platforms, while non-Intel platforms seldom support anything longer than 64-bit floats.

This creates a situation where there is no one agreed-upon format for very large floating-point numbers, and it is a significant portability burden to write code which uses either, let alone both.  Modern Data’s purpose is to be an interchange format and lingua franca; therefore, it leaves out support for these types entirely.  It is expected that, should a standard emerge, Modern Data will be duly extended in some future revision.

This decision was not made casually; in fact, an early version of the design featured a 128-bit float.  The only way to reconcile this design decision with the portability constraints Modern Data aspires to was to use the largest available floating-point type on any given platform for all function parameters and return values, while converting these to and from 128-bit representations for interchange.  This was actually implemented at one point, but was removed for the above reasons.  A type which cannot be used is - axiomatically! - of no use to anyone.


## Text ##

## Binary large objects ##

----

# C library reference #

## Library instances ##

### modern_library_initialize ###

modern_library *
modern_library_initalize
(struct modern_error_handler *error_handler,
 struct modern_allocator *allocator,
 void (*finalizer)(void *client_state),
 void *client_state);

### modern_library_get_error_handler ###

struct modern_error_handler *
modern_library_get_error_handler
(modern_library *library);

### modern_library_get_allocator ###

struct modern_allocator *
modern_library_get_allocator
(modern_library *library);

### modern_library_get_client_state ###

void *
modern_library_get_client_state
(modern_library *library);

### modern_library_finalize ###

void
modern_library_finalize
(modern_library *library);


----

## Error handlers ##

struct modern_error_handler {
    …
};

### modern_error_handler_memory ###

void
(*modern_error_handler_memory)
(void *client_state,
 size_t requested_size);

### modern_error_handler_retain_count_overflow ###

void
(*modern_error_handler_retain_count_overflow)
(void *client_state,
 void *retainable);

### modern_error_handler_retain_count_underflow ###

void
(*modern_error_handler_retain_count_underflow)
(void *client_state,
 void *retainable);

### modern_error_handler_double_autorelease ###

void
(*modern_error_handler_double_autorelease)
(void *client_state,
 void *retainable);

### modern_error_handler_type_mismatch ###

void
(*modern_error_handler_type_mismatch)
(void *client_state,
 modern *expected,
 modern *actual);

### modern_error_handler_universe_level_overflow ###

void
(*modern_error_handler_universe_level_overflow)
(void *client_state);

### modern_error_handler_buffer_index ###

void
(*modern_error_handler_buffer_index)
(void *client_state);

### modern_error_handler_not_applicable ###

void
(*modern_error_handler_not_applicable)
(void *client_state);

### modern_error_handler_non_numeric_float ###

void
(*modern_error_handler_non_numeric_float)
(void *client_state);

### modern_error_handler_immutable ###

void
(*modern_error_handler_immutable)
(void *client_state,
 modern *node);

----

## Allocators ##

struct modern_allocator {
    …
};

### modern_allocator_alloc ###

void *
(*modern_allocator_alloc)
(void *client_state,
 size_t size);

### modern_allocator_free ###

void
(*modern_allocator_free)
(void *client_state,
 void *memory);

### modern_allocator_realloc ###

void *
(*modern_allocator_realloc)
(void *client_state,
 void *memory, size_t size);

----

## Memory management ##

### Modern autorelease pools ###

typedef void modern_autorelease_pool;

### modern_make_autorelease_pool ###

modern_autorelease_pool *
modern_make_autorelease_pool
(modern_library *library);

### modern_autorelease_pool_release ###

void
modern_autorelease_pool_release
(modern_library *library,
 modern_autorelease_pool *pool);

### modern_retain ###

void
modern_retain
(modern_library *library,
 void *retainable);

### modern_release ###

void
modern_release
(modern_library *library,
 void *retainable);

### modern_autorelease ###

void
modern_autorelease
(modern_library *library,
 modern_autorelease_pool *pool,
 void *retainable);

----

## Virtual files ##

struct modern_vfile {
    …
};

### modern_vfile_read ###

ssize_t
(*modern_vfile_read)
(void *vfile_state,
 uint8_t *buffer,
 size_t length);

### modern_vfile_write ###

ssize_t
(*modern_vfile_write)
(void *vfile_state,
 uint8_t *buffer,
 size_t length);

----

## Numeric values ##

typedef float modern_float32;
typedef double modern_float64;
typedef long double modern_float128;

----

## Hashing ##

### Hashes ###

struct modern_hash {
    uint64_t a;
    uint64_t b;
};

### modern_compute_hash ###

void
modern_compute_hash
(uint8_t *data,
 size_t length,
 struct modern_hash *out);

### modern_compute_child_hash ###

void
modern_compute_child_hash
(struct modern_hash *parent,
 uint8_t *data,
 size_t length,
 struct modern_hash *out);

----

## Evaluation ##

### modern_evaluate ###

modern *
modern_evaluate
(modern_library *library,
 modern *node);

----

## Node-based object model ##

### The "modern" type ###

typedef void modern;

The “modern” type is a polymorphic node for object-oriented access to both types and values.  Because it is not limited to values, it is not called “modern_data”; it is simply called “modern” and the library user is encouraged to think of it as its own thing.

Note that this is true in all the versions of the library for non-functional languages, but in languages which support generalized algebraic datatypes, the situation is more complicated; see for example the Haskell version.

----

### Node Types ###

enum modern_node_type {
    …
};

#### int8_value_modern_node_type ####

int8_value_modern_node_type = 1,

#### int16_value_modern_node_type ####

int16_value_modern_node_type = 2,

#### int32_value_modern_node_type ####

int32_value_modern_node_type = 3,

#### int64_value_modern_node_type ####

int64_value_modern_node_type = 4,

#### nat8_value_modern_node_type ####

nat8_value_modern_node_type = 5,

#### nat16_value_modern_node_type ####

nat16_value_modern_node_type = 6,

#### nat32_value_modern_node_type ####

nat32_value_modern_node_type = 7,

#### nat64_value_modern_node_type ####

nat64_value_modern_node_type = 8,

#### float32_value_modern_node_type ####

float32_value_modern_node_type = 9,

#### float64_value_modern_node_type ####

float64_value_modern_node_type = 10,

#### utf8_value_modern_node_type ####

utf8_value_modern_node_type = 11,

#### blob_value_modern_node_type ####

blob_value_modern_node_type = 12,

#### sigma_value_modern_node_type ####

sigma_value_modern_node_type = 13,

#### named_value_modern_node_type ####

named_value_modern_node_type = 14,

#### int8_type_modern_node_type ####

int8_type_modern_node_type = 15,

#### int16_type_modern_node_type ####

int16_type_modern_node_type = 16,

#### int32_type_modern_node_type ####

int32_type_modern_node_type = 17,

#### int64_type_modern_node_type ####

int64_type_modern_node_type = 18,

#### nat8_type_modern_node_type ####

nat8_type_modern_node_type = 19,

#### nat16_type_modern_node_type ####

nat16_type_modern_node_type = 20,

#### nat32_type_modern_node_type ####

nat32_type_modern_node_type = 21,

#### nat64_type_modern_node_type ####

nat64_type_modern_node_type = 22,

#### float32_type_modern_node_type ####

float32_type_modern_node_type = 23,

#### float64_type_modern_node_type ####

float64_type_modern_node_type = 24,

#### utf8_type_modern_node_type ####

utf8_type_modern_node_type = 25,

#### blob_type_modern_node_type ####

blob_type_modern_node_type = 26,

#### function_type_modern_node_type ####

blob_type_modern_node_type = 27,

#### sigma_type_modern_node_type ####

sigma_type_modern_node_type = 28,

#### named_type_modern_node_type ####

named_type_modern_node_type = 29,

#### universe_type_modern_node_type ####

universe_type_modern_node_type = 30,

#### lambda_modern_node_type ####

lambda_modern_node_type = 31,

#### apply_modern_node_type ####

apply_modern_node_type = 32,

#### type_family_modern_node_type ####

type_family_modern_node_type = 33,

#### let_modern_node_type ####

let_modern_node_type = 34,

#### backreference_modern_node_type ####

backreference_modern_node_type = 35,

#### builtin_modern_node_type ####

builtin_modern_node_type = 36,

----

### Builtin Identifiers ###

enum modern_builtin_identifier {
    …
};

#### int8_value_modern_node_type ####

plus_int8_modern_builtin_identifier = 1,

----

### Reading ###

#### General ####

##### modern_node_get_node_type #####

enum modern_node_type
modern_node_get_node_type
(modern_library *library,
 modern *value);

##### modern_node_get_value_type #####

modern *
modern_node_get_value_type
(modern_library *library,
 modern *value);

##### modern_node_get_mutable #####

int
modern_node_get_mutable
(modern_library *library,
 modern *value);

----

#### Values ####

##### modern_node_get_int8 #####

int8_t
modern_node_get_int8
(modern_library *library,
 modern *value);

##### modern_node_get_int16 #####

int16_t
modern_node_get_int16
(modern_library *library,
 modern *value);

##### modern_node_get_int32 #####

int32_t
modern_node_get_int32
(modern_library *library,
 modern *value);

##### modern_node_get_int64 #####

int64_t
modern_node_get_int64
(modern_library *library,
 modern *value);

##### modern_node_get_nat8 #####

uint8_t
modern_node_get_nat8
(modern_library *library,
 modern *value);

##### modern_node_get_nat16 #####

uint16_t
modern_node_get_nat16
(modern_library *library,
 modern *value);

##### modern_node_get_nat32 #####

uint32_t
modern_node_get_nat32
(modern_library *library,
 modern *value);

##### modern_node_get_nat64 #####

uint64_t
modern_node_get_nat64
(modern_library *library,
 modern *value);

##### modern_node_get_float32 #####

float
modern_node_get_float32
(modern_library *library,
 modern *value);

##### modern_node_get_float64 #####

double
modern_node_get_float64
(modern_library *library,
 modern *value);

##### modern_node_get_utf8_bytes #####

size_t
modern_node_get_utf8_bytes
(modern_library *library,
 modern *value);

##### modern_node_get_utf8_data_piece #####

uint8_t *
modern_node_get_utf8_data_piece
(modern_library *library,
 modern *value,
 size_t offset,
 size_t bytes);

##### modern_node_get_blob_bytes #####

size_t
modern_node_get_blob_bytes
(modern_library *library,
 modern *value);

##### modern_node_get_blob_data_piece #####

uint8_t *
modern_node_get_blob_data_piece
(modern_library *library,
 modern *value,
 size_t offset,
 size_t bytes);

##### modern_node_get_sigma_field_value #####

modern *
modern_node_get_sigma_field_value
(modern_library *library,
 modern *value);

##### modern_node_get_sigma_successor #####

modern *
modern_node_get_sigma_successor
(modern_library *library,
 modern *value);

##### modern_node_get_named_value #####

modern *
modern_node_get_named_value
(modern_library *library,
 modern *value);

----

#### Types ####

##### modern_node_get_function_type_left #####

modern *
modern_node_get_function_type_left
(modern_library *library,
 modern *value);

##### modern_node_get_function_type_right #####

modern *
modern_node_get_function_type_right
(modern_library *library,
 modern *value);

##### modern_node_get_sigma_type_field_type #####

modern *
modern_node_get_sigma_type_field_type
(modern_library *library,
 modern *value);

##### modern_node_get_sigma_type_successor #####

modern *
modern_node_get_sigma_type_successor
(modern_library *library,
 modern *value);

##### modern_node_get_named_type_name #####

struct modern_hash *
modern_node_get_named_type_name
(modern_library *library,
 modern *value);

##### modern_node_get_named_type_content_type #####

modern *
modern_node_get_named_type_content_type
(modern_library *library,
 modern *value);

##### modern_node_get_universe_type_level #####

uint64_t
modern_node_get_universe_type_level
(modern_library *library,
 modern *value);

----

#### Combinators ####

##### modern_node_get_lambda_content #####

modern *
modern_node_get_lambda_content
(modern_library *library,
 modern *value);

##### modern_node_get_apply_left #####

modern *
modern_node_get_apply_left
(modern_library *library,
 modern *value);

##### modern_node_get_apply_right #####

modern *
modern_node_get_apply_right
(modern_library *library,
 modern *value);

##### modern_node_get_type_family_count #####

uint64_t
modern_node_get_type_family_count
(modern_library *library,
 modern *value);

##### modern_node_get_type_family_item #####

modern *
modern_node_get_type_family_item
(modern_library *library,
 modern *value,
 uint64_t index);

##### modern_node_get_let_count #####

uint64_t
modern_node_get_let_count
(modern_library *library,
 modern *value);

##### modern_node_get_let_item #####

modern *
modern_node_get_let_item
(modern_library *library,
 modern *value,
 uint64_t index);

##### modern_node_get_let_content #####

modern *
modern_node_get_let_content
(modern_library *library,
 modern *value);

##### modern_node_get_builtin_identifier #####

uint16_t
modern_node_get_builtin_identifier
(modern_library *library,
 modern *value);

----

### Construction ###

#### Values ####

##### modern_node_make_int8 #####

modern *
modern_node_make_int8
(modern_library *library,
 int8_t value);

##### modern_node_make_int16 #####

modern *
modern_node_make_int16
(modern_library *library,
 int16_t value);

##### modern_node_make_int32 #####

modern *
modern_node_make_int32
(modern_library *library,
 int32_t value);

##### modern_node_make_int64 #####

modern *
modern_node_make_int64
(modern_library *library,
 int64_t value);

##### modern_node_make_nat8 #####

modern *
modern_node_make_nat8
(modern_library *library,
 uint8_t value);

##### modern_node_make_nat16 #####

modern *
modern_node_make_nat16
(modern_library *library,
 uint16_t value);

##### modern_node_make_nat32 #####

modern *
modern_node_make_nat32
(modern_library *library,
 uint32_t value);

##### modern_node_make_nat64 #####

modern *
modern_node_make_nat64
(modern_library *library,
 uint64_t value);

##### modern_node_make_float32 #####

modern *
modern_node_make_float32
(modern_library *library,
 float value);

##### modern_node_make_float64 #####

modern *
modern_node_make_float64
(modern_library *library,
 double value);

##### modern_node_make_utf8 #####

modern *
modern_node_make_utf8
(modern_library *library,
 uint8_t *data);

##### modern_node_make_blob #####

modern *
modern_node_make_blob
(modern_library *library,
 uint8_t *data,
 size_t bytes);

##### modern_node_make_sigma #####

modern *
modern_node_make_sigma
(modern_library *library,
 modern *type,
 modern *field_value,
 modern *successor_value);

##### modern_node_make_named_value #####

modern *
modern_node_make_named_value
(modern_library *library,
 modern *type,
 modern *value);

----

#### Types ####

##### modern_node_make_int8_type #####

modern *
modern_node_make_int8_type
(modern_library *library);

##### modern_node_make_int16_type #####

modern *
modern_node_make_int16_type
(modern_library *library);

##### modern_node_make_int32_type #####

modern *
modern_node_make_int32_type
(modern_library *library);

##### modern_node_make_int64_type #####

modern *
modern_node_make_int64_type
(modern_library *library);

##### modern_node_make_nat8_type #####

modern *
modern_node_make_nat8_type
(modern_library *library);

##### modern_node_make_nat16_type #####

modern *
modern_node_make_nat16_type
(modern_library *library);

##### modern_node_make_nat32_type #####

modern *
modern_node_make_nat32_type
(modern_library *library);

##### modern_node_make_nat64_type #####

modern *
modern_node_make_nat64_type
(modern_library *library);

##### modern_node_make_float32_type #####

modern *
modern_node_make_float32_type
(modern_library *library);

##### modern_node_make_float64_type #####

modern *
modern_node_make_float64_type
(modern_library *library);

##### modern_node_make_utf8_type #####

modern *
modern_node_make_utf8_type
(modern_library *library);

##### modern_node_make_blob_type #####

modern *
modern_node_make_blob_type
(modern_library *library);

##### modern_node_make_function_type #####

modern *
modern_node_make_function_type
(modern_library *library,
 modern *left,
 modern *right);

##### modern_node_make_sigma_type #####

modern *
modern_node_make_sigma_type
(modern_library *library,
 modern *field_type, modern *successor);

##### modern_node_make_named_type #####

modern *
modern_node_make_named_type
(modern_library *library,
 struct modern_hash *name,
 modern *content_type);

##### modern_node_make_universe_type #####

modern *
modern_node_make_universe_type
(modern_library *library,
 uint64_t level);

----

#### Combinators ####

##### modern_node_make_lambda #####

modern *
modern_node_make_lambda
(modern_library *library,
 modern *content);

##### modern_node_make_apply #####

modern *
modern_node_make_apply
(modern_library *library,
 modern *left,
 modern *right);

##### modern_node_make_type_family #####

modern *
modern_node_make_type_family
(modern_library *library,
 uint64_t n_items,
 modern **types);

##### modern_node_make_let #####

modern *
modern_node_make_let
(modern_library *library,
 uint64_t n_items,
 modern **types,
 modern *content);

##### modern_node_make_backreference #####

modern *
modern_node_make_backreference
(modern_library *library,
 uint64_t index);

##### modern_node_make_builtin #####

modern *
modern_node_make_builtin
(modern_library *library,
 uint16_t identifier);

----

### Mutation ###

#### General ####

##### modern_node_set_immutable #####

void
modern_node_set_immutable
(modern_library *library,
 modern *value);

----

#### Values ####

##### modern_node_set_int8 #####

void
modern_node_set_int8
(modern_library *library,
 modern *node,
 int8_t value);

##### modern_node_set_int16 #####

void
modern_node_set_int16
(modern_library *library,
 modern *node,
 int16_t value);

##### modern_node_set_int32 #####

void
modern_node_set_int32
(modern_library *library,
 modern *node,
 int32_t value);

##### modern_node_set_int64 #####

void
modern_node_set_int64
(modern_library *library,
 modern *node,
 int64_t value);

##### modern_node_set_nat8 #####

void
modern_node_set_nat8
(modern_library *library,
 modern *node,
 uint8_t value);

##### modern_node_set_nat16 #####

void
modern_node_set_nat16
(modern_library *library,
 modern *node,
 uint16_t value);

##### modern_node_set_nat32 #####

void
modern_node_set_nat32
(modern_library *library,
 modern *node,
 uint32_t value);

##### modern_node_set_nat64 #####

void
modern_node_set_nat64
(modern_library *library,
 modern *node,
 uint64_t value);

##### modern_node_set_float32 #####

void
modern_node_set_float32
(modern_library *library,
 modern *node,
 float value);

##### modern_node_set_float64 #####

void
modern_node_set_float64
(modern_library *library,
 modern *node,
 double value);

##### modern_node_set_utf8_data_piece #####

void
modern_node_set_utf8_data_piece
(modern_library *library,
 modern *value,
 uint8_t *data,
 size_t offset,
 size_t old_bytes,
 size_t new_bytes);

##### modern_node_set_blob_data_piece #####

void
modern_node_set_blob_data_piece
(modern_library *library,
 modern *value,
 uint8_t *data,
 size_t offset,
 size_t old_bytes,
 size_t new_bytes);

##### modern_node_set_sigma_field_value #####

void
modern_node_set_sigma_field_value
(modern_library *library,
 modern *value,
 modern *field_value);

##### modern_node_set_sigma_successor #####

void
modern_node_set_sigma_successor
(modern_library *library,
 modern *value,
 modern *successor);

##### modern_node_set_named_value #####

void
modern_node_set_named_value
(modern_library *library,
 modern *node,
 modern *type,
 modern *value);

----

#### Types ####

##### modern_node_set_function_type_left #####

void
modern_node_set_function_type_left
(modern_library *library,
 modern *value,
 modern *left);

##### modern_node_set_function_type_right #####

void
modern_node_set_function_type_right
(modern_library *library,
 modern *value,
 modern *right);

##### modern_node_set_sigma_type_field_type #####

void
modern_node_set_sigma_type_field_type
(modern_library *library,
 modern *value,
 modern *field_type);

##### modern_node_set_sigma_type_successor #####

void
modern_node_set_sigma_type_successor
(modern_library *library,
 modern *value,
 modern *successor);

##### modern_node_set_named_type_name #####

void
modern_node_set_named_type_name
(modern_library *library,
 modern *value
 struct modern_hash *name);

##### modern_node_set_named_type_content_type #####

void
modern_node_set_named_type_content_type
(modern_library *library,
 modern *value,
 modern *content_type);

##### modern_node_set_universe_type_level #####

void
modern_node_set_universe_type_level
(modern_library *library,
 modern *value,
 uint64_t level);

----

#### Combinators ####

##### modern_node_set_lambda_content #####

void
modern_node_set_lambda_content
(modern_library *library,
 modern *value,
 modern *content);

##### modern_node_set_apply_left #####

void
modern_node_set_apply_left
(modern_library *library,
 modern *value,
 modern *left);

##### modern_node_set_apply_right #####

void
modern_node_set_apply_right
(modern_library *library,
 modern *value,
 modern *right);

##### modern_node_set_type_family_add_item #####

void
modern_node_set_type_family_add_item
(modern_library *library,
 modern *value,
 modern *item,
 uint64_t index);

##### modern_node_set_type_family_remove_item #####

void
modern_node_set_type_family_remove_item
(modern_library *library,
 modern *value,
 uint64_t index);

##### modern_node_set_let_add_item #####

void
modern_node_set_let_add_item
(modern_library *library,
 modern *value,
 modern *item,
 uint64_t index);

##### modern_node_set_let_remove_item #####

void
modern_node_set_let_remove_item
(modern_library *library,
 modern *value,
 uint64_t index);

##### modern_node_set_let_content #####

void
modern_node_set_let_content
(modern_library *library,
 modern *value,
 modern *content);

##### modern_node_set_builtin_identifier #####

void
modern_node_set_builtin_identifier
(modern_library *library,
 modern *value,
 uint16_t identifier);

----

### Canonical form ###

#### modern_node_canonical ####

void
modern_node_canonical_hash
(modern_library *library,
 modern *value,
 struct modern_hash *out);

----

### Contexts ###

#### Modern contexts ####

typedef void modern_context;

#### modern_make_initial_context ####

modern_context *
modern_make_initial_context
(modern_library *library);

#### modern_copy_context ####

modern_context *
modern_copy_context
(modern_library *library,
 modern_context *context);

#### modern_get_in_context ####

int
modern_get_in_context
(modern_library *library,
 modern_context *context,
 modern *node);

#### modern_add_to_context ####

void
modern_add_to_context
(modern_library *library,
 modern_context *context,
 modern *node);

#### modern_get_from_context ####

modern *
modern_get_from_context
(modern_library *library,
 modern_context *context,
 struct modern_hash *hash);

----

## Node-based deserialization ##

### modern_deserialize_memory ###

modern *
modern_deserialize_memory
(modern_library *library,
 modern_autorelease_pool *pool,
 modern_context *context,
 uint8_t *data,
 size_t length);

### modern_deserialize_file ###

modern *
modern_deserialize_file
(modern_library *library,
 modern_autorelease_pool *pool,
 modern_context *context,
 FILE *file);

### modern_deserialize_fd ###

modern *
modern_deserialize_fd
(modern_library *library,
 modern_autorelease_pool *pool,
 modern_context *context,
 int fd);

### modern_deserialize_vfile ###

modern *
modern_deserialize_vfile
(modern_library *library,
 modern_autorelease_pool *pool,
 modern_context *context,
 struct modern_vfile *vfile,
 void *vfile_state);

### moden_deserialize_input_stream ###

modern *
modern_deserialize_input_stream
(modern_library *library,
 modern_autorelease_pool *pool,
 modern_context *context,
 void *processor_state,
 void *stream_state);

----

## Node-based serialization ##

### modern_serialize_memory_buffer ###

void
modern_serialize_memory_buffer
(modern_library *library,
 modern *value,
 modern_context *context,
 uint8_t *buffer,
 size_t *length);

### modern_serialize_memory_allocating ###

uint8_t *
modern_serialize_memory_allocating
(modern_library *library,
 modern *value,
 modern_context *context,
 size_t *length);

### modern_serialize_file ###

void
modern_serialize_file
(modern_library *library,
 modern *value,
 modern_context *context,
 FILE *file);

### modern_serialize_fd ###

void
modern_serialize_fd
(modern_library *library,
 modern *value,
 modern_context *context,
 int fd);

### modern_serialize_vfile ###

void
modern_serialize_vfile
(modern_library *library,
 modern *value,
 modern_context *context,
 struct modern_vfile *vfile,
 void *vfile_state);

### modern_serialize_output_stream ###

modern *
modern_serialize_output_stream
(modern_library *library,
 modern *value,
 modern_context *context,
 struct modern_stream *stream);

----

## Processors ##

struct modern_processor {
    …
};

### pool ###

modern_autorelease_pool *pool;

### modern_processor_abort ###

void
(*modern_processor_abort)
(void *processor_state);

### modern_processor_flush ###

void (*modern_processor_flush)
  (void *processor_state);

----

## Streams ##

struct modern_stream {
    …
};

### modern_stream_initialize ###

void *
(*modern_stream_initialize)
();

### modern_stream_start ###

void
(*modern_stream_start)
(void *processor_state,
 void *stream_state);

### modern_stream_type_magic_number ###

void
(*modern_stream_type_magic_number)
(void *processor_state,
 void *stream_state);

### modern_stream_name_definition ###

void
(*modern_stream_name_definition)
(void *processor_state,
 void *stream_state,
 uint8_t *data,
 size_t length);

### modern_stream_type_definition_int8 ###

void
(*modern_stream_type_definition_int8)
(void *processor_state,
 void *stream_state);

### modern_stream_type_definition_int16 ###

void
(*modern_stream_type_definition_int16)
(void *processor_state,
 void *stream_state);

### modern_stream_type_definition_int32 ###

void
(*modern_stream_type_definition_int32)
(void *processor_state,
 void *stream_state);

### modern_stream_type_definition_int64 ###

void
(*modern_stream_type_definition_int64)
(void *processor_state,
 void *stream_state);

### modern_stream_type_definition_nat8 ###

void
(*modern_stream_type_definition_nat8)
(void *processor_state,
 void *stream_state);

### modern_stream_type_definition_nat16 ###

void
(*modern_stream_type_definition_nat16)
(void *processor_state,
 void *stream_state);

### modern_stream_type_definition_nat32 ###

void
(*modern_stream_type_definition_nat32)
(void *processor_state,
 void *stream_state);

### modern_stream_type_definition_nat64 ###

void
(*modern_stream_type_definition_nat64)
(void *processor_state,
 void *stream_state);

### modern_stream_type_definition_float32 ###

void
(*modern_stream_type_definition_float32)
(void *processor_state,
 void *stream_state);

### modern_stream_type_definition_float64 ###

void
(*modern_stream_type_definition_float64)
(void *processor_state,
 void *stream_state);

### modern_stream_type_definition_utf8 ###

void
(*modern_stream_type_definition_utf8)
(void *processor_state,
 void *stream_state);

### modern_stream_type_definition_blob ###

void
(*modern_stream_type_definition_blob)
(void *processor_state,
 void *stream_state);

### modern_stream_type_definition_function_is_next ###

void
(*modern_stream_type_definition_function_is_next)
(void *processor_state,
 void *stream_state);

### modern_stream_type_definition_sigma_is_next ###

void
(*modern_stream_type_definition_sigma_is_next)
(void *processor_state,
 void *stream_state
 struct modern_hash *a,
 struct modern_hash *b);

### modern_stream_type_definition_named_is_next ###

void
(*modern_stream_type_definition_named_is_next)
(void *processor_state,
 void *stream_state,
 struct modern_hash *name);

### modern_stream_type_definition_universe ###

void
(*modern_stream_type_definition_universe)
(void *processor_state,
 void *stream_state);

### modern_stream_int8 ###

void
(*modern_stream_int8)
(void *processor_state,
 void *stream_state,
 int8_t value);

### modern_stream_int16 ###

void
(*modern_stream_int16)
(void *processor_state,
 void *stream_state,
 int16_t value);

### modern_stream_int32 ###

void
(*modern_stream_int32)
(void *processor_state,
 void *stream_state,
 int32_t value);

### modern_stream_int64 ###

void
(*modern_stream_int64)
(void *processor_state,
 void *stream_state,
 int64_t value);

### modern_stream_nat8 ###

void
(*modern_stream_nat8)
(void *processor_state,
 void *stream_state,
 uint8_t value);

### modern_stream_nat16 ###

void
(*modern_stream_nat16)
(void *processor_state,
 void *stream_state,
 uint16_t value);

### modern_stream_nat32 ###

void
(*modern_stream_nat32)
(void *processor_state,
 void *stream_state,
 uint32_t value);

### modern_stream_nat64 ###

void
(*modern_stream_nat64)
(void *processor_state,
 void *stream_state,
 uint64_t value);

### modern_stream_float32 ###

void
(*modern_stream_float32)
(void *processor_state,
 void *stream_state,
 float value);

### modern_stream_float64 ###

void
(*modern_stream_float64)
(void *processor_state,
 void *stream_state,
 double value);

### modern_stream_utf8_start ###

void
(*modern_stream_utf8_start)
(void *processor_state,
 void *stream_state);

### modern_stream_utf8_data ###

void
(*modern_stream_utf8_data)
(void *processor_state,
 void *stream_state,
 uint8_t *data,
 size_t length);

### modern_stream_utf8_end ###

void
(*modern_stream_utf8_end)
(void *processor_state,
 void *stream_state);

### modern_stream_blob_start ###

void
(*modern_stream_blob_start)
(void *processor_state,
 void *stream_state);

### modern_stream_blob_data ###

void
(*modern_stream_blob_data)
(void *processor_state,
 void *stream_state,
 uint8_t *data,
 size_t length);

### modern_stream_blob_end ###

void
(*modern_stream_blob_end)
(void *processor_state,
 void *stream_state);

### modern_stream_array_start ###

void
(*modern_stream_array_start)
(void *processor_state,
 void *stream_state);

### modern_stream_array_end ###

void
(*modern_stream_array_end)
(void *processor_state,
 void *stream_state);

### modern_stream_union_field ###

void
(*modern_stream_union_field)
(void *processor_state,
 void *stream_state,
 struct modern_hash *type,
 struct modern_hash *field);

### modern_stream_structure_start ###

void
(*modern_stream_structure_start)
(void *processor_state,
 void *stream_state,
 struct modern_hash *type);

### modern_stream_structure_field ###

void
(*modern_stream_structure_field)
(void *processor_state,
 void *stream_state,
 struct modern_hash *field);

### modern_stream_structure_end ###

void
(*modern_stream_structure_end)
(void *processor_state,
 void *stream_state);

### modern_stream_named_value_is_next ###

void
(*modern_stream_named_value_is_next)
(void *processor_state,
 void *stream_state,
 struct modern_hash *name);

### modern_stream_lambda_is_next ###

void
(*modern_stream_lambda_is_next)
(void *processor_state,
 void *stream_state);

### modern_stream_apply_is_next ###

void
(*modern_stream_apply_is_next)
(void *processor_state,
 void *stream_state);

### modern_stream_type_family_is_next ###

void
(*modern_stream_type_family_is_next)
(void *processor_state,
 void *stream_state,
 uint64_t n_items);

### modern_stream_let_is_next ###

void
(*modern_stream_type_let_is_next)
(void *processor_state,
 void *stream_state,
 uint64_t n_items);

### modern_stream_backreference_is_next ###

void
(*modern_stream_backreference_is_next)
(void *processor_state,
 void *stream_state,
 uint64_t index);

### modern_stream_builtin_is_next ###

void
(*modern_stream_builtin_is_next)
(void *processor_state,
 void *stream_state,
 uint16_t identifier);

### modern_stream_type_as_value_is_next ###

void
(*modern_stream_type_as_value_is_next)
(void *processor_state,
 void *stream_state,
 struct modern_hash *type);

### modern_stream_end ###

void
(*modern_stream_end)
(void *processor_state,
 void *stream_state);

### modern_stream_finalize ###

void
(*modern_stream_finalize)
(void *stream_state);

----

## Stream-based deserialization  ##

### General ###

#### modern_input_stream_memory ####

void *
modern_input_stream_memory
(modern_library *library,
 modern_autorelease_pool *pool,
 struct modern_stream *stream,
 uint8_t *data,
 size_t length);

#### modern_input_stream_file ####

void *
modern_input_stream_file
(modern_library *library,
 modern_autorelease_pool *pool,
 struct modern_stream *stream,
 FILE *file);

#### modern_input_stream_fd ####

void *
modern_input_stream_fd
(modern_library *library,
 modern_autorelease_pool *pool,
 struct modern_stream *stream,
 int fd);

#### modern_input_stream_vfile ####

void *
modern_input_stream_vfile
(modern_library *library,
 modern_autorelease_pool *pool,
 struct modern_stream *stream,
 struct modern_vfile *vfile,
 void *vfile_state);

#### modern_input_stream_finalize ####

void
modern_input_stream_finalize
(modern_library *library,
 void *processor_state);

----

### Customization ###

#### modern_input_stream_step ####

void
modern_input_stream_step
(modern_library *library,
 struct modern_stream *stream,
 void *processor_state,
 void **stream_state);

#### modern_input_stream_run ####

void
modern_input_stream_run
(modern_library *library,
 struct modern_stream *stream,
 void *processor_state,
 void **stream_state);

#### modern_input_stream_do_all ####

void
modern_input_stream_do_all
(modern_library *library,
 struct modern_stream *stream,
 void *processor_state);

----

## Stream-based serialization ##

### modern_output_stream_memory_buffer ###

void *
modern_output_stream_memory_buffer
(modern_library *library,
 modern_autorelease_pool *pool,
 uint8_t *buffer,
 size_t *length);

### modern_output_stream_memory_allocating ###

void *
modern_output_stream_memory_allocating
(modern_library *library,
 modern_autorelease_pool *pool,
 size_t *length);

### modern_output_stream_memory_allocating_result ###

uint8_t *
modern_output_stream_memory_allocating_result
(modern_library *library,
 void *stream_state,
 size_t *length)

### modern_output_stream_file ###

void *
modern_output_stream_file
(modern_library *library,
 modern_autorelease_pool *pool,
 FILE *file);

### modern_output_stream_fd ###

void *
modern_output_stream_fd
(modern_library *library,
 modern_autorelease_pool *pool,
 int fd);

### modern_output_stream_vfile ###

void *
modern_output_stream_vfile
(modern_library *library,
 modern_autorelease_pool *pool,
 struct modern_vfile *vfile,
 void *vfile_state);

----

## Predefined hashes ##

### modern_compute_initial_namespace_hash ###

void
modern_compute_initial_namespace_hash
(struct modern_hash *out);

----

## Predefined virtual files ##

### modern_make_memory_buffer_vfile ###

struct modern_vfile *
modern_make_memory_buffer_vfile
(modern_library *library);

### modern_make_memory_allocating_vfile ###

struct modern_vfile *
modern_make_memory_allocating_vfile
(modern_library *library);

### modern_make_file_vfile ###

struct modern_vfile *
modern_make_file_vfile
(modern_library *library);

### modern_make_fd_vfile ###

struct modern_vfile *
modern_make_fd_vfile
(modern_library *library);

----

## Predefined streams ##

### modern_make_explicatory_stream ###

struct modern_stream *
modern_make_explicatory_stream
(modern_library *library);

### modern_make_documentation_stream ###

struct modern_stream *
modern_make_documentation_stream
(modern_library *library);

----

# Normative references #

## IEEE 754-2008 ##