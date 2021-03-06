# WHAT-THE-FFI aka WTF

## Summary

This is a tool for importing c2ffi specs, examining and reasoning about FFIs that can be generated from it, renaming and fine-tuning them, finally outputting a FFI tuned to your specification.

## Target Audience

Common Lisp users who wish to create FFI bindings to native libraries, develop new FFI code, or just experiment with foreign libraries and interfaces to them.

## Requirements

c2ffi to generate the initial 'spec' file.

## Rationale

There are several tools for generating FFIs (swig, verrazano and autowrap come to mind).  There are few if any tools for examining and reasoning about the information generated by the initial intake from the header files.  I find the process cumbersome, requiring much grepping and searching of original files, the generated 'specfiles' and finall FFIs to detect issues with conversion or naming.

## Workflow

The starting point is a header file with includes of the library headers (similar to what you may include for a demo project in C).  c2ffi will then create a large JSON 'spec' file containing a 'view' of all relevant headers and types for our project.  

We then import the JSON spec into our WTF project to work on the CFFI.  WTF builds a type database of all known types (saved as a .sexp file for later).  

We can now query the database about the types available, create lists of types we are interested in, work on renaming strategies, optimizations, and generation of FFIS.

## STATUS:

05-Aug-2017  Parsing of spec files fairly complete, database built.
