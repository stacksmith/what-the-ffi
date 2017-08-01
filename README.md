This is the stub README.txt for the "test" project.

## Workflow

### Project creation
The project struct makes an ffi generation task repeatable by keeping track of important parameters such as directories, selection criteria, conversion and renaming regexes etc.

### c2ffi spec generation

See the documentation for c2ffi and autowrap.  Generally speaking, we want a complete description of all types we are interested in, all the way down to the architectural description of integers etc.  Start with a file that includes all the headers you need for the library, and add whatever is missing.  You may have to do some header grepping to find the missing files.

### Intake

During intake we generate a Lisp-readable .sexp version of the JSON project file.  Do not run c2ffi in sexp mode: JSON mode retains a lot more information.  We shall convert JSON files and convert them to .sexp ourselves.

`(spec)` will reload the c2ffi JSON specfile and generate a .sexp file

`(reload)` will load the (existing) .sexp file should you restart a session.

`(parse)` will read the sexp file and sort identifiers into `*names*` and `*tags*`
### Select

By design, the spec file contains way too much nformation for our ffi.  Here we select what we need by creating regexs that exclude or include identifiers based on their file location or actual string and placing results into *selected*.  

Note that only functions are selected.  The required data types will be pulled in later.

Iterative fine tuning often requires excluding large groups of symbols, and re-including only the ones needed.  Understanding of regular expressions helps, but brute-force approaches will work.

### Pull

Once the function list is complete, we pull in the required data types.  The system will automatically pull in all dependencies into *pulled* hashtable.

### Reason

At this point we can reason about the FFIs that will be created.  

`(what-files xxx)` will tell you what C header files are involved in generating the xxx symbol.  XXX may be *pulled* or *selected*, in which case all the relevant files will be listed.

`(what-defines fname)` will scan a file for promising #defines which must be added manually.  fname may also be a list of filenames, such as `(what-files *pulled*)`

### Rename

This phase converts C-style identifier names to Lisp symbols of your liking.


### Notes

C interfaces use two namespaces: tags and names.  As a shortcut, they are interchangable here; assuming the headers are written withing C limitations, we can play fast and loose.

Anonymous structs, enums and unions wind up with a numeric tag, hopefully resolved with a typedef to a name.
